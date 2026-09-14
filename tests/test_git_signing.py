#!/usr/bin/env python3
"""Regression coverage for profile-specific unattended Git SSH signing."""

import os
import subprocess
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
ROOT_AGENTS_MD = ROOT / "AGENTS.md"
MACHINES_NIX = ROOT / "nix/profiles/machines.nix"
GIT_NIX = ROOT / "nix/home-manager/config/apps/git.nix"
ENVIRONMENT_NIX = ROOT / "nix/home-manager/config/environment.nix"
PROGRAMS_NIX = ROOT / "nix/home-manager/config/programs.nix"
XDG_NIX = ROOT / "nix/home-manager/config/xdg.nix"
SIGNING_HELPER = ROOT / "bin/files/codex-signing-key"
AGENTS_MD = ROOT / "config/ai/AGENTS.md"
PUBLISH_SKILL = ROOT / "config/agents/skills/multi-agent-team/signed-pr-publish/SKILL.md"


class GitSigningConfigurationTest(unittest.TestCase):
    def test_profiles_select_distinct_public_signing_keys(self):
        machines = MACHINES_NIX.read_text(encoding="utf-8")

        self.assertIn(
            'git_signing_key = "~/.ssh/codex-signing-office-ed25519.pub";',
            machines,
        )
        self.assertIn(
            'git_signing_key = "~/.ssh/codex-signing-personal-ed25519.pub";',
            machines,
        )

    def test_generated_git_configuration_uses_mandatory_openssh_signing(self):
        git_nix = GIT_NIX.read_text(encoding="utf-8")

        self.assertIn("key = machineConfig.git_signing_key;", git_nix)
        self.assertIn("signByDefault = true;", git_nix)
        self.assertIn('format = "ssh";', git_nix)
        self.assertIn('program = "/usr/bin/ssh-keygen";', git_nix)
        self.assertNotIn("op-ssh-sign", git_nix)

    def test_office_https_publication_leaves_ssh_as_an_explicit_option(self):
        git_nix = GIT_NIX.read_text(encoding="utf-8")
        programs_nix = PROGRAMS_NIX.read_text(encoding="utf-8")

        self.assertNotIn("SSH_AUTH_SOCK =", ENVIRONMENT_NIX.read_text(encoding="utf-8"))
        self.assertIn("IdentityAgent = op1PasswordAgent;", programs_nix)
        self.assertNotIn("github.com-work", git_nix)
        self.assertNotIn("insteadOf", git_nix)
        self.assertNotIn("github.com-work", programs_nix)
        self.assertIn("Explicit SSH remotes may use the 1Password agent", programs_nix)

    def test_helper_is_linked_and_uses_a_24_hour_login_session_agent_key(self):
        helper = SIGNING_HELPER.read_text(encoding="utf-8")
        programs_nix = PROGRAMS_NIX.read_text(encoding="utf-8")

        self.assertIn('"codex-signing-key"', XDG_NIX.read_text(encoding="utf-8"))
        self.assertIn("readonly agent_lifetime=86400", helper)
        self.assertIn('/usr/bin/ssh-add --apple-use-keychain -t "$agent_lifetime" "$private_key"', helper)
        self.assertNotIn("ssh-add -c", helper)
        self.assertIn("-S \"$SSH_AUTH_SOCK\"", helper)
        self.assertIn("*1password*", helper)
        self.assertIn("launchd.agents.codexSigningKeychainLoader", programs_nix)
        self.assertIn('"--apple-load-keychain"', programs_nix)
        self.assertIn('"/usr/bin/ssh-add"', programs_nix)
        self.assertNotIn("ssh-agent -D", programs_nix)

    def test_bootstrap_refuses_existing_key_material_without_invoking_ssh_keygen(self):
        with tempfile.TemporaryDirectory() as temporary_home:
            ssh_dir = Path(temporary_home) / ".ssh"
            ssh_dir.mkdir()
            (ssh_dir / "codex-signing-office-ed25519.pub").write_text("existing\n", encoding="utf-8")

            environment = os.environ | {"HOME": temporary_home}
            result = subprocess.run(
                [str(SIGNING_HELPER), "bootstrap", "--profile", "office"],
                env=environment,
                check=False,
                capture_output=True,
                text=True,
            )

        self.assertNotEqual(result.returncode, 0)
        self.assertIn("refusing to overwrite existing key material", result.stderr)

    def test_bootstrap_refuses_dangling_public_key_symlink(self):
        with tempfile.TemporaryDirectory() as temporary_home:
            ssh_dir = Path(temporary_home) / ".ssh"
            ssh_dir.mkdir()
            (ssh_dir / "codex-signing-office-ed25519.pub").symlink_to(ssh_dir / "missing")

            environment = os.environ | {"HOME": temporary_home}
            result = subprocess.run(
                [str(SIGNING_HELPER), "bootstrap", "--profile", "office"],
                env=environment,
                check=False,
                capture_output=True,
                text=True,
            )

        self.assertNotEqual(result.returncode, 0)
        self.assertIn("refusing to overwrite existing key material", result.stderr)

    def test_bootstrap_uses_a_temporary_key_and_rejects_empty_passphrases(self):
        helper = SIGNING_HELPER.read_text(encoding="utf-8")

        self.assertIn('mktemp -d "$ssh_dir/.codex-signing-key.XXXXXX"', helper)
        self.assertIn('private_key="$temp_private"', helper)
        self.assertIn('refusing to load an unencrypted key', helper)
        self.assertIn("trap cleanup_temporary_key EXIT", helper)

    def test_bootstrap_prepares_signing_only_github_registration(self):
        helper = SIGNING_HELPER.read_text(encoding="utf-8")

        self.assertIn("gh ssh-key add", helper)
        self.assertIn("--type signing", helper)
        self.assertIn("does not contact GitHub", helper)

    def test_generated_signing_key_is_enrolled_for_local_verification(self):
        git_nix = GIT_NIX.read_text(encoding="utf-8")
        helper = SIGNING_HELPER.read_text(encoding="utf-8")

        self.assertIn('home.file.".ssh/allowed_signers.managed"', git_nix)
        self.assertIn("home.activation.gitAllowedSigners", git_nix)
        self.assertIn('allowedSignersFile = "~/.ssh/allowed_signers";', git_nix)
        self.assertIn("enroll_allowed_signer()", helper)
        self.assertIn('mv "$temporary_local_signers" "$local_allowed_signers"', helper)
        self.assertIn('cat "$managed_allowed_signers" > "$temporary_allowed_signers"', helper)
        self.assertIn('cat "$local_allowed_signers" >> "$temporary_allowed_signers"', helper)
        self.assertIn('cat "$MANAGED" > "$TMP"', git_nix)
        self.assertIn('cat "$LOCAL" >> "$TMP"', git_nix)

    def test_profile_key_rotation_replaces_its_local_verifier_entries(self):
        helper = SIGNING_HELPER.read_text(encoding="utf-8")

        self.assertIn('mv "$temporary_local_signers" "$local_allowed_signers"', helper)
        self.assertNotIn('>> "$local_allowed_signers"', helper)

    def test_office_guidance_keeps_unattended_publication_on_https(self):
        for guidance in (ROOT_AGENTS_MD, AGENTS_MD, PUBLISH_SKILL):
            text = guidance.read_text(encoding="utf-8")
            self.assertIn("HTTPS", text)
            self.assertIn("credential helper", text)
            self.assertIn("repo", text)
            self.assertIn("workflow", text)

    def test_no_profile_private_key_is_tracked(self):
        tracked_files = subprocess.run(
            ["git", "ls-files"],
            cwd=ROOT,
            check=True,
            capture_output=True,
            text=True,
        ).stdout.splitlines()

        self.assertFalse(
            [
                path
                for path in tracked_files
                if path.endswith(("codex-signing-office-ed25519", "codex-signing-personal-ed25519"))
            ],
            "profile-specific private key material must never be tracked",
        )
        private_key_marker = "BEGIN OPENSSH" + " PRIVATE KEY"
        private_key_files = subprocess.run(
            ["git", "grep", "-l", private_key_marker],
            cwd=ROOT,
            check=False,
            capture_output=True,
            text=True,
        ).stdout.splitlines()
        self.assertEqual(private_key_files, [], "no OpenSSH private key may be tracked")

    def test_guidance_distinguishes_machine_provenance_from_ai_attribution(self):
        for guidance in (ROOT_AGENTS_MD, AGENTS_MD, PUBLISH_SKILL):
            text = guidance.read_text(encoding="utf-8")
            self.assertIn("machine-key provenance", text)
            self.assertIn("per-commit human review", text)
            self.assertIn("Assisted-by", text)


if __name__ == "__main__":
    unittest.main()
