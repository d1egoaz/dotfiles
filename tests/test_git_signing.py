#!/usr/bin/env python3
"""Regression coverage for profile-specific Git signing."""

import pathlib
import unittest


ROOT = pathlib.Path(__file__).resolve().parents[1]
MACHINES_NIX = ROOT / "nix/profiles/machines.nix"
GIT_NIX = ROOT / "nix/home-manager/config/apps/git.nix"
PROGRAMS_NIX = ROOT / "nix/home-manager/config/programs.nix"
XDG_NIX = ROOT / "nix/home-manager/config/xdg.nix"


class GitSigningConfigurationTest(unittest.TestCase):
    def test_profiles_select_distinct_public_signing_keys(self):
        machines = MACHINES_NIX.read_text(encoding="utf-8")

        self.assertIn('git_signing_key = "~/.ssh/codex-signing-office-ed25519.pub";', machines)
        self.assertIn('git_signing_key = "~/.ssh/codex-signing-personal-ed25519.pub";', machines)

    def test_office_signing_key_is_trusted_for_both_office_identities(self):
        machines = MACHINES_NIX.read_text(encoding="utf-8")

        self.assertIn('work_signing = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJnBxAfxKcz/D7Pu1MestMop4PJ6uDTi/uY9EiAqPwY4";', machines)
        self.assertEqual(machines.count("key = keys.work_signing;"), 2)

    def test_git_uses_mandatory_openssh_signing(self):
        git_nix = GIT_NIX.read_text(encoding="utf-8")

        self.assertIn("key = machineConfig.git_signing_key;", git_nix)
        self.assertIn("signByDefault = true;", git_nix)
        self.assertIn('format = "ssh";', git_nix)
        self.assertIn('program = "/usr/bin/ssh-keygen";', git_nix)
        self.assertNotIn("op-ssh-sign", git_nix)

    def test_login_uses_macos_keychain_not_a_custom_agent(self):
        programs_nix = PROGRAMS_NIX.read_text(encoding="utf-8")

        self.assertIn("launchd.agents.codexSigningKeychainLoader", programs_nix)
        self.assertIn('"--apple-load-keychain"', programs_nix)
        self.assertIn('"-t"', programs_nix)
        self.assertIn('"86400"', programs_nix)
        self.assertIn("signingPrivateKey", programs_nix)
        self.assertNotIn("ssh-agent -D", programs_nix)
        self.assertNotIn('"codex-signing-key"', XDG_NIX.read_text(encoding="utf-8"))

    def test_office_publication_keeps_https(self):
        git_nix = GIT_NIX.read_text(encoding="utf-8")

        self.assertNotIn("github.com-work", git_nix)
        self.assertNotIn("insteadOf", git_nix)


if __name__ == "__main__":
    unittest.main()
