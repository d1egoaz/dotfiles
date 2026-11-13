#!/usr/bin/env python3
"""Behavior tests for dotfiles-local-state include_all resources.

Each test builds a throwaway HOME, age key, and SOPS file, so real local state
is never read or written.
"""

import json
import os
import pathlib
import shutil
import subprocess
import tempfile
import unittest

REPO_ROOT = pathlib.Path(__file__).resolve().parents[1]
SCRIPT = REPO_ROOT / "bin/files/dotfiles-local-state"
TOOLS = all(shutil.which(tool) for tool in ("sops", "age-keygen", "jq"))


@unittest.skipUnless(TOOLS, "sops, age-keygen, and jq are required")
class IncludeAllTest(unittest.TestCase):
    def setUp(self):
        self.tmp = tempfile.TemporaryDirectory()
        self.home = pathlib.Path(self.tmp.name)
        self.key = self.home / "age.key"
        subprocess.run(["age-keygen", "-o", str(self.key)], check=True, capture_output=True)
        public = subprocess.run(
            ["age-keygen", "-y", str(self.key)], check=True, capture_output=True, text=True
        ).stdout.strip()
        state = {
            "local_state": {
                "skills": {
                    "profile": "office",
                    "root": "$HOME/skills",
                    "include_all": True,
                    "files": [{"path": "$HOME/old/SKILL.md", "content": "old\n"}],
                }
            }
        }
        plain = self.home / "state.json"
        plain.write_text(json.dumps(state))
        self.sops_file = self.home / "state.yaml"
        encrypted = subprocess.run(
            ["sops", "--encrypt", "--age", public, "--input-type", "json", "--output-type", "yaml", str(plain)],
            check=True, capture_output=True, text=True, cwd=self.home,
        ).stdout
        self.sops_file.write_text(encrypted)
        plain.unlink()
        self.root = self.home / "skills"

    def tearDown(self):
        self.tmp.cleanup()

    def run_state(self, action):
        env = {
            "PATH": os.environ["PATH"],
            "HOME": str(self.home),
            "DOTFILES_LOCAL_STATE_PROFILE": "office",
            "DOTFILES_LOCAL_STATE_SOPS_FILE": str(self.sops_file),
            "SOPS_AGE_KEY_FILE": str(self.key),
        }
        return subprocess.run(
            ["bash", str(SCRIPT), action, "skills"], env=env, capture_output=True, text=True, cwd=self.home
        )

    def write(self, rel, content, mode=0o644):
        path = self.root / rel
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(content)
        path.chmod(mode)
        return path

    def test_new_files_are_untracked_until_sync(self):
        self.write("a/SKILL.md", "a\n")
        result = self.run_state("check")
        self.assertEqual(result.returncode, 1)
        self.assertIn(f"untracked: {self.root}/a/SKILL.md", result.stderr)

        self.assertEqual(self.run_state("sync").returncode, 0)
        self.assertEqual(self.run_state("check").returncode, 0)

    def test_restore_recreates_content_and_mode(self):
        self.write("a/SKILL.md", "a\n")
        self.write("b/run.sh", "#!/bin/sh\n", 0o755)
        self.assertEqual(self.run_state("sync").returncode, 0)

        shutil.rmtree(self.root)
        self.assertEqual(self.run_state("restore-if-missing").returncode, 0)
        self.assertEqual((self.root / "a/SKILL.md").read_text(), "a\n")
        self.assertEqual((self.root / "b/run.sh").stat().st_mode & 0o777, 0o755)
        self.assertFalse((self.home / "old/SKILL.md").exists())

    def test_deleted_files_leave_the_backup(self):
        self.write("a/SKILL.md", "a\n")
        self.write("b/SKILL.md", "b\n")
        self.assertEqual(self.run_state("sync").returncode, 0)

        shutil.rmtree(self.root / "a")
        self.assertEqual(self.run_state("sync").returncode, 0)
        shutil.rmtree(self.root)
        self.assertEqual(self.run_state("restore-if-missing").returncode, 0)
        self.assertFalse((self.root / "a").exists())
        self.assertEqual((self.root / "b/SKILL.md").read_text(), "b\n")


if __name__ == "__main__":
    unittest.main()
