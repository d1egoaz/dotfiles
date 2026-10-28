#!/usr/bin/env python3
"""Independent subagent spawn-depth safety checks."""

import json
import pathlib
import tomllib
import unittest


REPO_ROOT = pathlib.Path(__file__).resolve().parents[1]


class SpawnDepthTest(unittest.TestCase):
    def test_codex_subagents_cannot_spawn_descendants(self):
        with (REPO_ROOT / "config/codex/config.toml").open("rb") as config_file:
            config = tomllib.load(config_file)
        self.assertEqual(config["agents"]["max_depth"], 1)

    def test_claude_subagents_cannot_spawn_descendants(self):
        settings = json.loads((REPO_ROOT / "config/claude/settings.json").read_text())
        self.assertEqual(settings["env"]["CLAUDE_CODE_MAX_SUBAGENT_SPAWN_DEPTH"], "1")


if __name__ == "__main__":
    unittest.main()
