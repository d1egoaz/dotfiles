#!/usr/bin/env python3
"""Regression tests for hardcoded Codex subagent routing."""

import pathlib
import re
import tomllib
import unittest


REPO_ROOT = pathlib.Path(__file__).resolve().parents[1]
AGENT_DIR = REPO_ROOT / "config/codex/agents"
AGENTS_MD = REPO_ROOT / "config/ai/AGENTS.md"
CONFIG_TOML = REPO_ROOT / "config/codex/config.toml"
TASK_COORDINATOR_SKILL = REPO_ROOT / "config/agents/skills/multi-agent-team/task-coordinator/SKILL.md"
XDG_NIX = REPO_ROOT / "nix/home-manager/config/xdg.nix"

EXPECTED_AGENTS = {
    "evidence-auditor": {
        "model": "gpt-5.6-terra",
        "model_reasoning_effort": "xhigh",
        "sandbox_mode": "read-only",
    },
    "explorer": {
        "model": "gpt-5.6-terra",
        "model_reasoning_effort": "medium",
        "sandbox_mode": "read-only",
    },
    "reviewer": {
        "model": "gpt-5.6-terra",
        "model_reasoning_effort": "high",
        "sandbox_mode": "read-only",
    },
    "utility": {
        "model": "gpt-5.6-luna",
        "model_reasoning_effort": "medium",
        "sandbox_mode": "read-only",
    },
    "worker": {
        "model": "gpt-5.6-luna",
        "model_reasoning_effort": "xhigh",
        "sandbox_mode": "workspace-write",
    },
}


class CodexAgentsTest(unittest.TestCase):
    def test_global_fallback_depth_and_concurrency_are_hardcoded(self):
        with CONFIG_TOML.open("rb") as config_file:
            agents = tomllib.load(config_file)["agents"]

        self.assertEqual(agents["default_subagent_model"], "gpt-5.6-luna")
        self.assertEqual(agents["default_subagent_reasoning_effort"], "xhigh")
        self.assertEqual(agents["max_depth"], 1)
        self.assertEqual(agents["max_concurrent_threads_per_session"], 10)
        self.assertNotIn("max_threads", agents)

    def test_named_agent_files_match_the_routing_contract(self):
        self.assertEqual(
            {path.stem for path in AGENT_DIR.glob("*.toml")},
            set(EXPECTED_AGENTS),
        )

        for name, expected in EXPECTED_AGENTS.items():
            with (AGENT_DIR / f"{name}.toml").open("rb") as agent_file:
                agent = tomllib.load(agent_file)

            self.assertEqual(agent["name"], name)
            for key, value in expected.items():
                self.assertEqual(agent[key], value, f"{name}.{key}")
            self.assertTrue(agent["description"].strip())
            instructions = agent["developer_instructions"]
            self.assertTrue(instructions.strip())

            # Named workers remain lead-controlled and cannot create a second
            # delegation tree or communicate around the lead.
            self.assertIn("Do not message or coordinate with peer agents", instructions)
            self.assertIn("discover peer IDs", instructions)
            self.assertIn("spawn descendants", instructions)
            self.assertIn("parent lead", instructions)

    def test_named_roles_have_only_the_expected_write_capability(self):
        read_only_roles = {"evidence-auditor", "explorer", "reviewer", "utility"}
        for name, expected in EXPECTED_AGENTS.items():
            with (AGENT_DIR / f"{name}.toml").open("rb") as agent_file:
                agent = tomllib.load(agent_file)

            if name in read_only_roles:
                self.assertEqual(agent["sandbox_mode"], "read-only")
            else:
                self.assertEqual(name, "worker")
                self.assertEqual(agent["sandbox_mode"], "workspace-write")

        self.assertEqual(
            {name for name, expected in EXPECTED_AGENTS.items() if expected["sandbox_mode"] == "workspace-write"},
            {"worker"},
        )

    def test_documented_routes_match_runtime_configuration(self):
        rows = re.findall(
            r"^\| (Unnamed fallback|`[^`]+`) \| `([^`]+)` \| ([a-z]+) \|",
            AGENTS_MD.read_text(),
            re.MULTILINE,
        )
        documented = {name.strip("`"): (model, effort) for name, model, effort in rows}
        self.assertEqual(len(rows), len(documented), "duplicate documented routes")
        self.assertEqual(set(documented), {*EXPECTED_AGENTS, "Unnamed fallback"})

        with CONFIG_TOML.open("rb") as config_file:
            defaults = tomllib.load(config_file)["agents"]
        self.assertEqual(
            documented["Unnamed fallback"],
            (defaults["default_subagent_model"], defaults["default_subagent_reasoning_effort"]),
        )
        for name in EXPECTED_AGENTS:
            with (AGENT_DIR / f"{name}.toml").open("rb") as agent_file:
                agent = tomllib.load(agent_file)
            self.assertEqual(
                documented[name],
                (agent["model"], agent["model_reasoning_effort"]),
                name,
            )

    def test_tracked_agents_are_portable(self):
        required_keys = {
            "name",
            "description",
            "model",
            "model_reasoning_effort",
            "sandbox_mode",
            "developer_instructions",
        }
        allowed_keys = required_keys | {"model_provider"}
        forbidden_markers = ("http://", "https://", "/Users/", "/home/", "@")

        for path in AGENT_DIR.glob("*.toml"):
            text = path.read_text()
            with path.open("rb") as agent_file:
                agent = tomllib.load(agent_file)

            self.assertLessEqual(set(agent), allowed_keys, path.name)
            self.assertLessEqual(required_keys, set(agent), path.name)
            # Roles are shared across profiles, so a pinned provider must be a
            # built-in one. Profile-local providers would break other machines.
            if "model_provider" in agent:
                self.assertEqual(agent["model_provider"], "openai", path.name)
            for marker in forbidden_markers:
                self.assertNotIn(marker, text, f"{path.name}: {marker}")

    def test_home_manager_links_custom_agents(self):
        xdg_nix = XDG_NIX.read_text()
        self.assertIn('".codex/agents".source', xdg_nix)
        self.assertIn("/dotfiles/config/codex/agents", xdg_nix)

    def test_coordinator_references_are_linked_and_portable(self):
        skill_root = TASK_COORDINATOR_SKILL.parent
        references = set((skill_root / "references").glob("*.md"))
        documents = {TASK_COORDINATOR_SKILL, *references}
        linked = set()
        for path in documents:
            source = path.read_text()
            for target in re.findall(r"\]\(([^)]+\.md)\)", source):
                resolved = (path.parent / target).resolve()
                self.assertTrue(resolved.is_relative_to(skill_root), target)
                self.assertTrue(resolved.is_file(), target)
                linked.add(resolved)
            for marker in ("/Users/", "/home/", "http://", "https://", "@"):
                self.assertNotIn(marker, source, path.name)
        self.assertEqual(linked, references, "unreachable coordinator references")


if __name__ == "__main__":
    unittest.main()
