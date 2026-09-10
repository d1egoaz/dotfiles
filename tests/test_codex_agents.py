#!/usr/bin/env python3
"""Regression tests for hardcoded Codex subagent routing."""

import pathlib
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
        read_only_roles = {"evidence-auditor", "explorer", "reviewer"}
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

    def test_lead_contract_matches_hardcoded_roles(self):
        agents_md = AGENTS_MD.read_text()
        expected_rows = (
            "| Unnamed fallback | `GPT-5.6-luna` | xhigh |",
            "| `evidence-auditor` | `GPT-5.6-terra` | xhigh |",
            "| `explorer` | `GPT-5.6-terra` | medium |",
            "| `worker` | `GPT-5.6-luna` | xhigh |",
            "| `reviewer` | `GPT-5.6-terra` | high |",
        )

        for row in expected_rows:
            self.assertIn(row, agents_md)
        self.assertIn(
            "Optimize expected outcome value",
            agents_md,
        )
        self.assertIn("Tokens from different models are not interchangeable", agents_md)
        self.assertIn(
            "Tokens from different models are not interchangeable",
            agents_md,
        )
        self.assertIn("Explicitly select the model and effort for every visible task", agents_md)
        self.assertIn("Prefer configured named roles for native subagents", agents_md)
        self.assertIn(
            "parallelism, context isolation, specialization, or independent verification",
            agents_md,
        )
        self.assertIn("A single reviewer or evidence auditor is valid", agents_md)
        self.assertIn("The lead owns planning, scope, delegation, communication", agents_md)
        self.assertIn("Before interrupting a running subagent", agents_md)
        self.assertIn("send a focused status request or redirect", agents_md)
        self.assertIn("Resume an idle agent instead of duplicating it", agents_md)
        self.assertIn("They do not coordinate with peers", agents_md)
        self.assertIn("discover peer IDs", agents_md)
        self.assertIn("spawn descendants", agents_md)
        self.assertIn("Do not duplicate the runtime skill catalog", agents_md)

    def test_tracked_agents_are_portable(self):
        allowed_keys = {
            "name",
            "description",
            "model",
            "model_reasoning_effort",
            "sandbox_mode",
            "developer_instructions",
        }
        forbidden_markers = ("http://", "https://", "/Users/", "/home/", "@")

        for path in AGENT_DIR.glob("*.toml"):
            text = path.read_text()
            with path.open("rb") as agent_file:
                agent = tomllib.load(agent_file)

            self.assertEqual(set(agent), allowed_keys, path.name)
            for marker in forbidden_markers:
                self.assertNotIn(marker, text, f"{path.name}: {marker}")

    def test_home_manager_links_custom_agents(self):
        xdg_nix = XDG_NIX.read_text()
        self.assertIn('".codex/agents".source', xdg_nix)
        self.assertIn("/dotfiles/config/codex/agents", xdg_nix)

    def test_task_coordinator_routes_and_labels_both_agent_layers(self):
        instructions = TASK_COORDINATOR_SKILL.read_text()
        normalized = " ".join(instructions.split())
        controls = (TASK_COORDINATOR_SKILL.parent / "references/codex-controls.md").read_text()
        creation = (TASK_COORDINATOR_SKILL.parent / "references/codex-task-creation.md").read_text()
        normalized_creation = " ".join(creation.split())

        self.assertIn("Explicitly pass a model and reasoning effort", normalized)
        self.assertIn("Sol, Terra, and Luna are all valid task routes", normalized)
        self.assertIn("proactively spawn the minimum useful named", normalized)
        self.assertIn("Treat explicit `$task-coordinator` invocation as a request", normalized)
        self.assertIn("create at least one visible implementation task", normalized)
        self.assertIn("If no visible task is justified, explain why", normalized)
        self.assertIn("reference-only repository analysis in a native `explorer`", normalized)
        self.assertIn("Whenever this skill applies, rename the lead", normalized)
        self.assertIn("ordinary continuation of one task does not count", normalized)
        self.assertIn("Reassess the decomposition when a material follow-up", normalized)
        self.assertIn("🤖 [<key>] <goal>", instructions)
        self.assertIn("[<key>] <model-label>-<effort> <scope>: <outcome>", instructions)
        self.assertIn("Selected model: <exact-model-id>", instructions)
        self.assertIn("Selected effort: <effort>", instructions)
        self.assertIn("Display route: <model-label>-<effort>", instructions)
        self.assertIn("<key>_<model-label>_<effort>_<role>_<slice>", instructions)
        self.assertIn("[<key>] <model-label>-<effort> <role>: <slice>", instructions)
        self.assertIn("Never put a raw model ID", normalized)
        self.assertIn("at most 56 characters", normalized)
        self.assertIn("Normalize a plan-supplied title", normalized)
        self.assertIn("longest goal prefix that fits at a word boundary", normalized)
        self.assertIn("Do not paraphrase or add an ellipsis", normalized)
        self.assertIn("Before interrupting a running subagent", controls)
        self.assertIn("next safe boundary", controls)
        self.assertIn("send a follow-up or resume it instead of spawning a", controls)
        self.assertIn("saved Git project", creation)
        self.assertIn("isGitRepository = true", creation)
        self.assertIn("saved non-Git umbrella", creation)
        self.assertIn("returned `clientThreadId` means worktree setup is pending", normalized_creation)
        self.assertIn("Do not call `create_thread` again", normalized_creation)
        self.assertIn("add `(retry N)` after the outcome", normalized_creation)
        self.assertIn("`(superseded)`", normalized_creation)
        self.assertIn("A timeout, missing `threadId`, or `clientThreadId` alone is not failure", normalized_creation)
        self.assertIn("one more than the highest existing retry number", normalized_creation)
        self.assertIn("retain the intended title", normalized_creation)
        self.assertIn("no atomic key reservation", normalized_creation)
        self.assertIn("complete retry title remains within 56 characters", normalized_creation)
        self.assertIn("earliest `createdAt` as owner", normalized_creation)
        self.assertIn("lexical `threadId`", normalized_creation)
        for runtime_tool in (
            "list_agents",
            "send_message",
            "wait_agent",
            "followup_task",
            "interrupt_agent",
            "wait_threads",
            "read_thread",
            "send_message_to_thread",
        ):
            self.assertIn(f"`{runtime_tool}`", controls)
        for app_server_name in (
            "thread/read",
            "turn/steer",
            "turn/start",
            "turn/interrupt",
            "thread/started",
            "item/started",
            "item/completed",
            "turn/completed",
        ):
            self.assertIn(f"`{app_server_name}`", controls)
        self.assertIn("not lifecycle events or universal public API methods", controls)
        for nonportable_marker in ("/Users/", "/home/", "http://", "https://", "@"):
            for document in (instructions, controls, creation):
                self.assertNotIn(nonportable_marker, document)


if __name__ == "__main__":
    unittest.main()
