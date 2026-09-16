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
CAPABILITY_ROUTING = TASK_COORDINATOR_SKILL.parent / "references/capability-routing.md"
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

    def test_lead_contract_matches_hardcoded_roles(self):
        agents_md = AGENTS_MD.read_text()
        expected_rows = (
            "| Unnamed fallback | `GPT-5.6-luna` | xhigh |",
            "| `evidence-auditor` | `GPT-5.6-terra` | xhigh |",
            "| `explorer` | `GPT-5.6-terra` | medium |",
            "| `utility` | `GPT-5.6-luna` | medium |",
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
        self.assertIn("choose the cheapest adequate model and effort", agents_md)
        self.assertIn("Reclassify materially different follow-ups", agents_md)
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
        routing = CAPABILITY_ROUTING.read_text()
        normalized_routing = " ".join(routing.split())
        controls = (TASK_COORDINATOR_SKILL.parent / "references/codex-controls.md").read_text()
        normalized_controls = " ".join(controls.split())
        creation = (TASK_COORDINATOR_SKILL.parent / "references/codex-task-creation.md").read_text()
        normalized_creation = " ".join(creation.split())
        metadata = (TASK_COORDINATOR_SKILL.parent / "agents/openai.yaml").read_text()

        for contract in (
            "references/capability-routing.md",
            "references/codex-task-creation.md",
            "references/codex-controls.md",
            "cheapest adequate model and effort",
            "fenced Markdown `text` block",
            "🤖 [<key>] <goal>",
            "[<key>] <model-label>-<effort-code> <action> <object>[: <outcome>]",
            "<key>_<model-label>_<effort-code>_<role>_<slice>",
            "[<key>] <model-label>-<effort-code> <role>: <slice>",
            "at most 72 characters",
            "action/object before context",
            "keep exact model/full effort",
            "Visible tasks are user-owned",
            "Native subagents return only to the parent",
            "lead never proxies it",
            "Creation never authorizes commit, push, PR",
        ):
            self.assertIn(contract, normalized)
        for field in (
            "Work unit and expected output",
            "Scope: local | bounded multi-component | cross-system",
            "Ambiguity: low | medium | high",
            "Judgment: procedural | synthesis | adversarial | exceptional",
            "Verification: objective | partial | subjective/unknown",
            "Adaptivity: bounded | iterative | open-ended",
            "Consequence if wrong: low | material | high",
            "Cheapest capable route",
            "Why cheaper routes are insufficient",
            "Escalate when",
        ):
            self.assertIn(field, routing)
        for scenario, expected_route in {
            "one_step": "Lead: one-step work",
            "bounded_read": "Luna-medium `utility`",
            "scoped_write": "Luna-xhigh `worker`",
            "broad_mapping": "Terra-medium `explorer`",
            "correctness_review": "Terra-high `reviewer`",
            "conflicting_evidence": "Terra-xhigh `evidence-auditor`",
            "exceptional_judgment": "Sol-high/xhigh",
        }.items():
            self.assertIn(expected_route, routing, scenario)
        for contract in (
            "not an additive score",
            "High consequence alone does not select a premium model",
            "Missing access, approval, data, or tools means blocked",
            "Never repeat a side effect",
        ):
            self.assertIn(contract, normalized_routing)
        for contract in (
            "exact lead `projectId`",
            "Use projectless only for a projectless lead",
            "inspect `isGitRepository`",
            "`clientThreadId`: setup pending, not failure",
            "earliest `createdAt`",
            "lexical `threadId`",
            "`(superseded)`",
            "ask once, and wait",
            "never proxy, quote, or duplicate approval",
            "Creation never authorizes commit, push, PR",
        ):
            self.assertIn(contract, normalized_creation)
        for effort, code in {
            "none": "n",
            "minimal": "min",
            "low": "lo",
            "medium": "med",
            "high": "hi",
            "xhigh": "xh",
            "max": "max",
            "ultra": "ult",
        }.items():
            self.assertIn(f"`{code}`={effort}", normalized_creation)
        self.assertIn("readability budget is not a platform limit", normalized_creation)
        self.assertIn("Never remove action/object or add an ellipsis", normalized_creation)
        self.assertIn("context-rich titles with compact route prefixes", metadata)

        visible_titles = (
            "[IC-563] Terra-xh Verify controls: close remaining rollout gaps",
            "[IC-563] Terra-xh Reconcile records: confirm final desired state",
            "[IC-563] Sol-xh Coordinate rollout batches and acceptance",
            "[INF-11223] Luna-xh Stage 0 fixes: restore USE2 parity",
            "[SIGNING-PROFILES] Terra-xh Verify signing key restoration",
            "[IC-563] Terra-xh Verify controls: close gaps (retry 2)",
            "[IC-563] Terra-xh Verify controls: close gaps (superseded)",
        )
        self.assertEqual(len(visible_titles), len(set(visible_titles)))
        for title in visible_titles:
            self.assertLessEqual(len(title), 72, title)
            self.assertRegex(title, r"^\[[^]]+\] (Sol|Terra|Luna)-(n|min|lo|med|hi|xh|max|ult) ")
        self.assertIn("Visible tasks ask the user directly for approval", normalized_controls)
        self.assertIn("lead waits and never relays approval", normalized_controls)
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
        self.assertIn("not universal model tools", controls)
        for nonportable_marker in ("/Users/", "/home/", "http://", "https://", "@"):
            for document in (instructions, controls, creation):
                self.assertNotIn(nonportable_marker, document)


if __name__ == "__main__":
    unittest.main()
