#!/usr/bin/env python3
"""Regression tests for generated Codex agent routing."""

import copy
import importlib.machinery
import importlib.util
import pathlib
import re
import subprocess
import tempfile
import tomllib
import unittest
from unittest import mock


REPO_ROOT = pathlib.Path(__file__).resolve().parents[1]
REGISTRY = REPO_ROOT / "nix/data/agent-routing.toml"
ROUTING_GENERATOR = REPO_ROOT / "bin/files/agent-routing-generate"
AGENT_ROOT = REPO_ROOT / "config/codex/agents/generated"
AGENTS_MD = REPO_ROOT / "config/ai/AGENTS.md"
ROOT_AGENTS_MD = REPO_ROOT / "AGENTS.md"
CONFIG_TOML = REPO_ROOT / "config/codex/config.toml"
TASK_COORDINATOR_SKILL = REPO_ROOT / "config/agents/skills/multi-agent-team/task-coordinator/SKILL.md"
CAPABILITY_ROUTING = TASK_COORDINATOR_SKILL.parent / "references/capability-routing.md"
XDG_NIX = REPO_ROOT / "nix/home-manager/config/xdg.nix"
PI_NIX = REPO_ROOT / "nix/home-manager/config/apps/pi.nix"

EXPECTED_ROLES = {
    "utility": {"tier": "economy", "effort": "medium", "sandbox_mode": "read-only"},
    "worker": {"tier": "economy", "effort": "xhigh", "sandbox_mode": "workspace-write"},
    "explorer": {"tier": "balanced", "effort": "medium", "sandbox_mode": "read-only"},
    "reviewer": {"tier": "balanced", "effort": "high", "sandbox_mode": "read-only"},
    "evidence-auditor": {"tier": "balanced", "effort": "xhigh", "sandbox_mode": "read-only"},
}
EXPECTED_AGENT_KEYS = {
    "name",
    "description",
    "model",
    "model_reasoning_effort",
    "sandbox_mode",
    "developer_instructions",
}
EXPECTED_GENERIC_AGENT_KEYS = {
    "name",
    "description",
    "model",
    "model_reasoning_effort",
    "developer_instructions",
}
EXPECTED_CANONICAL = {
    "spawn-subagent-economy-check": "utility",
    "spawn-subagent-economy-implement": "worker",
    "spawn-subagent-balanced-explore": "explorer",
    "spawn-subagent-balanced-review": "reviewer",
    "spawn-subagent-balanced-audit": "evidence-auditor",
}
EXPECTED_GENERIC = {
    "economy": "xhigh",
    "balanced": "high",
    "frontier": "xhigh",
}


def load_registry():
    with REGISTRY.open("rb") as registry_file:
        return tomllib.load(registry_file)


def load_generator_module():
    loader = importlib.machinery.SourceFileLoader("agent_routing_generate", str(ROUTING_GENERATOR))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    if spec is None or spec.loader is None:
        raise RuntimeError("cannot load agent routing generator")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class CodexAgentsTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.registry = load_registry()
        cls.generator = load_generator_module()

    def test_registry_has_required_tiers_roles_and_runtimes(self):
        self.assertEqual(self.registry["tiers"], ["economy", "balanced", "frontier"])
        self.assertEqual(set(self.registry["roles"]), set(EXPECTED_ROLES))
        self.assertEqual(
            set(self.registry["runtimes"]),
            {"office_codex", "personal_opencode_go"},
        )
        self.assertEqual(
            {
                name: role["canonical_name"]
                for name, role in self.registry["roles"].items()
            },
            {role: launcher for launcher, role in EXPECTED_CANONICAL.items()},
        )
        self.assertEqual(
            {
                tier: self.registry["launchers"]["generic"][tier]["effort"]
                for tier in self.registry["tiers"]
            },
            EXPECTED_GENERIC,
        )
        for tier in self.registry["tiers"]:
            generic = self.registry["launchers"]["generic"][tier]
            self.assertEqual(generic["sandbox_mode"], "inherit")
            self.assertEqual(generic["approval_policy"], "inherit")

    def test_registry_runtime_models_are_provider_specific(self):
        office = self.registry["runtimes"]["office_codex"]["tiers"]
        self.assertEqual(
            [office[tier]["model"] for tier in self.registry["tiers"]],
            ["gpt-5.6-luna", "gpt-5.6-terra", "gpt-5.6-sol"],
        )
        personal = self.registry["runtimes"]["personal_opencode_go"]["tiers"]
        self.assertEqual(
            {personal[tier]["model"] for tier in self.registry["tiers"]},
            {"deepseek-v4.1-flash"},
        )

    def test_named_agent_outputs_match_role_contract(self):
        for profile, runtime in (
            ("office", "office_codex"),
            ("personal", "personal_opencode_go"),
        ):
            paths = set((AGENT_ROOT / profile).glob("*.toml"))
            self.assertEqual(
                {path.stem for path in paths},
                set(EXPECTED_ROLES) | set(EXPECTED_CANONICAL) | {
                    f"spawn-subagent-{tier}" for tier in self.registry["tiers"]
                },
            )
            for name, expected in EXPECTED_ROLES.items():
                role = self.registry["roles"][name]
                selected = self.registry["runtimes"][runtime]["tiers"][role["tier"]]
                generated = {}
                for launcher_name in (role["canonical_name"], *role["legacy_aliases"]):
                    with (AGENT_ROOT / profile / f"{launcher_name}.toml").open("rb") as agent_file:
                        agent = tomllib.load(agent_file)
                    generated[launcher_name] = agent
                    self.assertEqual(set(agent), EXPECTED_AGENT_KEYS)
                    self.assertEqual(agent["name"], launcher_name)
                    self.assertEqual(agent["model"], selected["model"])
                    self.assertEqual(agent["model_reasoning_effort"], expected["effort"])
                    self.assertEqual(agent["sandbox_mode"], expected["sandbox_mode"])
                    self.assertTrue(agent["developer_instructions"].strip())
                    self.assertIn("parent lead", agent["developer_instructions"])
                    self.assertIn("native-subagent launcher", agent["developer_instructions"])
                canonical = generated[role["canonical_name"]]
                for alias in role["legacy_aliases"]:
                    alias_agent = generated[alias].copy()
                    alias_agent.pop("name")
                    self.assertIn(f"@{role['canonical_name']}", alias_agent["description"])
                    alias_agent.pop("description")
                    canonical_agent = canonical.copy()
                    canonical_agent.pop("name")
                    canonical_agent.pop("description")
                    self.assertEqual(alias_agent, canonical_agent)

            for tier, expected_effort in EXPECTED_GENERIC.items():
                launcher_name = f"spawn-subagent-{tier}"
                with (AGENT_ROOT / profile / f"{launcher_name}.toml").open("rb") as agent_file:
                    agent = tomllib.load(agent_file)
                selected = self.registry["runtimes"][runtime]["tiers"][tier]
                self.assertEqual(set(agent), EXPECTED_GENERIC_AGENT_KEYS)
                self.assertEqual(agent["name"], launcher_name)
                self.assertEqual(agent["model"], selected["model"])
                self.assertEqual(agent["model_reasoning_effort"], expected_effort)
                self.assertNotIn("sandbox_mode", agent)
                self.assertNotIn("approval_policy", agent)
                self.assertIn("parent lead's handoff", agent["developer_instructions"])
                self.assertIn("inherited", agent["developer_instructions"])
                self.assertIn("native-subagent launcher", agent["developer_instructions"])

    def test_global_config_keeps_limits_but_not_hand_maintained_route_ids(self):
        with CONFIG_TOML.open("rb") as config_file:
            agents = tomllib.load(config_file)["agents"]
        self.assertEqual(agents["max_depth"], 1)
        self.assertEqual(agents["max_concurrent_threads_per_session"], 10)
        self.assertNotIn("default_subagent_model", agents)
        self.assertNotIn("default_subagent_reasoning_effort", agents)
        config_text = CONFIG_TOML.read_text()
        for model in ("gpt-5.6-luna", "gpt-5.6-terra", "gpt-5.6-sol", "deepseek-v4.1-flash"):
            self.assertNotIn(model, config_text)

    def test_active_wiring_has_no_duplicate_model_ids(self):
        model_ids = {
            route["model"]
            for runtime in self.registry["runtimes"].values()
            for route in runtime["tiers"].values()
            if runtime["provider"] == "codex"
        }
        source_paths = (
            AGENTS_MD,
            CONFIG_TOML,
            XDG_NIX,
            PI_NIX,
            TASK_COORDINATOR_SKILL,
        )
        for path in source_paths:
            contents = path.read_text()
            for model_id in model_ids:
                self.assertNotIn(model_id, contents, path.name)

    def test_documentation_is_provider_neutral_and_has_runtime_fields(self):
        for document in (AGENTS_MD, TASK_COORDINATOR_SKILL, CAPABILITY_ROUTING):
            contents = document.read_text()
            for token in ("economy", "balanced", "frontier", "effective effort"):
                self.assertIn(token, contents)
        routing = CAPABILITY_ROUTING.read_text()
        for token in ("Tier:", "Runtime:", "Resolved model label:", "Requested effort:", "Effective effort:"):
            self.assertIn(token, routing)

    def test_shared_routing_instructions_use_tiers_not_concrete_routes(self):
        shared_skills = sorted(
            (REPO_ROOT / "config/agents/skills/multi-agent-team").rglob("*.md")
        )
        documents = [ROOT_AGENTS_MD, AGENTS_MD, *shared_skills]
        model_ids = {
            route["model"]
            for runtime in self.registry["runtimes"].values()
            for route in runtime["tiers"].values()
        }
        model_labels = {
            route["label"]
            for runtime in self.registry["runtimes"].values()
            for route in runtime["tiers"].values()
        }
        for path in documents:
            contents = path.read_text()
            for model_id in model_ids:
                self.assertNotIn(model_id, contents, path.name)
            for label in model_labels:
                self.assertIsNone(
                    re.search(rf"(?<![\w-]){re.escape(label)}(?![\w-])", contents, re.IGNORECASE),
                    path.name,
                )

    def test_routing_names_include_tier_and_model_label(self):
        documents = (
            AGENTS_MD.read_text(),
            TASK_COORDINATOR_SKILL.read_text(),
            CAPABILITY_ROUTING.read_text(),
            (TASK_COORDINATOR_SKILL.parent / "references/codex-task-creation.md").read_text(),
        )
        combined = "\n".join(documents)
        self.assertIn("<key>_<tier>_<model-label>_<effort-code>_<role>_<slice>", combined)
        self.assertIn("[<key>] <tier>-<model-label>-<effort-code>", combined)

    def test_generator_is_deterministic_and_current(self):
        result = subprocess.run(
            ["python3", str(ROUTING_GENERATOR), "--check"],
            cwd=REPO_ROOT,
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertIn("28 files", result.stdout)

    def test_launcher_names_remain_stable_when_model_mapping_changes(self):
        before = self.generator.build_outputs(copy.deepcopy(self.registry))
        after_registry = copy.deepcopy(self.registry)
        route = after_registry["runtimes"]["office_codex"]["tiers"]["frontier"]
        route["model"] = "gpt-6.0-astra"
        route["label"] = "astra"
        after = self.generator.build_outputs(after_registry)

        self.assertEqual(set(before), set(after))
        launcher_paths = {
            path for path in before if "spawn-subagent-" in path.name
        }
        self.assertTrue(launcher_paths)
        self.assertTrue(
            all("astra" not in path.name and "sol" not in path.name for path in launcher_paths)
        )
        frontier_path = next(
            path
            for path in launcher_paths
            if path.name == "spawn-subagent-frontier.toml" and "office" in path.parts
        )
        self.assertIn('model = "gpt-6.0-astra"', after[frontier_path])

    def test_generic_rendering_omits_boundary_overrides(self):
        outputs = self.generator.build_outputs(self.registry)
        for path, content in outputs.items():
            if "spawn-subagent-" not in path.name:
                continue
            if path.name.removesuffix(".toml") in EXPECTED_CANONICAL:
                continue
            self.assertNotIn("sandbox_mode", content)
            self.assertNotIn("approval_policy", content)

    def test_registry_rejects_invalid_provider(self):
        source = REGISTRY.read_text()
        with tempfile.TemporaryDirectory() as temp_dir:
            registry = pathlib.Path(temp_dir) / "agent-routing.toml"
            registry.write_text(source.replace('provider = "codex"', 'provider = "unknown"', 1))
            with mock.patch.object(self.generator, "REGISTRY_PATH", registry):
                with self.assertRaisesRegex(self.generator.RoutingError, "provider is invalid"):
                    self.generator.read_registry()

    def test_registry_rejects_invalid_role_tier_and_effort(self):
        source = REGISTRY.read_text()
        cases = (
            ('tier = "economy"', 'tier = "unknown"', "not a known tier"),
            ('effort = "medium"', 'effort = "impossible"', "not a known effort"),
        )
        for old, new, message in cases:
            with self.subTest(field=old), tempfile.TemporaryDirectory() as temp_dir:
                registry = pathlib.Path(temp_dir) / "agent-routing.toml"
                registry.write_text(source.replace(old, new, 1))
                with mock.patch.object(self.generator, "REGISTRY_PATH", registry):
                    with self.assertRaisesRegex(self.generator.RoutingError, message):
                        self.generator.read_registry()

    def test_registry_rejects_malformed_tier_role_and_runtime_sets(self):
        source = REGISTRY.read_text()
        cases = (
            (
                source.replace(
                    'tiers = ["economy", "balanced", "frontier"]',
                    'tiers = ["economy", "balanced"]',
                    1,
                ),
                "tiers must be exactly",
            ),
            (source.replace("[roles.utility]", "[roles.unexpected]", 1), "roles must be exactly"),
            (
                source.replace("[runtimes.personal_opencode_go]", "[runtimes.unexpected]", 1),
                "runtimes must be exactly",
            ),
            (source + "\n[runtimes.office_codex]\n", "cannot read"),
        )
        for mutated, message in cases:
            with self.subTest(message=message), tempfile.TemporaryDirectory() as temp_dir:
                registry = pathlib.Path(temp_dir) / "agent-routing.toml"
                registry.write_text(mutated)
                with mock.patch.object(self.generator, "REGISTRY_PATH", registry):
                    with self.assertRaisesRegex(self.generator.RoutingError, message):
                        self.generator.read_registry()

    def test_render_rejects_unsupported_lead_and_pi_efforts(self):
        cases = ("lead", "pi")
        for consumer in cases:
            with self.subTest(consumer=consumer):
                registry = copy.deepcopy(self.registry)
                office = registry["runtimes"]["office_codex"]["tiers"]["frontier"]
                office["supported_efforts"].remove("xhigh")
                if consumer == "pi":
                    registry["profiles"]["office"]["codex_lead_effort"] = "high"
                else:
                    registry["profiles"]["office"]["pi_effort"] = "high"
                with self.assertRaisesRegex(
                    self.generator.RoutingError,
                    "frontier does not support requested effort xhigh",
                ):
                    self.generator.render_nix(registry)

    def test_home_manager_uses_profile_generated_codex_routes(self):
        xdg = XDG_NIX.read_text()
        self.assertIn("agent-routing.generated.nix", xdg)
        self.assertIn("config/codex/agents/generated/${codex.agent_directory}", xdg)
        self.assertIn('".claude/settings.json".source', xdg)
        self.assertIn("config/claude/settings.json", xdg)
        self.assertNotIn('".claude/agents".source', xdg)
        self.assertNotIn("settings.generated.json", xdg)
        self.assertFalse((REPO_ROOT / "config/claude/settings.generated.json").exists())
        self.assertFalse((REPO_ROOT / "config/claude/agents/generated").exists())
        self.assertIn('CODEX_DEFAULT_MODEL="${codex.default.model}"', xdg)
        self.assertIn('default_subagent_model = \\"" default_model', xdg)
        pi = PI_NIX.read_text()
        self.assertIn("routing.profiles.${profile}.pi", pi)
        self.assertIn("piRoute.model", pi)

    def test_no_direct_handwritten_codex_agent_files_remain(self):
        self.assertEqual(list((REPO_ROOT / "config/codex/agents").glob("*.toml")), [])

if __name__ == "__main__":
    unittest.main()
