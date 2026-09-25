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
    "worker": {"tier": "economy", "effort": "xhigh", "sandbox_mode": "workspace-write"},
    "explorer": {"tier": "balanced", "effort": "low", "sandbox_mode": "read-only"},
    "reviewer": {"tier": "balanced", "effort": "low", "sandbox_mode": "read-only"},
}
EXPECTED_AGENT_KEYS = {
    "name",
    "description",
    "model",
    "model_reasoning_effort",
    "sandbox_mode",
    "developer_instructions",
}
EXPECTED_CANONICAL = {
    "spawn-subagent-explore": "explorer",
    "spawn-subagent-implement": "worker",
    "spawn-subagent-review": "reviewer",
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
        self.assertNotIn("launchers", self.registry)

    def test_registry_runtime_models_are_provider_specific(self):
        office = self.registry["runtimes"]["office_codex"]["tiers"]
        self.assertEqual(
            [office[tier]["model"] for tier in self.registry["tiers"]],
            ["gpt-6-luna", "gpt-6-sol", "gpt-6-sol"],
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
            self.assertEqual({path.stem for path in paths}, set(EXPECTED_CANONICAL))
            for name, expected in EXPECTED_ROLES.items():
                role = self.registry["roles"][name]
                selected = self.registry["runtimes"][runtime]["tiers"][role["tier"]]
                with (AGENT_ROOT / profile / f"{role['canonical_name']}.toml").open("rb") as agent_file:
                    agent = tomllib.load(agent_file)
                self.assertEqual(set(agent), EXPECTED_AGENT_KEYS)
                self.assertEqual(agent["name"], role["canonical_name"])
                self.assertEqual(agent["model"], selected["model"])
                self.assertEqual(agent["model_reasoning_effort"], expected["effort"])
                self.assertEqual(agent["sandbox_mode"], expected["sandbox_mode"])
                self.assertIn("parent lead", agent["developer_instructions"])
                self.assertIn("native-subagent launcher", agent["developer_instructions"])
                if expected["sandbox_mode"] == "read-only":
                    self.assertIn("couldn't confirm", agent["developer_instructions"])

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
        routing = CAPABILITY_ROUTING.read_text()
        for token in ("economy", "balanced", "frontier", *EXPECTED_CANONICAL):
            self.assertIn(token, routing)
        self.assertNotIn("scorecard", routing.lower())
        skill = TASK_COORDINATOR_SKILL.read_text()
        for launcher in EXPECTED_CANONICAL:
            self.assertIn(f"@{launcher}", skill)

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
        self.assertIn("8 files", result.stdout)

    def test_launcher_names_remain_stable_when_model_mapping_changes(self):
        before = self.generator.build_outputs(copy.deepcopy(self.registry))
        after_registry = copy.deepcopy(self.registry)
        route = after_registry["runtimes"]["office_codex"]["tiers"]["balanced"]
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
        review_path = next(
            path
            for path in launcher_paths
            if path.name == "spawn-subagent-review.toml" and "office" in path.parts
        )
        self.assertIn('model = "gpt-6.0-astra"', after[review_path])

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
            ('effort = "xhigh"', 'effort = "impossible"', "not a known effort"),
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
            (source.replace("[roles.explorer]", "[roles.unexpected]", 1), "roles must be exactly"),
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
                office["supported_efforts"].remove("high")
                if consumer == "pi":
                    registry["profiles"]["office"]["codex_lead_effort"] = "xhigh"
                else:
                    registry["profiles"]["office"]["pi_effort"] = "xhigh"
                with self.assertRaisesRegex(
                    self.generator.RoutingError,
                    "frontier does not support requested effort high",
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
