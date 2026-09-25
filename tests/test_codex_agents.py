#!/usr/bin/env python3
"""Invariant tests for the agent routing registry and its generated outputs.

Expected values come from the registry itself; the generator's --check covers
exact output drift. Only policy (who may write, which models are banned) is
pinned here.
"""

import copy
import importlib.machinery
import importlib.util
import json
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
CODEX_AGENT_ROOT = REPO_ROOT / "config/codex/agents/generated"
CLAUDE_AGENT_ROOT = REPO_ROOT / "config/claude/agents/generated"
CLAUDE_SETTINGS = REPO_ROOT / "config/claude/settings.json"
MATRIX = REPO_ROOT / "docs/agent-routing-matrix.md"
SHARED_AGENTS = REPO_ROOT / "config/ai/AGENTS.md"
ROOT_AGENTS = REPO_ROOT / "AGENTS.md"
CONFIG_TOML = REPO_ROOT / "config/codex/config.toml"
SKILL_ROOT = REPO_ROOT / "config/agents/skills/multi-agent-team"
XDG_NIX = REPO_ROOT / "nix/home-manager/config/xdg.nix"
PI_NIX = REPO_ROOT / "nix/home-manager/config/apps/pi.nix"

# Policy: only the implementer may write.
ROLE_SANDBOX = {
    "explorer": "read-only",
    "reviewer": "read-only",
    "worker": "workspace-write",
}
# Policy: never route Claude work to Haiku.
BANNED_CLAUDE_MODELS = {"haiku"}


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


def claude_frontmatter(path: pathlib.Path) -> tuple[dict, str]:
    _, frontmatter, body = path.read_text().split("---\n", 2)
    return dict(line.split(": ", 1) for line in frontmatter.strip().splitlines()), body


class AgentRoutingTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.registry = load_registry()
        cls.generator = load_generator_module()
        cls.roles = cls.registry["roles"]
        cls.canonical = {role["canonical_name"] for role in cls.roles.values()}

    def route(self, runtime_name: str, role_name: str) -> dict:
        tier = self.roles[role_name]["tier"]
        return self.registry["runtimes"][runtime_name]["tiers"][tier]

    def test_generated_outputs_are_current(self):
        result = subprocess.run(
            ["python3", str(ROUTING_GENERATOR), "--check"],
            cwd=REPO_ROOT,
            capture_output=True,
            text=True,
            check=False,
        )
        self.assertEqual(result.returncode, 0, result.stderr)

    def test_only_the_implementer_writes(self):
        self.assertEqual(
            {name: role["sandbox_mode"] for name, role in self.roles.items()},
            ROLE_SANDBOX,
        )

    def test_codex_agents_follow_registry(self):
        for profile_name, profile in self.registry["profiles"].items():
            runtime_name = profile["codex_runtime"]
            directory = CODEX_AGENT_ROOT / profile_name
            self.assertEqual({path.stem for path in directory.glob("*.toml")}, self.canonical)
            for role_name, role in self.roles.items():
                with (directory / f"{role['canonical_name']}.toml").open("rb") as agent_file:
                    agent = tomllib.load(agent_file)
                self.assertEqual(agent["model"], self.route(runtime_name, role_name)["model"])
                self.assertEqual(agent["model_reasoning_effort"], role["effort"])
                self.assertEqual(agent["sandbox_mode"], ROLE_SANDBOX[role_name])
                self.assertIn("parent lead", agent["developer_instructions"])
                if ROLE_SANDBOX[role_name] == "read-only":
                    self.assertIn("couldn't confirm", agent["developer_instructions"])

    def test_claude_agents_follow_registry(self):
        for profile_name, profile in self.registry["profiles"].items():
            directory = CLAUDE_AGENT_ROOT / profile_name
            runtime_name = profile.get("claude_runtime")
            if runtime_name is None:
                self.assertFalse(directory.exists(), profile_name)
                continue
            models = {route["model"] for route in self.registry["runtimes"][runtime_name]["tiers"].values()}
            self.assertFalse(models & BANNED_CLAUDE_MODELS)
            self.assertEqual({path.stem for path in directory.glob("*.md")}, self.canonical)
            for role_name, role in self.roles.items():
                fields, body = claude_frontmatter(directory / f"{role['canonical_name']}.md")
                self.assertEqual(fields["model"], self.route(runtime_name, role_name)["model"])
                self.assertEqual(fields["effort"], role["effort"])
                if ROLE_SANDBOX[role_name] == "read-only":
                    self.assertEqual(fields["disallowedTools"], "Edit, Write, NotebookEdit")
                else:
                    self.assertNotIn("disallowedTools", fields)
                self.assertEqual(
                    fields.get("omitClaudeMd") == "true",
                    role.get("claude_omit_claude_md", False),
                )
                self.assertIn("parent lead", body)

    def test_claude_settings_match_registry(self):
        settings = json.loads(CLAUDE_SETTINGS.read_text())
        env = settings["env"]
        for profile in self.registry["profiles"].values():
            runtime_name = profile.get("claude_runtime")
            if runtime_name:
                runtime = self.registry["runtimes"][runtime_name]
                default = runtime["tiers"][runtime["default_tier"]]["model"]
                self.assertEqual(env["CLAUDE_CODE_SUBAGENT_MODEL"], default)
                self.assertEqual(settings["model"], runtime["tiers"][runtime["lead_tier"]]["model"])
                self.assertEqual(settings["effortLevel"], profile["claude_lead_effort"])
                for tier in runtime["tiers"].values():
                    pin = f"ANTHROPIC_DEFAULT_{tier['model'].upper()}_MODEL"
                    self.assertEqual(env[pin], tier["resolves_to"], pin)
        # A full-ID override would bypass the alias pins above.
        self.assertNotIn("ANTHROPIC_MODEL", env)
        # Same policy as Codex max_depth = 1: subagents cannot spawn descendants.
        self.assertEqual(env["CLAUDE_CODE_MAX_SUBAGENT_SPAWN_DEPTH"], "1")
        # The built-in Explore would compete with spawn-subagent-explore.
        self.assertIn("Agent(Explore)", settings["permissions"]["deny"])

    def test_matrix_covers_every_profile_and_harness(self):
        matrix = MATRIX.read_text()
        for profile_name, profile in self.registry["profiles"].items():
            self.assertIn(f"## {profile_name}", matrix)
        claude_profiles = [p for p in self.registry["profiles"].values() if p.get("claude_runtime")]
        self.assertEqual(matrix.count("| Claude Code ("), len(claude_profiles))
        for name in self.canonical:
            self.assertIn(name, matrix)

    def test_home_tiers_stay_on_home_profiles(self):
        # Home tier layers apply to home machines only, never to the office profile.
        home_profiles = {name for name, p in self.registry["profiles"].items() if p.get("home_runtime")}
        self.assertEqual(home_profiles, {"personal"})
        with (REPO_ROOT / "nix/data/agent-routing.generated.nix").open() as nix_file:
            office_block = nix_file.read().split("office = {", 1)[1].split("personal = {", 1)[0]
        self.assertNotIn("personal_home", office_block)
        self.assertNotIn("personal-home", office_block)

    def test_model_ids_live_only_in_the_registry(self):
        routes = [
            route
            for runtime in self.registry["runtimes"].values()
            for route in runtime["tiers"].values()
        ]
        model_ids = {route["model"] for route in routes}
        model_ids |= {route["resolves_to"] for route in routes if "resolves_to" in route}
        labels = {route["label"] for route in routes}
        instructions = [ROOT_AGENTS, SHARED_AGENTS, *sorted(SKILL_ROOT.rglob("*.md"))]
        for path in [*instructions, CONFIG_TOML, XDG_NIX, PI_NIX]:
            contents = path.read_text()
            for model_id in model_ids:
                if len(model_id) > 5:  # Claude aliases are plain words; labels cover them.
                    self.assertNotIn(model_id, contents, path.name)
        for path in instructions:
            contents = path.read_text()
            for label in labels:
                pattern = rf"(?<![\w-]){re.escape(label)}(?![\w-])"
                self.assertIsNone(re.search(pattern, contents, re.IGNORECASE), f"{path.name}: {label}")

    def test_subagents_cannot_spawn_descendants(self):
        with CONFIG_TOML.open("rb") as config_file:
            agents = tomllib.load(config_file)["agents"]
        self.assertEqual(agents["max_depth"], 1)
        # The default subagent route is injected from the registry at activation.
        self.assertNotIn("default_subagent_model", agents)

    def test_role_names_stay_stable_when_models_change(self):
        before = self.generator.build_outputs(copy.deepcopy(self.registry))
        changed = copy.deepcopy(self.registry)
        for runtime in changed["runtimes"].values():
            for route in runtime["tiers"].values():
                route["model"], route["label"] = "renamed-model", "renamed"
        after = self.generator.build_outputs(changed)
        self.assertEqual(set(before), set(after))

    def test_home_manager_links_generated_agents(self):
        xdg = XDG_NIX.read_text()
        self.assertIn("agent-routing.generated.nix", xdg)
        self.assertIn("config/codex/agents/generated/${codex.agent_directory}", xdg)
        # Per-file Claude links keep hand-made agents in ~/.claude/agents intact.
        self.assertIn('".claude/agents/${name}.md"', xdg)
        self.assertNotIn('".claude/agents".source', xdg)
        self.assertIn("routing.profiles.${profile}.pi", PI_NIX.read_text())

    def test_no_handwritten_codex_agent_files(self):
        self.assertEqual(list((REPO_ROOT / "config/codex/agents").glob("*.toml")), [])


class RegistryValidationTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.source = REGISTRY.read_text()
        cls.generator = load_generator_module()

    def assert_rejected(self, mutated: str, message: str):
        with tempfile.TemporaryDirectory() as temp_dir:
            path = pathlib.Path(temp_dir) / "agent-routing.toml"
            path.write_text(mutated)
            with mock.patch.object(self.generator, "REGISTRY_PATH", path):
                with self.assertRaisesRegex(self.generator.RoutingError, message):
                    self.generator.read_registry()

    def test_rejects_invalid_registries(self):
        cases = (
            ('provider = "codex"', 'provider = "unknown"', "provider is invalid"),
            ('tier = "economy"', 'tier = "unknown"', "not a known tier"),
            ('effort = "xhigh"', 'effort = "impossible"', "not a known effort"),
            ('tiers = ["economy", "balanced", "frontier"]', 'tiers = ["economy", "balanced"]', "tiers must be exactly"),
            ("[roles.explorer]", "[roles.unexpected]", "roles must be exactly"),
            ("[runtimes.personal_opencode_go]", "[runtimes.unexpected]", "runtimes must be exactly"),
            ('claude_runtime = "office_claude"', 'claude_runtime = "office_codex"', "must use the claude provider"),
            ("claude_omit_claude_md = true", 'claude_omit_claude_md = "yes"', "must be a boolean"),
            ('resolves_to = "claude-sonnet-5"', 'resolves_to = "sonnet-5"', "needs a claude-\\* resolves_to"),
            ('claude_lead_effort = "high"', 'claude_lead_effort = "loud"', "claude_lead_effort is invalid"),
            ('home_runtime = "personal_home"', 'home_runtime = "personal_opencode_go"', "must use the neutral provider"),
            ("\nverified = false\n", '\nverified = "no"\n', "verified must be a boolean"),
        )
        for old, new, message in cases:
            with self.subTest(message=message):
                self.assertIn(old, self.source)
                self.assert_rejected(self.source.replace(old, new, 1), message)
        self.assert_rejected(self.source + "\n[runtimes.office_codex]\n", "cannot read")

    def test_rejects_unsupported_efforts(self):
        registry = load_registry()
        registry["runtimes"]["office_codex"]["tiers"]["frontier"]["supported_efforts"] = ["low"]
        with self.assertRaisesRegex(self.generator.RoutingError, "does not support requested effort"):
            self.generator.render_nix(registry)


if __name__ == "__main__":
    unittest.main()
