#!/usr/bin/env python3
"""Regression tests for Codex instruction and skill context budgets."""

import importlib.machinery
import importlib.util
import json
import pathlib
import re
import stat
import tempfile
import tomllib
import unittest
from types import SimpleNamespace
from unittest import mock


REPO_ROOT = pathlib.Path(__file__).resolve().parents[1]
SHARED_AGENTS = REPO_ROOT / "config/ai/AGENTS.md"
ROOT_AGENTS = REPO_ROOT / "AGENTS.md"
SKILL_ROOT = REPO_ROOT / "config/agents/skills/multi-agent-team"
SHARED_CONFIG = REPO_ROOT / "config/codex/config.toml"
PERSONAL_CONFIG = REPO_ROOT / "config/codex/profiles/personal.toml"
AUDIT_SCRIPT = REPO_ROOT / "bin/files/codex-context-audit"
JUSTFILE = REPO_ROOT / "justfile"
XDG_NIX = REPO_ROOT / "nix/home-manager/config/xdg.nix"

SKILL_BUDGETS = {
    "command-discipline": 1800,
    "scratch-log": 2000,
    "codex-config-maintenance": 3500,
    "task-coordinator": 6000,
}
GLOBAL_DISABLED = {
    "~/.agents/skills/mattpocock-skills/deprecated/design-an-interface/SKILL.md",
    "~/.agents/skills/mattpocock-skills/deprecated/qa/SKILL.md",
    "~/.agents/skills/mattpocock-skills/deprecated/request-refactor-plan/SKILL.md",
    "~/.agents/skills/mattpocock-skills/deprecated/ubiquitous-language/SKILL.md",
    "~/.agents/skills/mattpocock-skills/in-progress/review/SKILL.md",
    "~/.agents/skills/mattpocock-skills/in-progress/writing-beats/SKILL.md",
    "~/.agents/skills/mattpocock-skills/in-progress/writing-fragments/SKILL.md",
    "~/.agents/skills/mattpocock-skills/in-progress/writing-shape/SKILL.md",
}
INFRA_SKILLS = {
    "1password-op",
    "aurora-migration",
    "aws-account-provisioning",
    "aws-trace-path",
    "bedrock-model-checker",
    "bulk-approve-prs",
    "create-skill",
    "csc-domain-handoff",
    "dsql-cluster-bootstrap",
    "dynamodb",
    "dynamodb-capacity-mode-migration",
    "eks-cluster-bootstrap",
    "github-webhook-enablement",
    "incident-review-notes",
    "jira",
    "okta-aws-role-bootstrap",
    "pg-connection-killer",
    "pup",
    "rds-get-master-password",
    "s3-bucket-check",
    "sdm-db-check",
    "service-quotas",
    "terraform",
    "vector-rollback",
    "wiz-eol-update",
}
DATADOG_SKILLS = {
    "apm-configuration",
    "aws-integration",
    "container-monitoring",
    "dashboards",
    "database-monitoring",
    "dd-apm",
    "dd-debugger",
    "dd-docs",
    "dd-logs",
    "dd-monitors",
    "dd-pup",
    "events",
    "infrastructure",
    "logs",
    "metrics",
    "monitoring-alerting",
    "network-performance",
    "notebooks",
    "synthetics",
    "traces",
}


def load_audit_module():
    loader = importlib.machinery.SourceFileLoader("codex_context_audit", str(AUDIT_SCRIPT))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    if spec is None or spec.loader is None:
        raise RuntimeError("cannot load codex-context-audit")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def frontmatter_description(path: pathlib.Path) -> str:
    match = re.search(r"\A---\n.*?^description:\s*([^\n]+)$.*?^---$", path.read_text(), re.M | re.S)
    if match is None:
        raise AssertionError(f"missing one-line description: {path}")
    return match.group(1).strip()


def implicit_invocation(name: str) -> bool:
    metadata = (SKILL_ROOT / name / "agents/openai.yaml").read_text()
    match = re.search(r"^\s*allow_implicit_invocation:\s*(true|false)\s*$", metadata, re.M)
    if match is None:
        raise AssertionError(f"missing invocation policy for {name}")
    return match.group(1) == "true"


class CodexContextPolicyTest(unittest.TestCase):
    def test_always_loaded_instruction_budgets(self):
        self.assertLessEqual(len(SHARED_AGENTS.read_text()), 7000)
        self.assertLessEqual(len(ROOT_AGENTS.read_text()), 6000)
        self.assertIn("Do not reread instruction content already present", SHARED_AGENTS.read_text())
        self.assertNotRegex(SHARED_AGENTS.read_text(), r"(?m)^- `\$[a-z-]+`:")
        self.assertIn("docs/dotfiles-reference.md", ROOT_AGENTS.read_text())

    def test_skill_entrypoint_and_description_budgets(self):
        for name, budget in SKILL_BUDGETS.items():
            self.assertLessEqual(len((SKILL_ROOT / name / "SKILL.md").read_text()), budget, name)
        for path in SKILL_ROOT.glob("*/SKILL.md"):
            self.assertLessEqual(len(frontmatter_description(path)), 220, path.parent.name)

    def test_implicit_invocation_is_selective(self):
        self.assertFalse(implicit_invocation("command-discipline"))
        self.assertFalse(implicit_invocation("scratch-log"))
        self.assertTrue(implicit_invocation("repo-research"))
        self.assertTrue(implicit_invocation("codex-config-maintenance"))
        self.assertTrue(implicit_invocation("task-coordinator"))

        coordinator = re.sub(r"\s+", " ", (SKILL_ROOT / "task-coordinator/SKILL.md").read_text())
        self.assertIn("at least two independent outcomes", coordinator)
        self.assertIn("one tightly coupled implementation outcome", coordinator)
        self.assertIn("one-time status check", coordinator)

    def test_progressive_disclosure_references_exist(self):
        expected = {
            "scratch-log": ("references/template.md",),
            "codex-config-maintenance": (
                "references/config-layering.md",
                "references/validation.md",
            ),
            "task-coordinator": (
                "references/codex-controls.md",
                "references/codex-task-creation.md",
            ),
        }
        for name, references in expected.items():
            entrypoint = (SKILL_ROOT / name / "SKILL.md").read_text()
            for reference in references:
                self.assertTrue((SKILL_ROOT / name / reference).is_file(), f"{name}/{reference}")
                self.assertIn(reference, entrypoint)

    def test_global_catalog_curation_is_exact(self):
        with SHARED_CONFIG.open("rb") as config_file:
            config = tomllib.load(config_file)
        disabled = {
            item["path"]
            for item in config["skills"]["config"]
            if item.get("enabled") is False
        }
        self.assertEqual(disabled, GLOBAL_DISABLED)
        self.assertNotIn("max_context_tokens", config["skills"])

    def test_personal_profile_disables_office_skill_packs(self):
        with PERSONAL_CONFIG.open("rb") as config_file:
            config = tomllib.load(config_file)
        disabled = {
            item["path"]
            for item in config["skills"]["config"]
            if item.get("enabled") is False
        }
        expected = {
            *(f"~/.agents/skills/infra-skills/{name}/SKILL.md" for name in INFRA_SKILLS),
            *(f"~/.agents/skills/datadog/{name}/SKILL.md" for name in DATADOG_SKILLS),
            "~/.agents/skills/investigate/SKILL.md",
        }
        self.assertEqual(disabled, expected)

    def test_audit_is_executable_and_wired_into_home_manager(self):
        self.assertTrue(AUDIT_SCRIPT.stat().st_mode & stat.S_IXUSR)
        self.assertIn('"codex-context-audit"', XDG_NIX.read_text())
        self.assertIn("codex-context-audit:", JUSTFILE.read_text())
        self.assertIn("codex-context-test", JUSTFILE.read_text())

    def test_profile_composition_keeps_shared_config_before_fragment(self):
        xdg = XDG_NIX.read_text()
        self.assertIn('codexProfileFile = if profile == "office" then "work.local.toml" else "personal.toml"', xdg)
        self.assertLess(xdg.index('cat "$SHARED"'), xdg.index('cat "$PROFILE_FILE"'))


class CodexContextAuditTest(unittest.TestCase):
    def test_summarizes_sizes_without_returning_prompt_content(self):
        module = load_audit_module()
        secret = "private-prompt-content"
        catalog = "\n".join(
            (
                "<skills_instructions>",
                "- one: First skill. (file: r1/multi-agent-team/one/SKILL.md)",
                "- two: Second skill. (file: r1/datadog/two/SKILL.md)",
                "</skills_instructions>",
            )
        )
        agents = f"# AGENTS.md instructions for /tmp/project\n{secret}"
        messages = [
            {"content": [{"type": "input_text", "text": catalog}]},
            {"content": [{"type": "input_text", "text": agents}]},
        ]

        summary = module.summarize_prompt(messages, "/tmp/project")
        rendered = json.dumps(summary)

        self.assertEqual(summary["skills"]["count"], 2)
        self.assertEqual(summary["agents"]["chars"], len(agents))
        self.assertEqual(summary["combined_chars"], len(catalog) + len(agents))
        self.assertNotIn(secret, rendered)
        self.assertEqual(
            {group["name"] for group in summary["skills"]["groups"]},
            {"multi-agent-team", "datadog"},
        )

    def test_rejects_incomplete_prompt_input(self):
        module = load_audit_module()
        with self.assertRaisesRegex(ValueError, "both skill and AGENTS"):
            module.summarize_prompt([], "/tmp/project")

    def test_rejects_malformed_json_shapes_without_tracebacks(self):
        module = load_audit_module()
        malformed = (
            {},
            "bad",
            ["bad"],
            [{"content": "bad"}],
            [{"content": ["bad"]}],
            [{"content": [{"text": 1}]}],
        )
        for prompt in malformed:
            with self.subTest(prompt=prompt), self.assertRaises(ValueError):
                module.summarize_prompt(prompt, "/tmp/project")

    def test_codex_failure_does_not_echo_stderr(self):
        module = load_audit_module()
        secret = "private-codex-error-output"
        result = SimpleNamespace(returncode=9, stderr=secret, stdout="")
        with mock.patch.object(module.subprocess, "run", return_value=result):
            with self.assertRaises(RuntimeError) as raised:
                module.load_prompt(None)

        self.assertIn("exit 9", str(raised.exception))
        self.assertNotIn(secret, str(raised.exception))


if __name__ == "__main__":
    unittest.main()
