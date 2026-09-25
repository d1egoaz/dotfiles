#!/usr/bin/env python3
"""Regression tests for Codex instruction and skill context budgets."""

import importlib.machinery
import importlib.util
import json
import pathlib
import re
import stat
import unittest
from types import SimpleNamespace
from unittest import mock


REPO_ROOT = pathlib.Path(__file__).resolve().parents[1]
SHARED_AGENTS = REPO_ROOT / "config/ai/AGENTS.md"
ROOT_AGENTS = REPO_ROOT / "AGENTS.md"
SKILL_ROOT = REPO_ROOT / "config/agents/skills/multi-agent-team"
AUDIT_SCRIPT = REPO_ROOT / "bin/files/codex-context-audit"
JUSTFILE = REPO_ROOT / "justfile"
XDG_NIX = REPO_ROOT / "nix/home-manager/config/xdg.nix"
CODEX_RULES = REPO_ROOT / "config/codex/rules/10-shared.rules"

# Common spellings of destructive commands that must ask, even under a broader
# allow rule. Prefix rules cannot catch every form; see the rules file comment.
DESTRUCTIVE_PREFIXES = (
    ("git", "push", "--force"),
    ("git", "push", "-f"),
    ("git", "push", "--force-with-lease"),
    ("git", "reset", "--hard"),
    ("git", "clean"),
    ("git", "branch", "-D"),
    ("git", "worktree", "remove", "--force"),
    ("rm", "-rf"),
    ("rm", "-fr"),
)

# One budget for every always-discoverable SKILL.md; descriptions load every turn.
SKILLS_TOTAL_BUDGET = 12000
DESCRIPTION_BUDGET = 220

# Safety gates that must survive any rewrite. Wording elsewhere is free to change.
GATES = {
    SHARED_AGENTS: (
        "Production is read-only unless the user authorizes the exact mutation",
        "Confirm before destructive changes",
        "Never bypass or disable commit signing",
        "Assisted-by: [Exact model identifier] via [Tool]",
        "Open new PRs in draft mode",
        "Keep primary checkouts on `main`",
    ),
    SKILL_ROOT / "signed-pr-publish/SKILL.md": (
        "git commit -S",
        "Assisted-by: [Exact model identifier] via [Tool]",
        "Open new PRs as drafts",
        "Do not infer push, PR, ready-for-review, merge, deployment",
        "run one independent review pass",
    ),
    SKILL_ROOT / "git-worktree-flow/SKILL.md": (
        "--no-track",
        "Never remove a worktree, delete a branch, or reset",
    ),
    SKILL_ROOT / "tfctl/SKILL.md": (
        "Every delete and production mutation requires direct current-task approval",
    ),
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
        shared_agents = SHARED_AGENTS.read_text()
        self.assertLessEqual(len(shared_agents), 5000)
        self.assertLessEqual(len(ROOT_AGENTS.read_text()), 6000)
        self.assertNotRegex(shared_agents, r"(?m)^- `\$[a-z-]+`:")
        self.assertIn("docs/dotfiles-reference.md", ROOT_AGENTS.read_text())

    def test_skill_budgets(self):
        skills = sorted(SKILL_ROOT.glob("*/SKILL.md"))
        self.assertLessEqual(sum(len(path.read_text()) for path in skills), SKILLS_TOTAL_BUDGET)
        for path in skills:
            self.assertLessEqual(len(frontmatter_description(path)), DESCRIPTION_BUDGET, path.parent.name)

    def test_safety_gates_survive(self):
        for path, gates in GATES.items():
            normalized = " ".join(path.read_text().split())
            for gate in gates:
                self.assertIn(gate, normalized, f"{path.relative_to(REPO_ROOT)}: {gate}")

    def test_destructive_commands_always_prompt(self):
        rules = {}
        for pattern, decision in re.findall(
            r'prefix_rule\(pattern=\[([^\]]*)\], decision="(\w+)"\)', CODEX_RULES.read_text()
        ):
            rules.setdefault(tuple(re.findall(r'"([^"]*)"', pattern)), set()).add(decision)
        for prefix in DESTRUCTIVE_PREFIXES:
            self.assertEqual(rules.get(prefix), {"prompt"}, " ".join(prefix))

    def test_scratch_log_stays_explicit(self):
        # It writes files, so it must never trigger on its own.
        self.assertFalse(implicit_invocation("scratch-log"))

    def test_skill_references_resolve_and_are_linked(self):
        for skill in sorted(path.parent for path in SKILL_ROOT.glob("*/SKILL.md")):
            entrypoint = (skill / "SKILL.md").read_text()
            linked = set(re.findall(r"references/[\w.-]+\.md", entrypoint))
            present = {f"references/{path.name}" for path in (skill / "references").glob("*.md")}
            self.assertEqual(linked, present, skill.name)

    def test_audit_is_executable_and_wired_into_home_manager(self):
        self.assertTrue(AUDIT_SCRIPT.stat().st_mode & stat.S_IXUSR)
        self.assertIn('"codex-context-audit"', XDG_NIX.read_text())
        self.assertIn("codex-context-audit:", JUSTFILE.read_text())
        self.assertIn("codex-context-test", JUSTFILE.read_text())

    def test_profile_composition_keeps_shared_config_before_fragment(self):
        xdg = XDG_NIX.read_text()
        self.assertIn('profileFile = if profile == "office" then "work.local.toml" else "personal.toml";', xdg)
        self.assertIn('profile = if profile == "office" then "work" else "personal";', xdg)
        self.assertLess(xdg.index('"$SHARED"'), xdg.index('cat "$PROFILE_FILE"'))


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
