#!/usr/bin/env python3
"""Tests for the shared Stop hook and the agent audit, with synthetic transcripts only."""

import json
import os
import pathlib
import subprocess
import tempfile
import unittest


REPO_ROOT = pathlib.Path(__file__).resolve().parents[1]
HOOK = REPO_ROOT / "bin/files/agent-closeout-check"
AUDIT = REPO_ROOT / "bin/files/agent-audit"
SECRET = "private-transcript-content"


def write_jsonl(path: pathlib.Path, records: list[dict]) -> pathlib.Path:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("".join(json.dumps(record) + "\n" for record in records))
    return path


def claude_prompt(text: str = SECRET) -> dict:
    return {"type": "user", "message": {"role": "user", "content": text}}


def claude_tools(*names: str) -> dict:
    blocks = [{"type": "tool_use", "id": f"t{i}", "name": name, "input": {}} for i, name in enumerate(names)]
    return {"type": "assistant", "message": {"role": "assistant", "content": blocks}}


def codex_items(turn_id: str, *kinds: str) -> list[dict]:
    return [
        {"type": "event_msg", "payload": {"type": "item_completed", "turn_id": turn_id, "item": {"type": kind}}}
        for kind in kinds
    ]


def codex_thread(source, turns: list[tuple[str, list[dict]]]) -> list[dict]:
    records = [{"type": "session_meta", "payload": {"id": "thread-1", "source": source}}]
    for turn_id, body in turns:
        records.append({"type": "event_msg", "payload": {"type": "task_started", "turn_id": turn_id}})
        records.extend(body)
    return records


class CloseoutHookTest(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory()
        self.root = pathlib.Path(self.temp.name)

    def tearDown(self):
        self.temp.cleanup()

    def run_hook(self, harness: str, payload) -> subprocess.CompletedProcess:
        stdin = payload if isinstance(payload, str) else json.dumps(payload)
        return subprocess.run([str(HOOK), harness], input=stdin, capture_output=True, text=True, timeout=10)

    def claude(self, records, message="Done.", **extra):
        path = write_jsonl(self.root / "claude.jsonl", records)
        return self.run_hook("claude", {"transcript_path": str(path), "last_assistant_message": message, **extra})

    def assert_quiet(self, result):
        self.assertEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "")

    def assert_blocks(self, result, why: str):
        self.assertEqual(result.returncode, 0)
        decision = json.loads(result.stdout)
        self.assertEqual(decision["decision"], "block")
        self.assertIn(why, decision["reason"])

    def test_claude_edit_without_sections_blocks(self):
        self.assert_blocks(self.claude([claude_prompt(), claude_tools("Read", "Edit")]), "edited files")

    def test_claude_spawn_and_tool_count_are_substantial(self):
        self.assert_blocks(self.claude([claude_prompt(), claude_tools("Agent")]), "spawned a subagent")
        self.assert_blocks(self.claude([claude_prompt(), claude_tools(*["Bash"] * 10)]), "made 10 tool calls")

    def test_claude_closing_sections_pass(self):
        for heading in ("**Changed**", "## Found", "**Blocked on me**"):
            with self.subTest(heading=heading):
                result = self.claude([claude_prompt(), claude_tools("Edit")], message=f"Summary.\n\n{heading}\n- item")
                self.assert_quiet(result)

    def test_claude_small_turn_and_earlier_turns_stay_quiet(self):
        records = [claude_prompt(), claude_tools("Edit", "Agent"), claude_prompt(), claude_tools("Read", "Read")]
        self.assert_quiet(self.claude(records))

    def test_claude_tool_results_and_meta_do_not_reset_the_turn(self):
        tool_result = {
            "type": "user",
            "toolUseResult": {},
            "message": {"role": "user", "content": [{"type": "tool_result", "tool_use_id": "t0"}]},
        }
        meta = {"type": "user", "isMeta": True, "message": {"role": "user", "content": "skill body"}}
        records = [claude_prompt(), claude_tools("Edit"), tool_result, meta, claude_tools("Read")]
        self.assert_blocks(self.claude(records), "edited files")

    def test_stop_hook_active_never_blocks_twice(self):
        self.assert_quiet(self.claude([claude_prompt(), claude_tools("Edit")], stop_hook_active=True))

    def test_codex_turn_scope_and_subagent_skip(self):
        turns = [("t1", codex_items("t1", "FileChange")), ("t2", codex_items("t2", "CommandExecution"))]
        path = write_jsonl(self.root / "codex.jsonl", codex_thread("vscode", turns))
        base = {"transcript_path": str(path), "last_assistant_message": "ok", "stop_hook_active": False}
        self.assert_quiet(self.run_hook("codex", {**base, "turn_id": "t2"}))
        self.assert_blocks(self.run_hook("codex", {**base, "turn_id": "t1"}), "edited files")
        spawn = {"type": "response_item", "payload": {"type": "function_call", "name": "spawn_agent", "arguments": "{}"}}
        path = write_jsonl(self.root / "spawn.jsonl", codex_thread("vscode", [("t3", [spawn])]))
        self.assert_blocks(self.run_hook("codex", {**base, "transcript_path": str(path), "turn_id": "t3"}), "spawned")
        child = codex_thread({"subagent": {"thread_spawn": {"depth": 1}}}, turns)
        path = write_jsonl(self.root / "child.jsonl", child)
        self.assert_quiet(self.run_hook("codex", {**base, "transcript_path": str(path), "turn_id": "t1"}))

    def test_bad_input_exits_quietly(self):
        for harness, payload in (("claude", "not json"), ("claude", "[]"), ("other", "{}"), ("codex", "{}")):
            with self.subTest(harness=harness, payload=payload):
                self.assert_quiet(self.run_hook(harness, payload))
        self.assert_quiet(self.run_hook("claude", {"transcript_path": "/nonexistent", "last_assistant_message": "x"}))


class AgentAuditTest(unittest.TestCase):
    def test_scorecard_prints_metadata_but_never_content(self):
        with tempfile.TemporaryDirectory() as home:
            project = pathlib.Path(home) / ".claude/projects/demo"
            spawn = {
                "type": "tool_use",
                "id": "a1",
                "name": "Agent",
                "input": {"subagent_type": "spawn-subagent-gather", "prompt": f"{SECRET} Done means: x. Stop and ask if: y."},
            }
            write_jsonl(
                project / "s1.jsonl",
                [
                    claude_prompt(),
                    {"type": "assistant", "message": {"content": [spawn, {"type": "tool_use", "name": "Edit", "input": {}}]}},
                    {"type": "assistant", "message": {"content": [{"type": "text", "text": f"{SECRET}\n\n**Changed**"}]}},
                ],
            )
            child = project / "s1/subagents/agent-1.jsonl"
            write_jsonl(
                child,
                [{"type": "assistant", "message": {"model": "claude-x", "stop_reason": "tool_use", "content": SECRET}}],
            )
            (child.parent / "agent-1.meta.json").write_text(json.dumps({"agentType": "spawn-subagent-gather", "spawnDepth": 1}))
            codex = pathlib.Path(home) / ".codex/sessions/2026/10/08/rollout.jsonl"
            fork = json.dumps({"agent_type": "spawn-subagent-review", "fork_turns": "all", "message": SECRET})
            write_jsonl(
                codex,
                codex_thread(
                    "vscode",
                    [
                        (
                            "t1",
                            [
                                {"type": "response_item", "payload": {"type": "function_call", "name": "spawn_agent", "arguments": fork}},
                                {"type": "event_msg", "payload": {"type": "task_complete", "last_agent_message": SECRET}},
                            ],
                        )
                    ],
                ),
            )
            env = {**os.environ, "HOME": home}
            result = subprocess.run(
                ["python3", "-I", str(AUDIT), "--profile", "office", "--days", "1"],
                capture_output=True,
                text=True,
                env=env,
                timeout=30,
            )
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertNotIn(SECRET, result.stdout + result.stderr)
        self.assertIn("## Claude Code: 1 lead sessions", result.stdout)
        self.assertIn("claude:spawn-subagent-gather", result.stdout)
        self.assertIn("'claude:pending tool_use': 1", result.stdout)
        self.assertIn("Handoffs with required labels: 1/1", result.stdout)
        self.assertIn("Closing sections on substantial turns: 1/1", result.stdout)
        self.assertIn("Codex fork_turns: {'all': 1}", result.stdout)


class WiringTest(unittest.TestCase):
    def test_scripts_are_executable_linked_and_hooked(self):
        xdg = (REPO_ROOT / "nix/home-manager/config/xdg.nix").read_text()
        for script in (HOOK, AUDIT):
            self.assertTrue(script.stat().st_mode & 0o100, script.name)
            self.assertIn(f'"{script.name}"', xdg)
        claude = json.loads((REPO_ROOT / "config/claude/settings.json").read_text())
        codex = json.loads((REPO_ROOT / "config/codex/hooks.json").read_text())
        for config, harness in ((claude, "claude"), (codex, "codex")):
            commands = [hook["command"] for group in config["hooks"]["Stop"] for hook in group["hooks"]]
            self.assertTrue(any(c.endswith(f"agent-closeout-check\" {harness}") for c in commands), harness)
            self.assertTrue(any("afplay" in c for c in commands), harness)
        self.assertIn("agent-audit *args:", (REPO_ROOT / "justfile").read_text())


if __name__ == "__main__":
    unittest.main()
