#!/usr/bin/env python3
"""Regression tests for the current-Codex-model resolver."""

import json
import os
import pathlib
import subprocess
import tempfile
import unittest


REPO_ROOT = pathlib.Path(__file__).resolve().parents[1]
SCRIPT = REPO_ROOT / "bin/files/codex-current-model"
THREAD_ID = "01a0010d-caac-70c0-bf7a-972f1cfc70ef"


class CodexCurrentModelTest(unittest.TestCase):
    def run_resolver(self, session_dir: pathlib.Path, thread_id: str = THREAD_ID):
        environment = os.environ | {"CODEX_THREAD_ID": thread_id}
        return subprocess.run(
            [str(SCRIPT), "--session-dir", str(session_dir), "--json"],
            check=False,
            capture_output=True,
            env=environment,
            text=True,
        )

    def test_reports_the_latest_turn_context_for_the_current_thread(self):
        with tempfile.TemporaryDirectory() as temporary_directory:
            session_dir = pathlib.Path(temporary_directory)
            session_path = session_dir / f"rollout-2026-08-14T09-14-36-{THREAD_ID}.jsonl"
            records = [
                {"type": "session_meta", "payload": {"id": THREAD_ID}},
                {"type": "response_item", "payload": {"text": "must not be read"}},
                {
                    "type": "turn_context",
                    "timestamp": "2026-08-14T16:14:36Z",
                    "payload": {"model": "gpt-5.6-luna", "effort": "high"},
                },
                {
                    "type": "turn_context",
                    "timestamp": "2026-08-14T16:15:36Z",
                    "payload": {"model": "gpt-5.6-terra", "effort": "xhigh"},
                },
            ]
            session_path.write_text("\n".join(json.dumps(record) for record in records) + "\n")

            result = self.run_resolver(session_dir)

            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(
                json.loads(result.stdout),
                {
                    "thread_id": THREAD_ID,
                    "model": "gpt-5.6-terra",
                    "effort": "xhigh",
                    "timestamp": "2026-08-14T16:15:36Z",
                },
            )

    def test_fails_when_the_current_thread_has_no_session(self):
        with tempfile.TemporaryDirectory() as temporary_directory:
            result = self.run_resolver(pathlib.Path(temporary_directory))

            self.assertNotEqual(result.returncode, 0)
            self.assertIn("no session files found", result.stderr)


if __name__ == "__main__":
    unittest.main()
