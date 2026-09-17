#!/usr/bin/env python3
"""Routing audit regressions; never read real Codex sessions."""

import json
import pathlib
import subprocess
import tempfile
import unittest


REPO_ROOT = pathlib.Path(__file__).resolve().parents[1]
SCRIPT = REPO_ROOT / "bin/files/codex-model-usage"
NOW = "2026-09-17T12:00:00Z"


def context(turn_id, model="gpt-5.6-sol", effort="high", **overrides):
    record = {
        "timestamp": "2026-09-17T10:00:00Z",
        "type": "turn_context",
        "payload": {
            "turn_id": turn_id,
            "model": model,
            "effort": effort,
            "cwd": "/work/project",
        },
    }
    record["timestamp"] = overrides.pop("timestamp", record["timestamp"])
    record["payload"].update(overrides)
    return record


class CodexModelUsageTest(unittest.TestCase):
    def setUp(self):
        temporary = tempfile.TemporaryDirectory()
        self.addCleanup(temporary.cleanup)
        self.directory = pathlib.Path(temporary.name)
        self.sessions = self.directory / "sessions"
        self.archive = self.directory / "archive"
        self.sessions.mkdir()
        self.archive.mkdir()

    def write_records(self, records, path=None, compact=False):
        path = path or self.sessions / "rollout-2026-09-17T00-00-00-test.jsonl"
        options = {"separators": (",", ":")} if compact else {}
        path.write_text("\n".join(json.dumps(row, **options) for row in records) + "\n")
        return path

    def run_audit(self, *extra, roots=("/work", "/dotfiles"), output="--json"):
        command = [
            str(SCRIPT), "--now", NOW,
            "--session-dir", str(self.sessions),
            "--session-dir", str(self.archive),
        ]
        for root in roots:
            command.extend(["--root", root])
        if output:
            command.append(output)
        return subprocess.run(command + list(extra), capture_output=True, text=True, check=False)

    def report(self, *extra, **options):
        result = self.run_audit(*extra, **options)
        self.assertEqual(result.returncode, 0, result.stderr)
        return json.loads(result.stdout)

    def test_sample_report_exposes_opposing_premium_and_sol_shifts(self):
        records = []

        def add(model, effort, count, timestamp):
            for _ in range(count):
                records.append(context(str(len(records)), f"gpt-5.6-{model}", effort, timestamp=timestamp))

        daily = "2026-09-17T10:00:00Z"
        weekly = "2026-09-14T10:00:00Z"
        previous = "2026-09-07T10:00:00Z"
        for model, effort, count in [
            ("luna", "xhigh", 4), ("sol", "high", 20), ("sol", "xhigh", 3),
            ("terra", "high", 56), ("terra", "medium", 8), ("terra", "xhigh", 4),
        ]:
            add(model, effort, count, daily)
        for model, effort, count in [
            ("luna", "high", 18), ("luna", "medium", 7),
            ("sol", "high", 108), ("sol", "medium", 17), ("terra", "high", 124),
        ]:
            add(model, effort, count, weekly)
        dotfiles = {"gpt-5.6-luna": 1, "gpt-5.6-sol": 26, "gpt-5.6-terra": 5}
        for row in records:
            model = row["payload"]["model"]
            if dotfiles[model]:
                row["payload"]["cwd"] = "/dotfiles"
                dotfiles[model] -= 1
        for model, count in [("luna", 13), ("sol", 66), ("terra", 37)]:
            add(model, "high", count, previous)
        self.write_records(records)

        report = self.report()
        current = report["windows"]["current_7_days"]
        self.assertEqual(current["total_turns"], 369)
        self.assertEqual(current["luna_share_pct"], 7.9)
        self.assertEqual(current["terra_share_pct"], 52.0)
        self.assertEqual(current["sol_share_pct"], 40.1)
        self.assertEqual(current["premium_model_share_pct"], 92.1)
        self.assertEqual(current["luna_value_turns"], 22)
        self.assertEqual(current["luna_utility_turns"], 7)
        self.assertEqual(current["sol_quality_turns"], 131)
        self.assertEqual(current["sol_below_high_turns"], 17)
        self.assertEqual(report["trend"]["premium_model_share_delta_percentage_points"], 3.3)
        self.assertEqual(report["trend"]["sol_share_delta_percentage_points"], -16.8)
        self.assertTrue(report["trend"]["mixed_routing_signals"])
        self.assertEqual(report["windows"]["last_24_hours"]["total_turns"], 95)
        self.assertEqual(report["by_root"]["/dotfiles"]["windows"]["current_7_days"]["models"], {
            "gpt-5.6-luna": 1, "gpt-5.6-sol": 26, "gpt-5.6-terra": 5,
        })
        summary = "\n".join(report["summary_lines"])
        self.assertEqual(len(summary.splitlines()), 11)
        self.assertIn("gpt-5.6-terra@medium=8", summary)
        self.assertIn("mixed signals", summary)
        self.assertIn("Review candidate: /work has 17 Sol low/medium turns", summary)
        self.assertIn("not billed token or dollar usage", summary)
        self.assertNotIn("\\", summary)
        self.assertEqual(self.run_audit(output="--summary").stdout, summary + "\n")

    def test_window_boundaries_future_records_and_resumed_old_sessions(self):
        records = [
            context("now", timestamp=NOW),
            context("daily-start", timestamp="2026-09-16T12:00:00Z"),
            context("before-daily", timestamp="2026-09-16T11:59:59Z"),
            context("current-start", timestamp="2026-09-10T12:00:00Z"),
            context("before-current", timestamp="2026-09-10T11:59:59Z"),
            context("previous-start", timestamp="2026-09-03T12:00:00Z"),
            context("too-old", timestamp="2026-09-03T11:59:59Z"),
            context("future", timestamp="2026-09-17T12:00:01Z"),
        ]
        self.write_records(records, self.sessions / "rollout-2025-01-01T00-00-00-resumed.jsonl")
        report = self.report()
        self.assertEqual([report["windows"][key]["total_turns"] for key in (
            "last_24_hours", "current_7_days", "previous_7_days",
        )], [2, 4, 2])
        self.assertEqual(report["window_bounds"]["previous_7_days"], {
            "start_inclusive": "2026-09-03T12:00:00Z",
            "end_exclusive": "2026-09-10T12:00:00Z",
        })

    def test_archived_duplicates_keep_earliest_context_and_do_not_write(self):
        later = context("same", model="gpt-5.6-terra")
        earlier = context("same", model="gpt-5.6-luna", timestamp="2026-09-10T11:00:00Z")
        self.write_records([later], self.archive / "a.jsonl")
        self.write_records([earlier, earlier], compact=True)
        before = {path: path.read_bytes() for path in self.directory.rglob("*") if path.is_file()}
        report = self.report("--session-dir", str(self.sessions))
        self.assertEqual(report["duplicate_turn_contexts"], 2)
        self.assertEqual(report["windows"]["current_7_days"]["total_turns"], 0)
        self.assertEqual(report["windows"]["previous_7_days"]["models"], {"gpt-5.6-luna": 1})
        after = {path: path.read_bytes() for path in self.directory.rglob("*") if path.is_file()}
        self.assertEqual(before, after)

    def test_message_records_are_skipped_and_malformed_metadata_is_reported(self):
        type_first = {"type": "turn_context", **context("type-first")}
        path = self.write_records([context("valid"), type_first])
        with path.open("a") as stream:
            # Intentionally invalid message JSON must be skipped without parsing,
            # even when its payload contains a literal nested turn_context type.
            stream.write('{"type":"response_item","payload":{"type":"turn_context", MESSAGE_SENTINEL}\n')
            stream.write('{"timestamp":"2026-09-17T10:00:00Z","type":"turn_context","payload":null}\n')
            stream.write('{"type":"turn_context","payload":INVALID}\n')
            stream.write(json.dumps(context("bad-cwd", cwd=None)) + "\n")
            stream.write(json.dumps(context("bad-effort", effort=[])) + "\n")
            stream.write(json.dumps(context("", model="gpt-5.6-luna")) + "\n")
        result = self.run_audit()
        self.assertEqual(result.returncode, 0, result.stderr)
        report = json.loads(result.stdout)
        self.assertEqual(report["parse_errors"], 5)
        self.assertEqual(report["windows"]["current_7_days"]["total_turns"], 2)
        self.assertNotIn("MESSAGE_SENTINEL", result.stdout + result.stderr)

    def test_ordinal_between_timestamp_and_type_is_counted(self):
        record = context("with-ordinal")
        record = {
            "timestamp": record["timestamp"],
            "ordinal": 42,
            "type": record["type"],
            "payload": record["payload"],
        }
        self.write_records([record], compact=True)

        report = self.report()

        self.assertEqual(report["parse_errors"], 0)
        self.assertEqual(report["windows"]["current_7_days"]["total_turns"], 1)

    def test_unknown_efforts_are_never_counted_as_below_high(self):
        records = [context(str(i), effort=effort) for i, effort in enumerate(
            ["low", "medium", "high", "xhigh", "max", "ultra", None, "", "future"]
        )]
        records += [context(f"luna-{i}", model="gpt-5.6-luna", effort=effort) for i, effort in enumerate(
            ["low", "medium", "high", "xhigh", "max", "ultra", None]
        )]
        self.write_records(records)
        current = self.report()["windows"]["current_7_days"]
        self.assertEqual(current["sol_below_high_turns"], 2)
        self.assertEqual(current["sol_quality_turns"], 4)
        self.assertEqual(current["sol_unclassified_effort_turns"], 3)
        self.assertEqual(current["luna_value_turns"], 3)
        self.assertEqual(current["luna_utility_turns"], 2)
        self.assertEqual(current["luna_unclassified_effort_turns"], 2)

    def test_all_models_are_reported_but_shares_use_only_gpt56(self):
        self.write_records([
            context("sol"), context("luna", model="gpt-5.6-luna"),
            context("astra", model="gpt-6-astra"), context("deepseek", model="deepseek-v4.1-flash"),
            context("review", model="codex-auto-review"),
        ])
        report = self.report()
        current = report["windows"]["current_7_days"]
        self.assertEqual(current["total_turns"], 4)
        self.assertEqual(current["gpt_5_6_turns"], 2)
        self.assertEqual(current["other_model_turns"], 2)
        self.assertEqual(current["premium_model_share_pct"], 50.0)
        self.assertIn("gpt-6-astra", current["models"])
        self.assertIn("deepseek-v4.1-flash", "\n".join(report["summary_lines"]))

    def test_longest_root_matching_path_boundaries_and_filesystem_root(self):
        self.write_records([context(str(i), cwd=cwd) for i, cwd in enumerate([
            "/", "/work", "/work/", "/work/project", "/work/project/nested", "/work-other",
        ])])
        roots = ("/", "/work/", "/work/project", "/work")
        report = self.report(roots=roots)
        self.assertEqual(report["windows"]["current_7_days"]["total_turns"], 6)
        for root in ("/", "/work", "/work/project"):
            self.assertEqual(report["by_root"][root]["windows"]["current_7_days"]["total_turns"], 2)
        self.assertLess(len(report["summary_lines"]), 12)
        self.assertEqual(self.report()["windows"]["current_7_days"]["total_turns"], 4)

    def test_no_gpt56_baseline_does_not_invent_shares_or_direction(self):
        self.write_records([context("other", model="deepseek-v4.1-flash")])
        report = self.report()
        self.assertIsNone(report["windows"]["current_7_days"]["premium_model_share_pct"])
        self.assertIsNone(report["trend"]["premium_model_share_delta_percentage_points"])
        self.assertEqual(report["trend"]["status"], "insufficient_baseline")
        self.assertFalse(report["trend"]["mixed_routing_signals"])

    def test_empty_and_missing_session_directories_are_distinct(self):
        report = self.report("--session-dir", str(self.directory / "missing"))
        self.assertEqual(report["windows"]["current_7_days"]["total_turns"], 0)
        self.assertEqual(len(report["missing_session_dirs"]), 1)
        self.assertIn("missing session dirs: 1", report["summary_lines"][0])
        self.sessions.rmdir()
        self.archive.rmdir()
        result = self.run_audit()
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "")
        self.assertIn("no session directories exist", result.stderr)

    def test_cheaper_flat_and_costlier_routing_directions(self):
        for previous, current, status, delta in [
            ("sol", "luna", "cheaper_mix", -100.0),
            ("terra", "sol", "flat", 0.0),
            ("luna", "sol", "costlier_mix", 100.0),
        ]:
            with self.subTest(status=status):
                self.write_records([
                    context("previous", model=f"gpt-5.6-{previous}", timestamp="2026-09-07T10:00:00Z"),
                    context("current", model=f"gpt-5.6-{current}"),
                ])
                trend = self.report()["trend"]
                self.assertEqual(trend["status"], status)
                self.assertEqual(trend["premium_model_share_delta_percentage_points"], delta)
                self.assertFalse(trend["mixed_routing_signals"])


if __name__ == "__main__":
    unittest.main()
