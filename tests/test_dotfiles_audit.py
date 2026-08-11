#!/usr/bin/env python3

import importlib.machinery
import importlib.util
import tempfile
import unittest
from pathlib import Path
from unittest import mock


ROOT = Path(__file__).resolve().parents[1]
SCRIPT = ROOT / "bin" / "files" / "dotfiles-audit"
LOADER = importlib.machinery.SourceFileLoader("dotfiles_audit", str(SCRIPT))
SPEC = importlib.util.spec_from_loader(LOADER.name, LOADER)
assert SPEC is not None
MODULE = importlib.util.module_from_spec(SPEC)
LOADER.exec_module(MODULE)


def comparison_snapshot(
    *,
    coverage=None,
    dirty_paths=None,
    missing_casks=None,
    extra_casks=None,
):
    return {
        "nix": {"matches": True},
        "repository": {"dirty_paths": dirty_paths or []},
        "homebrew": {
            "comparison": {
                "missing_formulae": [],
                "missing_casks": missing_casks or [],
                "missing_taps": [],
                "extra_formulae_on_request": [],
                "extra_casks": extra_casks or [],
                "extra_taps": [],
            }
        },
        "macos": {
            "managed_defaults": [],
            "keyboard_mapping": {"status": "match"},
        },
        "coverage": coverage
        or [
            {"component": "repository", "status": "ok"},
            {"component": "nix_system", "status": "ok"},
            {"component": "homebrew", "status": "ok"},
            {"component": "macos_defaults", "status": "ok"},
        ],
    }


class CommandPolicyTest(unittest.TestCase):
    def test_allows_only_expected_read_operations(self):
        allowed = (
            ["brew", "info", "--json=v2", "--installed"],
            ["defaults", "read", "NSGlobalDomain", "_HIHideMenuBar"],
            ["defaults", "-currentHost", "read", "com.apple.controlcenter", "Weather"],
            ["nix", "eval", "--json", ".#example"],
            ["launchctl", "print", "gui/501/example"],
            ["mas", "list"],
        )
        for command in allowed:
            with self.subTest(command=command):
                self.assertTrue(MODULE.command_is_allowed(command))

    def test_blocks_mutating_operations(self):
        blocked = (
            ["brew", "update"],
            ["brew", "upgrade"],
            ["brew", "outdated", "--cask", "--greedy"],
            ["defaults", "write", "NSGlobalDomain", "_HIHideMenuBar", "false"],
            ["nix", "build", ".#example"],
            ["nix", "eval", "--impure", ".#example"],
            ["launchctl", "bootout", "gui/501/example"],
            ["mas", "upgrade"],
        )
        for command in blocked:
            with self.subTest(command=command):
                self.assertFalse(MODULE.command_is_allowed(command))


class ParsingTest(unittest.TestCase):
    def test_parses_brewfiles_without_comments_or_options(self):
        with tempfile.TemporaryDirectory() as directory:
            brewfile = Path(directory) / "Brewfile"
            brewfile.write_text(
                'tap "example/tap" # comment\n'
                'brew "example/tap/tool", restart_service: true\n'
                'cask "example/tap/app"\n'
                'mas "Example", id: 123\n',
                encoding="utf-8",
            )
            result = MODULE.parse_brewfiles([brewfile])
        self.assertEqual(result["taps"], ["example/tap"])
        self.assertEqual(result["formulae"], ["example/tap/tool"])
        self.assertEqual(result["casks"], ["app"])
        self.assertEqual(result["app_store"], ["Example"])

    def test_parses_current_host_defaults(self):
        with tempfile.TemporaryDirectory() as directory:
            source = Path(directory) / "default.nix"
            source.write_text(
                "/usr/bin/defaults -currentHost write com.apple.controlcenter Weather -int 2\n"
                "/usr/bin/defaults -currentHost write com.apple.controlcenter Enabled -bool true\n",
                encoding="utf-8",
            )
            result = MODULE.parse_system_current_host_defaults(source)
        self.assertEqual(
            result,
            [
                {
                    "domain": "com.apple.controlcenter",
                    "key": "Weather",
                    "desired": 2,
                    "scope": "current_host",
                },
                {
                    "domain": "com.apple.controlcenter",
                    "key": "Enabled",
                    "desired": True,
                    "scope": "current_host",
                },
            ],
        )

    def test_declared_current_host_battery_is_managed(self):
        evaluated = {
            "defaults": {
                "dock": {},
                "finder": {},
                "NSGlobalDomain": {},
                "trackpad": {},
                "CustomUserPreferences": {},
            },
            "currentHostDefaults": {
                "com.apple.controlcenter": {
                    "Battery": 18,
                    "BatteryShowPercentage": True,
                }
            },
            "controlCenter": {
                "battery": "always",
                "showBatteryPercentage": True,
            },
            "keyboard": {},
        }
        with tempfile.TemporaryDirectory() as directory:
            records, _ = MODULE.desired_defaults(Path(directory), evaluated)
        self.assertIn(
            {
                "domain": "com.apple.controlcenter",
                "key": "Battery",
                "desired": 18,
                "scope": "current_host",
                "semantic_setting": "battery",
                "desired_state": "always",
            },
            records,
        )

    def test_control_center_summary_uses_semantic_states(self):
        snapshot = comparison_snapshot()
        snapshot["host"] = {
            "darwin_host": "personal-mbp",
            "profile": "personal",
            "os_version": "26.5",
            "architecture": "arm64",
        }
        snapshot["repository"] = {"dirty_paths": []}
        snapshot["nix"] = {"matches": True}
        snapshot["homebrew"] = {
            "comparison": {
                "missing_formulae": [],
                "missing_casks": [],
                "missing_taps": [],
                "extra_formulae_on_request": [],
                "extra_casks": [],
                "extra_taps": [],
            }
        }
        snapshot["macos"] = {
            "managed_defaults": [
                {
                    "domain": "com.apple.controlcenter",
                    "key": "Sound",
                    "semantic_setting": "sound",
                    "desired_state": "always",
                    "actual_state": "always",
                    "status": "match",
                },
                {
                    "domain": "com.apple.controlcenter",
                    "key": "NowPlaying",
                    "semantic_setting": "nowPlaying",
                    "desired_state": "when-active",
                    "status": "missing",
                },
            ],
            "keyboard_mapping": {"status": "match"},
        }
        snapshot["managed_symlinks"] = []
        snapshot["launch_agents"] = []
        snapshot["background_items"] = {"identifiers": []}
        snapshot["comparison"] = {
            "status": "drift",
            "drift_count": 1,
            "baseline_status": "clean",
            "baseline_changed_path_count": 0,
            "coverage_gaps": [],
            "supplemental_coverage_gaps": [],
        }
        lines = MODULE.summary_lines(snapshot)
        self.assertIn(
            "Control Center: sound=always, nowPlaying=missing (expected when-active)",
            lines,
        )

    def test_hidden_control_center_item_is_managed_semantically(self):
        evaluated = {
            "defaults": {
                "dock": {},
                "finder": {},
                "NSGlobalDomain": {},
                "trackpad": {},
                "CustomUserPreferences": {
                    "com.apple.controlcenter": {
                        "NSStatusItem VisibleCC Bluetooth": False
                    }
                },
            },
            "currentHostDefaults": {},
            "controlCenter": {"bluetooth": "hidden"},
            "keyboard": {},
        }
        with tempfile.TemporaryDirectory() as directory:
            records, _ = MODULE.desired_defaults(Path(directory), evaluated)
        self.assertIn(
            {
                "domain": "com.apple.controlcenter",
                "key": "NSStatusItem VisibleCC Bluetooth",
                "desired": False,
                "scope": "user",
                "semantic_setting": "bluetooth",
                "desired_state": "hidden",
            },
            records,
        )

    def test_null_current_host_default_is_unmanaged(self):
        evaluated = {
            "defaults": {
                "dock": {},
                "finder": {},
                "NSGlobalDomain": {},
                "trackpad": {},
                "CustomUserPreferences": {},
            },
            "currentHostDefaults": {
                "com.apple.controlcenter": {"BatteryShowPercentage": None}
            },
            "keyboard": {},
        }
        with tempfile.TemporaryDirectory() as directory:
            records, _ = MODULE.desired_defaults(Path(directory), evaluated)
        self.assertNotIn(
            {
                "domain": "com.apple.controlcenter",
                "key": "BatteryShowPercentage",
                "desired": None,
                "scope": "current_host",
            },
            records,
        )

    def test_normalizes_scalar_defaults_without_type_coercion(self):
        self.assertIs(MODULE.normalize_default("1\n", True), True)
        self.assertEqual(MODULE.normalize_default("2\n", 2), 2)
        self.assertEqual(MODULE.normalize_default("1.0\n", 1.0), 1.0)
        self.assertEqual(MODULE.normalize_default("left\n", "left"), "left")
        self.assertEqual(MODULE.normalize_default(r"\u21e7\u2318J", "⇧⌘J"), "⇧⌘J")
        self.assertFalse(MODULE.values_equal(1, True))

    def test_null_hidutil_mapping_is_an_empty_list(self):
        self.assertEqual(MODULE.normalize_hidutil_output("(null)\n"), [])

    def test_hidutil_accepts_a_top_level_plist_array(self):
        mapping = [
            {
                "HIDKeyboardModifierMappingDst": "30064771113",
                "HIDKeyboardModifierMappingSrc": "30064771129",
            }
        ]
        with mock.patch.object(MODULE, "convert_plist_fragment", return_value=mapping):
            self.assertEqual(
                MODULE.normalize_hidutil_output("({ mapping; })"),
                [
                    {
                        "HIDKeyboardModifierMappingDst": 30064771113,
                        "HIDKeyboardModifierMappingSrc": 30064771129,
                    }
                ],
            )

    def test_redacts_repo_before_home(self):
        home = Path("/Users/example")
        repo = home / "dotfiles"
        self.assertEqual(
            MODULE.redact_path(str(repo / "config"), repo, home), "$DOTFILES/config"
        )
        self.assertEqual(
            MODULE.redact_path(str(home / ".config"), repo, home), "$HOME/.config"
        )


class ResultTest(unittest.TestCase):
    def test_unmanaged_homebrew_is_informational(self):
        result = MODULE.comparison_for(
            comparison_snapshot(extra_casks=["spotify", "telegram"])
        )
        self.assertEqual(result["status"], "match")
        self.assertEqual(result["drift_count"], 0)
        self.assertEqual(result["informational_count"], 2)

    def test_missing_homebrew_is_drift(self):
        result = MODULE.comparison_for(
            comparison_snapshot(missing_casks=["required-app"])
        )
        self.assertEqual(result["status"], "drift")
        self.assertEqual(result["drift_count"], 1)

    def test_supplemental_inventory_gap_does_not_fail_comparison(self):
        snapshot = comparison_snapshot(
            coverage=[
                {"component": "repository", "status": "ok"},
                {"component": "nix_system", "status": "ok"},
                {"component": "homebrew", "status": "ok"},
                {"component": "macos_defaults", "status": "ok"},
                {"component": "launch_agents", "status": "partial"},
            ]
        )
        snapshot["comparison"] = MODULE.comparison_for(snapshot)
        self.assertTrue(snapshot["comparison"]["coverage_complete"])
        self.assertFalse(snapshot["comparison"]["supplemental_coverage_complete"])
        self.assertEqual(MODULE.summary_exit_code(snapshot), 0)

    def test_required_comparison_gap_returns_two(self):
        snapshot = comparison_snapshot(
            coverage=[
                {"component": "repository", "status": "ok"},
                {"component": "nix_system", "status": "ok"},
                {"component": "homebrew", "status": "ok"},
                {"component": "macos_defaults", "status": "partial"},
            ]
        )
        snapshot["comparison"] = MODULE.comparison_for(snapshot)
        self.assertFalse(snapshot["comparison"]["coverage_complete"])
        self.assertEqual(MODULE.summary_exit_code(snapshot), 2)

    def test_modified_dotfiles_baseline_is_reported_without_system_drift(self):
        result = MODULE.comparison_for(
            comparison_snapshot(dirty_paths=["Brewfile.personal-mbp"])
        )
        self.assertEqual(result["status"], "match")
        self.assertEqual(result["baseline_status"], "modified")
        self.assertEqual(result["baseline_changed_path_count"], 1)

    def test_incomplete_coverage_has_precedence_in_exit_code(self):
        snapshot = {"comparison": {"coverage_complete": False, "drift_count": 3}}
        self.assertEqual(MODULE.summary_exit_code(snapshot), 2)

    def test_complete_drift_returns_one(self):
        snapshot = {"comparison": {"coverage_complete": True, "drift_count": 1}}
        self.assertEqual(MODULE.summary_exit_code(snapshot), 1)

    def test_complete_match_returns_zero(self):
        snapshot = {"comparison": {"coverage_complete": True, "drift_count": 0}}
        self.assertEqual(MODULE.summary_exit_code(snapshot), 0)


if __name__ == "__main__":
    unittest.main()
