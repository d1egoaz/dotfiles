import json
import pathlib
import runpy
import tempfile
import unittest
from unittest.mock import patch

SCRIPT = pathlib.Path(__file__).resolve().parents[1] / "bin/files/codex-native-runtime"
runtime_config = runpy.run_path(str(SCRIPT))["runtime_config"]
contains_config = runpy.run_path(str(SCRIPT))["contains_config"]
runtime_explicitly_disabled = runpy.run_path(str(SCRIPT))["runtime_explicitly_disabled"]


class NativeRuntimeTest(unittest.TestCase):
    def test_explicit_plugin_disable_is_respected(self):
        self.assertFalse(runtime_explicitly_disabled({}))
        for name in ("chrome@openai-bundled", "browser@openai-bundled", "computer-use@openai-bundled"):
            self.assertTrue(runtime_explicitly_disabled({"plugins": {name: {"enabled": False}}}))

    def test_activation_skips_missing_prerequisites_without_config_writes(self):
        namespace = runpy.run_path(str(SCRIPT))
        main = namespace["main"]
        with tempfile.TemporaryDirectory() as directory, patch("pathlib.Path.home", return_value=pathlib.Path(directory)), patch("sys.argv", [str(SCRIPT), "--apply", "--skip-unavailable"]):
            with patch.dict(main.__globals__, {"runtime_config": lambda *_: (_ for _ in ()).throw(RuntimeError("missing cache")), "register": lambda *_: self.fail("must not write unavailable runtime")}):
                main()

    def test_readback_accepts_defaults_but_rejects_missing_or_changed_values(self):
        self.assertTrue(contains_config({"enabled": True, "env": {"a": "1", "policy": "keep"}}, {"env": {"a": "1"}}))
        self.assertFalse(contains_config({"env": {"a": "2"}}, {"env": {"a": "1"}}))
        self.assertFalse(contains_config({}, {"enabled": True}))

    def fixture(self, root):
        home = root / "home/.codex"
        resources = root / "Codex.app/Contents/Resources"
        files = [
            resources / "cua_node/bin/node_repl",
            resources / "cua_node/bin/node",
            resources / "cua_node/lib/node_modules/@oai/sky/Codex Computer Use.app",
            home / "plugins/cache/openai-bundled/chrome/1/scripts/browser-service.mjs",
        ]
        for path in files:
            path.parent.mkdir(parents=True, exist_ok=True)
            path.touch()
        manifest = resources / "plugins/openai-bundled/plugins/chrome/.codex-plugin/plugin.json"
        manifest.parent.mkdir(parents=True)
        manifest.write_text(json.dumps({"version": "1"}))
        return home, resources

    def test_uses_native_runtime_and_trusted_cached_plugin_without_disabling_sandbox(self):
        with tempfile.TemporaryDirectory() as directory:
            home, resources = self.fixture(pathlib.Path(directory))
            config = runtime_config(home, resources)
            self.assertEqual(config["command"], str(resources / "cua_node/bin/node_repl"))
            self.assertNotIn("args", config)
            self.assertIn("CODEX_CLI_PATH", config["env"])
            self.assertEqual(config["env"]["NODE_REPL_TRUSTED_CODE_PATHS"].split(":"), [str(home), str(resources / "cua_node/lib/node_modules")])
            services = json.loads(config["env"]["NODE_REPL_TRUSTED_SERVICES"])
            self.assertTrue(services["browser"].startswith(str(home)))
            self.assertEqual(config["env"]["BROWSER_USE_AVAILABLE_BACKENDS"], "chrome")

    def test_respects_existing_cdp_permission(self):
        with tempfile.TemporaryDirectory() as directory:
            home, resources = self.fixture(pathlib.Path(directory))
            settings = home / "browser/config.toml"
            settings.parent.mkdir()
            settings.write_text("full_cdp_access_enabled = true\n")
            self.assertEqual(runtime_config(home, resources)["env"]["BROWSER_USE_AVAILABLE_BACKENDS"], "chrome,cdp")
            settings.write_text("full_cdp_access_enabled = false\n")
            self.assertEqual(runtime_config(home, resources)["env"]["BROWSER_USE_AVAILABLE_BACKENDS"], "chrome")

    def test_missing_runtime_fails_before_registration(self):
        with tempfile.TemporaryDirectory() as directory:
            home, resources = self.fixture(pathlib.Path(directory))
            (resources / "cua_node/bin/node_repl").unlink()
            with self.assertRaisesRegex(RuntimeError, "component missing"):
                runtime_config(home, resources)


if __name__ == "__main__":
    unittest.main()
