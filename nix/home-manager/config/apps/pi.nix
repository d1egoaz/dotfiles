{
  config,
  pkgs,
  profile,
  ...
}:

let
  piThinkingLevel = if profile == "office" then "xhigh" else "low";
  managedPiSettings = builtins.toJSON {
    defaultProvider = "openai-codex";
    defaultModel = "gpt-5.5";
    defaultThinkingLevel = piThinkingLevel;
  };
in
{
  # Keep mutable Pi state owned by Pi itself, but make the global default use
  # ChatGPT subscription auth once the user has logged in via /login.
  home.activation.piSettings = config.lib.dag.entryAfter [ "writeBoundary" ] ''
    PI_AGENT_DIR="$HOME/.pi/agent"
    PI_SETTINGS_PATH="$PI_AGENT_DIR/settings.json"
    MANAGED_PATH="$(mktemp)"
    TMP_PATH="$(mktemp)"

    mkdir -p "$PI_AGENT_DIR"
    printf '%s\n' '${managedPiSettings}' > "$MANAGED_PATH"

    if [ -f "$PI_SETTINGS_PATH" ] && [ -s "$PI_SETTINGS_PATH" ]; then
      ${pkgs.jq}/bin/jq -s '.[0] * .[1]' "$PI_SETTINGS_PATH" "$MANAGED_PATH" > "$TMP_PATH"
    else
      cp "$MANAGED_PATH" "$TMP_PATH"
    fi

    install -m 600 "$TMP_PATH" "$PI_SETTINGS_PATH"
    rm -f "$MANAGED_PATH"
    rm -f "$TMP_PATH"
  '';
}
