{
  config,
  pkgs,
  machineConfig,
  ...
}:

let
  mkOpRead =
    field:
    "!op read --account \"${machineConfig.op_account}\" \"op://${machineConfig.op_vault}/API_KEYS/${field}\"";
  managedPiAuth = builtins.toJSON {
    zai = {
      type = "api_key";
      key = mkOpRead "ZAI_API_KEY";
    };
  };
in
{
  # Merge managed provider credentials into pi's auth.json without clobbering
  # unrelated providers or OAuth entries created by pi itself.
  home.activation.piAuth = config.lib.dag.entryAfter [ "writeBoundary" ] ''
    PI_AGENT_DIR="$HOME/.pi/agent"
    PI_AUTH_PATH="$PI_AGENT_DIR/auth.json"
    MANAGED_PATH="$(mktemp)"
    TMP_PATH="$(mktemp)"

    mkdir -p "$PI_AGENT_DIR"
    printf '%s\n' '${managedPiAuth}' > "$MANAGED_PATH"

    if [ -f "$PI_AUTH_PATH" ] && [ -s "$PI_AUTH_PATH" ]; then
      ${pkgs.jq}/bin/jq -s '.[0] * .[1]' "$PI_AUTH_PATH" "$MANAGED_PATH" > "$TMP_PATH"
    else
      cp "$MANAGED_PATH" "$TMP_PATH"
    fi

    install -m 600 "$TMP_PATH" "$PI_AUTH_PATH"
    rm -f "$MANAGED_PATH"
    rm -f "$TMP_PATH"
  '';
}
