{
  config,
  lib,
  machineConfig,
  profile,
  ...
}:

{
  # ============================================================================
  # Alfred Configuration
  # ============================================================================
  # All values (model, provider, endpoint, 1Password account/vault) are baked
  # in at build time. This is required because Alfred workflows don't inherit
  # shell env vars.
  # ============================================================================

  # Generated script for Alfred OpenAI workflows (Grammar Fixer, Tone Fixer).
  # Both workflows call this via: exec "$HOME/.local/bin/alfred-openai-rewrite" "$1"
  # The $prompt env var is set by each workflow's "Prompt" configuration field.
  home.file.".local/bin/alfred-openai-rewrite" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash

      KEY=$(op read --account "${machineConfig.op_account}" "op://${machineConfig.op_vault}/${machineConfig.llm.key_item}/credential" 2>/dev/null)
      if [ -z "$KEY" ]; then
        echo "ERROR: ${machineConfig.llm.provider} API key not found. Authenticate: eval \$(op signin)"
        exit 0
      fi

      BASE_URL="${machineConfig.llm.base_url}"
      MODEL="${machineConfig.llm.model}"
      PROVIDER="${machineConfig.llm.provider}"

      curl -s "$BASE_URL/chat/completions" \
        -H "Authorization: Bearer $KEY" \
        -H "Content-Type: application/json" \
        -d "$(jq -n --arg model "$MODEL" --arg system "$prompt" --arg user "$1" \
          '{model:$model, messages:[{role:"system",content:$system},{role:"user",content:$user}]}')" \
      | jq -r 'if .error then "ERROR (" + $provider + "): " + (.error.message // "unknown") else .choices[0].message.content end' --arg provider "$PROVIDER"
    '';
  };

  # Set Alfred preferences folder to dotfiles location (office only)
  # This runs at login to ensure Alfred knows where to find its synced preferences
  launchd.agents.alfred-set-preferences = lib.mkIf (profile == "office") {
    enable = true;
    config = {
      ProgramArguments = [
        "/usr/bin/defaults"
        "write"
        "com.runningwithcrayons.Alfred-Preferences"
        "syncfolder"
        "${config.home.homeDirectory}/dotfiles/alfred"
      ];
      RunAtLoad = true;
    };
  };
}
