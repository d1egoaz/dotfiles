{
  config,
  lib,
  profile,
  ...
}:

let
  # Centralized Alfred workflow configuration
  alfredConfig = {
    openai_model = "gpt-4.1-nano";
  };
in
{
  # ============================================================================
  # Alfred Configuration
  # ============================================================================
  # OpenAI model is baked in at build time. 1Password account/vault come from
  # OP_ACCOUNT and OP_VAULT env vars at runtime.
  # ============================================================================

  # Generated script for Alfred OpenAI workflows (Grammar Fixer, Tone Fixer).
  # Both workflows call this via: exec "$HOME/.local/bin/alfred-openai-rewrite" "$1"
  # The $prompt env var is set by each workflow's "Prompt" configuration field.
  home.file.".local/bin/alfred-openai-rewrite" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      KEY=$(op read --account "$OP_ACCOUNT" "op://$OP_VAULT/OpenAI API/credential" 2>/dev/null)
      if [ -z "$KEY" ]; then
        echo "ERROR: OpenAI API key not found. Authenticate: eval \$(op signin)"
        exit 0
      fi

      curl -s https://api.openai.com/v1/chat/completions \
        -H "Authorization: Bearer $KEY" \
        -H "Content-Type: application/json" \
        -d "$(jq -n --arg model "${alfredConfig.openai_model}" --arg system "$prompt" --arg user "$1" \
          '{model:$model, messages:[{role:"system",content:$system},{role:"user",content:$user}]}')" \
      | jq -r '.choices[0].message.content'
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
