{
  config,
  lib,
  opConfig,
  profile,
  ...
}:

let
  # Centralized Alfred workflow configuration
  # Change these values and run `just switch` to update all workflows
  alfredConfig = {
    openai_model = "gpt-4.1-nano";
    inherit (opConfig) op_account op_vault;
  };
in
{
  # ============================================================================
  # Alfred Configuration
  # ============================================================================
  # Workflow settings (model, 1Password account/vault) are baked into the
  # generated alfred-openai-rewrite script at build time. No runtime config
  # file needed.
  # ============================================================================

  # Generated script for Alfred OpenAI workflows (Grammar Fixer, Tone Fixer).
  # Both workflows call this via: exec "$HOME/.local/bin/alfred-openai-rewrite" "$1"
  # The $prompt env var is set by each workflow's "Prompt" configuration field.
  home.file.".local/bin/alfred-openai-rewrite" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash
      KEY=$(op read --account "${alfredConfig.op_account}" "op://${alfredConfig.op_vault}/OpenAI API/credential" 2>/dev/null)
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
