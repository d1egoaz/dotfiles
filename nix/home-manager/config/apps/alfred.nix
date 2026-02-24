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

  # Generated script for Alfred text rewrite workflows.
  # Workflows call this with a mode flag, for example:
  #   exec "$HOME/.local/bin/alfred-openai-rewrite" --mode grammar "$1"
  #   exec "$HOME/.local/bin/alfred-openai-rewrite" --mode tone "$1"
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
      MODE=""
      USER_TEXT=""

      if [ "''${1:-}" = "--mode" ] && [ $# -ge 3 ]; then
        MODE="$2"
        USER_TEXT="$3"
      elif [[ "''${1:-}" == --mode=* ]] && [ $# -ge 2 ]; then
        MODE="''${1#--mode=}"
        USER_TEXT="$2"
      else
        echo "ERROR: usage: alfred-openai-rewrite --mode grammar|tone \"text\""
        exit 0
      fi

      DOTFILES_DIR="$HOME/dotfiles"
      case "$MODE" in
        grammar) PROMPT_FILE="$DOTFILES_DIR/config/ai/grammar-fixer-prompt.md" ;;
        tone) PROMPT_FILE="$DOTFILES_DIR/config/ai/tone-fixer-prompt.md" ;;
        *)
          echo "ERROR: unsupported mode '$MODE' (expected: grammar|tone)"
          exit 0
          ;;
      esac

      if [ ! -f "$PROMPT_FILE" ]; then
        echo "ERROR: prompt file not found: $PROMPT_FILE"
        exit 0
      fi

      # Normalize whitespace to reduce token overhead.
      SYSTEM_PROMPT=$(cat "$PROMPT_FILE" | tr '\n' ' ' | tr -s '[:space:]' ' ')

      # Keep responses quick: cap output size based on input length.
      INPUT_CHARS=''${#USER_TEXT}
      MAX_TOKENS=$(( (INPUT_CHARS / 3) + 120 ))
      if [ "$MAX_TOKENS" -lt 160 ]; then MAX_TOKENS=160; fi
      if [ "$MAX_TOKENS" -gt 700 ]; then MAX_TOKENS=700; fi

      if [ "$PROVIDER" = "OpenAI" ]; then
        REQUEST_BODY=$(jq -n --arg model "$MODEL" --arg system "$SYSTEM_PROMPT" --arg user "$USER_TEXT" --argjson max_tokens "$MAX_TOKENS" \
          '{model:$model, messages:[{role:"system",content:$system},{role:"user",content:$user}], reasoning_effort:"minimal", max_completion_tokens:$max_tokens}')
      else
        REQUEST_BODY=$(jq -n --arg model "$MODEL" --arg system "$SYSTEM_PROMPT" --arg user "$USER_TEXT" --argjson max_tokens "$MAX_TOKENS" \
          '{model:$model, messages:[{role:"system",content:$system},{role:"user",content:$user}], max_tokens:$max_tokens}')
      fi

      RESPONSE=$(curl -sS --connect-timeout 3 --max-time 45 "$BASE_URL/chat/completions" \
        -H "Authorization: Bearer $KEY" \
        -H "Content-Type: application/json" \
        -d "$REQUEST_BODY")

      if [ -z "$RESPONSE" ]; then
        echo "ERROR ($PROVIDER): empty response"
        exit 0
      fi

      echo "$RESPONSE" | jq -r 'if .error then "ERROR (" + $provider + "): " + (.error.message // "unknown") else .choices[0].message.content end' --arg provider "$PROVIDER"
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
