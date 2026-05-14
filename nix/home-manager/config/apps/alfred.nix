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
  # Public values (model, provider, endpoint, cache spec name) are baked in at
  # build time. 1Password references live in SOPS because Alfred workflows don't
  # inherit shell env vars.
  # ============================================================================

  # Generated script for Alfred text rewrite workflows.
  # Workflows call this with a mode flag, for example:
  #   exec "$HOME/.local/bin/alfred-openai-rewrite" --mode grammar "$1"
  #   exec "$HOME/.local/bin/alfred-openai-rewrite" --mode tone "$1"
  home.file.".local/bin/alfred-openai-rewrite" = {
    executable = true;
    text = ''
      #!/usr/bin/env bash

      OP_ACCOUNT="${machineConfig.op_account}"
      BASE_URL="${machineConfig.llm.base_url}"
      MODEL="${machineConfig.llm.model}"
      PROVIDER="${machineConfig.llm.provider}"
      CACHE_NAME="alfred-llm-rewrite-${profile}"
      SECRET_ENV="ALFRED_LLM_API_KEY"
      SOPS_SPEC_FILE="$HOME/dotfiles/secrets/codex.yaml"
      OP_ENV_CACHE_BIN="''${OP_ENV_CACHE_BIN:-$HOME/.local/bin/op-env-cache}"
      MODE=""
      USER_TEXT=""

      # Prefer the Home Manager symlink; fall back to the repo path before switch.
      if [ ! -x "$OP_ENV_CACHE_BIN" ] && [ -x "$HOME/dotfiles/bin/files/op-env-cache" ]; then
        OP_ENV_CACHE_BIN="$HOME/dotfiles/bin/files/op-env-cache"
      fi

      if [ ! -x "$OP_ENV_CACHE_BIN" ]; then
        echo "ERROR: op-env-cache not found. Run: just switch"
        exit 0
      fi

      CACHE_FILE_ARGS=()
      # Keep ALFRED_OPENAI_AUTH_CACHE as a legacy override for older local wrappers.
      if [ -n "''${ALFRED_OPENAI_AUTH_CACHE:-}" ]; then
        CACHE_FILE_ARGS=(--cache-file "$ALFRED_OPENAI_AUTH_CACHE")
      elif [ -n "''${ALFRED_LLM_AUTH_CACHE:-}" ]; then
        CACHE_FILE_ARGS=(--cache-file "$ALFRED_LLM_AUTH_CACHE")
      fi

      if [ "''${1:-}" = "--cache" ]; then
        CACHE_COMMAND="''${2:-refresh}"

        "$OP_ENV_CACHE_BIN" "$CACHE_COMMAND" "$CACHE_NAME" \
          --account "$OP_ACCOUNT" \
          "''${CACHE_FILE_ARGS[@]}" \
          --sops-file "$SOPS_SPEC_FILE"
        exit $?
      fi

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

      KEY_ERROR_FILE=$(mktemp "''${TMPDIR:-/tmp}/alfred-llm-rewrite.XXXXXX") || {
        echo "ERROR ($PROVIDER): failed to create temporary file"
        exit 0
      }
      KEY_OUTPUT=$("$OP_ENV_CACHE_BIN" get "$CACHE_NAME" "$SECRET_ENV" \
        --account "$OP_ACCOUNT" \
        --auto-refresh \
        "''${CACHE_FILE_ARGS[@]}" \
        --sops-file "$SOPS_SPEC_FILE" 2>"$KEY_ERROR_FILE")
      KEY_STATUS=$?
      if [ "$KEY_STATUS" -ne 0 ]; then
        KEY_ERROR=$(cat "$KEY_ERROR_FILE")
        rm -f "$KEY_ERROR_FILE"
        if [ -z "$KEY_ERROR" ]; then
          KEY_ERROR="failed to load API key with op-env-cache"
        fi
        echo "ERROR ($PROVIDER): $KEY_ERROR"
        exit 0
      fi
      rm -f "$KEY_ERROR_FILE"
      KEY="$KEY_OUTPUT"

      if [ "$PROVIDER" = "OpenAI" ]; then
        REQUEST_BODY=$(jq -n --arg model "$MODEL" --arg system "$SYSTEM_PROMPT" --arg user "$USER_TEXT" \
          '{model:$model, messages:[{role:"system",content:$system},{role:"user",content:$user}], reasoning_effort:"none"}')
      else
        REQUEST_BODY=$(jq -n --arg model "$MODEL" --arg system "$SYSTEM_PROMPT" --arg user "$USER_TEXT" \
          '{model:$model, messages:[{role:"system",content:$system},{role:"user",content:$user}], reasoning_effort:"low"}')
      fi

      RESPONSE=$(curl -sS --connect-timeout 2 --max-time 5 "$BASE_URL/chat/completions" \
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
