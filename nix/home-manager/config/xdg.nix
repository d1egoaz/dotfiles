{
  config,
  lib,
  machineConfig,
  pkgs,
  profile,
  ...
}:

let
  codexProfile = if profile == "office" then "work" else "personal";
  codexProfileFile = if profile == "office" then "work.local.toml" else "personal.toml";
in
{
  # ============================================================================
  # XDG Configuration and Directory Management
  # ============================================================================

  # ============================================================================
  # Home Directory File Symlinks
  # ============================================================================

  # AI assistant configuration files - direct symlinks to dotfiles (editable)
  home.file = {
    "AGENTS.md".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/ai/AGENTS.md";
    ".claude/CLAUDE.md".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/ai/AGENTS.md";
    ".claude/settings.json".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/claude/settings.json";
    ".codex/AGENTS.md".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/ai/AGENTS.md";
    ".codex/rules/10-shared.rules".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/codex/rules/10-shared.rules";
    ".codex/themes".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/codex/themes";
    ".agents/skills/multi-agent-team".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/agents/skills/multi-agent-team";

    # Stable fish wrapper for GUI apps (like Ghostty) that don't inherit Nix PATH
    ".local/bin/nix-fish" = {
      executable = true;
      text = ''
        #!/usr/bin/env bash
        exec "${pkgs.fish}/bin/fish" "$@"
      '';
    };

    # Ghostty CLI wrapper (executes Homebrew installation directly)
    ".local/bin/ghostty" = {
      executable = true;
      text = ''
        #!/usr/bin/env bash
        exec "/Applications/Ghostty.app/Contents/MacOS/ghostty" "$@"
      '';
    };

    ".local/bin/aa" = {
      executable = true;
      text = ''
        #!/usr/bin/env bash
        set -euo pipefail

        if ! command -v op >/dev/null 2>&1; then
          echo "aa: 1Password CLI ('op') is not available in PATH" >&2
          exit 1
        fi

        if ! command -v agent >/dev/null 2>&1; then
          echo "aa: 'agent' is not available in PATH" >&2
          exit 1
        fi

        key="$(op read --account "${machineConfig.op_account}" "op://${machineConfig.op_vault}/Cursor api key/API_KEY" 2>/dev/null || true)"
        if [ -z "$key" ]; then
          echo "aa: failed to read Cursor API key from 1Password. Authenticate with 'op signin' and verify the item exists." >&2
          exit 1
        fi

        export CURSOR_API_KEY="$key"
        exec agent "$@"
      '';
    };

    # Custom utility scripts from bin/files/ symlinked to ~/.local/bin/
    # To add a new script: drop it in bin/files/ and append the filename here.
    # .sh suffix is stripped from the symlink name.
    # Excluded: awsprofile.fish (sourced by fish), codex-notify.sh (invoked by absolute path).
  }
  // lib.optionalAttrs (profile == "office") {
    ".agents/skills/work-local".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/agents/skills/work.local";
  }
  // (
    let
      binDir = "${config.home.homeDirectory}/dotfiles/bin/files";
      scripts = [
        ",,"
        "dotfiles-local-state"
        "ediff"
        "ediff3"
        "k"
        "kc"
        "kk"
        "kn"
        "kstern"
        "ktmux"
        "op-env-cache"
        "pr-approve.sh"
      ];
    in
    lib.listToAttrs (
      map (name: {
        name = ".local/bin/${lib.removeSuffix ".sh" name}";
        value.source = config.lib.file.mkOutOfStoreSymlink "${binDir}/${name}";
      }) scripts
    )
  );

  xdg = {
    enable = true;
    # Configuration files - direct symlinks to dotfiles (editable)
    configFile = {
      "aerospace/aerospace.toml".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/aerospace/aerospace.toml";
      "borders/bordersrc".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/borders/bordersrc";
    }
    // (
      if profile != "office" then
        {
          "sketchybar".source =
            config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/sketchybar";
        }
      else
        { }
    )
    // {
      "wezterm".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/wezterm";
      "ghostty/config".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/ghostty/config";
      "emacs-plus/build.yml".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/emacs-plus/build.yml";
      # eza theme is configured via programs.eza.theme.source in programs.nix
      # AI assistant configuration
      "agents/AGENTS.md".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/ai/AGENTS.md";
    };

    # Data files
    dataFile = {
    };
  };

  # ============================================================================
  # Activation Scripts
  # ============================================================================

  # Ensure the user-level npm global prefix exists for Nix-managed Node.js.
  home.activation.npmGlobalPrefix = config.lib.dag.entryAfter [ "writeBoundary" ] ''
    mkdir -p "$HOME/.local/share/npm-global/bin"
  '';

  # Keep work-only skills out of tracked dotfiles while still giving office
  # machines a stable symlink target for local runbooks. This intentionally
  # creates an ignored directory inside the repo so SOPS can restore into it.
  home.activation.workLocalAgentSkills = lib.mkIf (profile == "office") (
    config.lib.dag.entryBetween [ "linkGeneration" ] [ "writeBoundary" ] ''
      mkdir -p "$HOME/dotfiles/config/agents/skills/work.local"
    ''
  );

  # Codex 0.134+ removed the top-level `profile = "name"` selector. Compose
  # the default config from the shared file and the Nix-selected machine profile.
  home.activation.codexConfigProfile = config.lib.dag.entryAfter [ "writeBoundary" ] ''
    CODEX_PROFILE="${codexProfile}"
    SHARED="$HOME/dotfiles/config/codex/config.toml"
    PROFILE_FILE="$HOME/dotfiles/config/codex/profiles/${codexProfileFile}"
    TARGET="$HOME/.codex/config.toml"
    TMP="$TARGET.tmp.toml"
    AWK="${pkgs.gawk}/bin/awk"
    TAPLO="${pkgs.taplo}/bin/taplo"

    if [ ! -f "$SHARED" ]; then
      echo "Missing shared Codex config: $SHARED" >&2
      exit 1
    fi

    if [ ! -f "$PROFILE_FILE" ]; then
      if [ "$CODEX_PROFILE" = "work" ]; then
        echo "Missing optional Codex work profile config: $PROFILE_FILE; using shared config only" >&2
        PROFILE_FILE=""
      else
        echo "Missing Codex $CODEX_PROFILE profile config: $PROFILE_FILE" >&2
        exit 1
      fi
    fi

    if [ -n "$PROFILE_FILE" ]; then
      FIRST_PROFILE_LINE="$("$AWK" 'NF && $1 !~ /^#/ { print; exit }' "$PROFILE_FILE")"
      case "$FIRST_PROFILE_LINE" in
        "" | \[*\])
          ;;
        *)
          echo "Codex profile fragment must start with a TOML section header: $PROFILE_FILE" >&2
          exit 1
          ;;
      esac
    fi

    mkdir -p "$HOME/.codex"
    rm -f "$TMP"
    {
      printf '# Generated by Home Manager from dotfiles Codex profile: %s\n' "$CODEX_PROFILE"
      printf '# Edit config/codex/config.toml and config/codex/profiles/%s instead.\n\n' "${codexProfileFile}"
      cat "$SHARED"
      printf '\n# Active Codex profile: %s\n' "$CODEX_PROFILE"
      if [ -n "$PROFILE_FILE" ]; then
        cat "$PROFILE_FILE"
      fi
    } > "$TMP"

    if ! "$TAPLO" check "$TMP"; then
      echo "Generated Codex config is invalid TOML: $TMP" >&2
      rm -f "$TMP"
      exit 1
    fi

    if [ -L "$TARGET" ]; then
      LINK_TARGET="$(readlink "$TARGET" || true)"
      if [ -e "$TARGET" ]; then
        BACKUP="$TARGET.backup"
        if [ -e "$BACKUP" ]; then
          BACKUP="$TARGET.backup.$(date -u +%Y%m%dT%H%M%SZ)"
        fi
        cp "$TARGET" "$BACKUP" || {
          rm -f "$BACKUP"
          exit 1
        }
        echo "Backed up existing $TARGET symlink target ($LINK_TARGET) to $BACKUP" >&2
      else
        echo "Removing dangling $TARGET symlink to $LINK_TARGET" >&2
      fi
      rm -f "$TARGET"
    fi

    if [ -e "$TARGET" ] && [ ! -L "$TARGET" ] && ! head -n 1 "$TARGET" | grep -q '^# Generated by Home Manager from dotfiles Codex profile:'; then
      BACKUP="$TARGET.backup"
      if [ -e "$BACKUP" ]; then
        BACKUP="$TARGET.backup.$(date -u +%Y%m%dT%H%M%SZ)"
      fi
      cp "$TARGET" "$BACKUP" || {
        rm -f "$BACKUP"
        exit 1
      }
      echo "Backed up existing $TARGET to $BACKUP" >&2
    fi

    if [ -f "$TARGET" ] && [ ! -L "$TARGET" ] && cmp -s "$TMP" "$TARGET"; then
      rm -f "$TMP"
    else
      mv "$TMP" "$TARGET"
    fi
  '';

  # Keep shared hooks portable while allowing an ignored work-only hook file on
  # the office machine. Missing local hooks fall back to the tracked shared file.
  home.activation.codexHooksProfile = config.lib.dag.entryAfter [ "writeBoundary" ] ''
    SHARED_HOOKS="$HOME/dotfiles/config/codex/hooks.json"
    WORK_HOOKS="$HOME/dotfiles/config/codex/hooks.work.local.json"
    TARGET="$HOME/.codex/hooks.json"
    HOOKS_FILE="$SHARED_HOOKS"
    JQ="${pkgs.jq}/bin/jq"

    if [ "${codexProfile}" = "work" ] && [ -f "$WORK_HOOKS" ]; then
      HOOKS_FILE="$WORK_HOOKS"
    fi

    if [ ! -f "$HOOKS_FILE" ]; then
      echo "Missing Codex hooks file: $HOOKS_FILE" >&2
      exit 1
    fi

    if ! "$JQ" . "$HOOKS_FILE" >/dev/null; then
      echo "Codex hooks file is invalid JSON: $HOOKS_FILE" >&2
      exit 1
    fi

    mkdir -p "$HOME/.codex"
    if [ -e "$TARGET" ] && [ ! -L "$TARGET" ]; then
      BACKUP="$TARGET.backup"
      if [ -e "$BACKUP" ]; then
        BACKUP="$TARGET.backup.$(date -u +%Y%m%dT%H%M%SZ)"
      fi
      cp "$TARGET" "$BACKUP" || {
        rm -f "$BACKUP"
        exit 1
      }
      echo "Backed up existing $TARGET to $BACKUP" >&2
    fi
    ln -sfn "$HOOKS_FILE" "$TARGET"
  '';

  # Restore ignored local state from SOPS on office machines only. `just switch`
  # only repairs missing local files; `just local-state-sync` is the explicit
  # path for persisting local edits back into encrypted state.
  home.activation.localStateRestore = lib.mkIf (profile == "office") (
    config.lib.dag.entryBetween [ "workLocalAgentSkills" ] [ "writeBoundary" ] ''
      LOCAL_STATE_HELPER="$HOME/dotfiles/bin/files/dotfiles-local-state"
      if [ -x "$LOCAL_STATE_HELPER" ]; then
        DOTFILES_LOCAL_STATE_PROFILE="${profile}" "$LOCAL_STATE_HELPER" restore-if-missing
      fi
    ''
  );

  # Hard link mouseless config into its macOS sandbox container.
  # Hard links work across sandbox boundaries (same filesystem/inode).
  # Re-runs on every `just switch` to repair if the app recreated the file.
  home.activation.mouselessConfig = config.lib.dag.entryAfter [ "writeBoundary" ] ''
    MOUSELESS_DIR="$HOME/Library/Containers/net.sonuscape.mouseless/Data/.mouseless/configs"
    SOURCE="${config.home.homeDirectory}/dotfiles/config/mouseless/config.yaml"
    TARGET="$MOUSELESS_DIR/config.yaml"

    if [ -f "$SOURCE" ]; then
      mkdir -p "$MOUSELESS_DIR"
      # Remove existing file (might be a regular file or broken link)
      rm -f "$TARGET"
      ln "$SOURCE" "$TARGET"
    fi
  '';

  # Manual launchd service for AeroSpace using external config (disabled on office profile)
  # AeroSpace installed via Homebrew cask: nikitabobko/tap/aerospace
  launchd.agents.aerospace = {
    enable = profile != "office";
    config = {
      ProgramArguments = [
        "/Applications/AeroSpace.app/Contents/MacOS/AeroSpace"
        "--config-path"
        "${config.home.homeDirectory}/dotfiles/config/aerospace/aerospace.toml"
      ];
      KeepAlive = true;
      RunAtLoad = true;
      StandardOutPath = "/tmp/aerospace.log";
      StandardErrorPath = "/tmp/aerospace.err.log";
    };
  };
}
