{
  config,
  lib,
  pkgs,
  profile,
  ...
}:

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
    ".codex/config.toml".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/codex/config.toml";
    ".codex/themes".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/codex/themes";

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

    # Custom utility scripts from bin/files/ symlinked to ~/.local/bin/
    # To add a new script: drop it in bin/files/ and append the filename here.
    # .sh suffix is stripped from the symlink name.
    # Excluded: awsprofile.fish (sourced by fish), codex-notify.sh (invoked by absolute path).
  }
  // (
    let
      binDir = "${config.home.homeDirectory}/dotfiles/bin/files";
      scripts = [
        "ediff"
        "ediff3"
        "k"
        "kc"
        "kk"
        "kn"
        "kstern"
        "ktmux"
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
