{
  config,
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
    ".codex/AGENTS.md".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/ai/AGENTS.md";

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

    # Stable emacs wrapper for GUI apps that don't inherit Nix PATH
    ".local/bin/nix-emacs" = {
      executable = true;
      text = ''
        #!/usr/bin/env bash
        exec "${import ../../packages/emacs.nix { inherit pkgs; }}/bin/emacs" "$@"
      '';
    };

    "Applications/nix-emacs.app/Contents/MacOS/nix-emacs" = {
      executable = true;
      text = ''
        #!/usr/bin/env bash
        exec "${config.home.homeDirectory}/.local/bin/nix-emacs" "$@"
      '';
    };

    # macOS app bundle for nix-emacs (enables `open -a nix-emacs`)
    "Applications/nix-emacs.app/Contents/Info.plist".text = ''
      <?xml version="1.0" encoding="UTF-8"?>
      <!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
      <plist version="1.0">
      <dict>
        <key>CFBundleExecutable</key>
        <string>nix-emacs</string>
        <key>CFBundleIdentifier</key>
        <string>com.nixos.nix-emacs</string>
        <key>CFBundleName</key>
        <string>nix-emacs</string>
        <key>CFBundlePackageType</key>
        <string>APPL</string>
        <key>CFBundleShortVersionString</key>
        <string>1.0</string>
        <key>CFBundleVersion</key>
        <string>1</string>
        <key>LSMinimumSystemVersion</key>
        <string>10.13</string>
      </dict>
      </plist>
    '';

    # Custom utility scripts from bin/files/
    ".local/bin/ediff".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/bin/files/ediff";
    ".local/bin/ediff3".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/bin/files/ediff3";
    ".local/bin/k".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/bin/files/k";
    ".local/bin/kc".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/bin/files/kc";
    ".local/bin/kk".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/bin/files/kk";
    ".local/bin/kn".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/bin/files/kn";
    ".local/bin/kstern".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/bin/files/kstern";
    ".local/bin/ktmux".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/bin/files/ktmux";
    ".local/bin/pr-approve".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/bin/files/pr-approve.sh";
  };

  xdg = {
    enable = true;
    # Configuration files - direct symlinks to dotfiles (editable)
    configFile = {
      "aerospace/aerospace.toml".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/aerospace/aerospace.toml";
      "borders/bordersrc".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/borders/bordersrc";
      "sketchybar".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/sketchybar";
      "wezterm".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/wezterm";
      "ghostty/config".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/ghostty/config";
      # eza theme is configured via programs.eza.theme.source in programs.nix
      # AI assistant configuration
      "agents/AGENTS.md".source =
        config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/config/ai/AGENTS.md";
    };

    # Data files
    dataFile = {
    };
  };

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
