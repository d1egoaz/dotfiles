{ pkgs, ... }:

{
  services.aerospace = {
    enable = true;
    settings = {
      # Execution settings
      exec = {
        inherit-env-vars = true;
        env-vars.PATH = "/etc/profiles/per-user/\${USER}/bin:\${PATH}";
      };

      # Startup commands
      after-login-command = [ ];
      after-startup-command = [
        "exec-and-forget sleep 5 && sketchybar --config ~/.config/sketchybar/sketchybarrc"
      ];

      # Workspace change notifications
      exec-on-workspace-change = [
        "/bin/bash"
        "-c"
        "sketchybar --trigger aerospace_workspace_change FOCUSED_WORKSPACE=\${AEROSPACE_FOCUSED_WORKSPACE}"
      ];

      # Normalizations
      enable-normalization-flatten-containers = true;
      enable-normalization-opposite-orientation-for-nested-containers = true;

      # Layout settings
      accordion-padding = 40;
      default-root-container-layout = "tiles";
      default-root-container-orientation = "auto";
      on-focused-monitor-changed = [ "move-mouse monitor-lazy-center" ];
      automatically-unhide-macos-hidden-apps = true;

      # Key mapping
      key-mapping.preset = "qwerty";

      # Gaps configuration
      gaps = {
        inner = {
          horizontal = 2;
          vertical = 2;
        };
        outer = {
          left = 3;
          right = 3;
          bottom = 2;
          top = [
            { monitor."LG" = 40; }
            { monitor."^built-in retina display$" = 15; }
            30
          ];
        };
      };

      # Mode configurations
      mode = {
        main.binding = {
          # Terminal and config
          alt-enter = "exec-and-forget open -n -a Wezterm";
          alt-shift-r = "reload-config";
          alt-r = "mode resize";

          # Layout commands
          alt-slash = "layout tiles horizontal vertical";
          alt-comma = "layout accordion horizontal vertical";

          # Focus commands
          alt-h = "focus --boundaries-action wrap-around-the-workspace left";
          alt-j = "focus --boundaries-action wrap-around-the-workspace down";
          alt-k = "focus --boundaries-action wrap-around-the-workspace up";
          alt-l = "focus --boundaries-action wrap-around-the-workspace right";

          # Move commands
          alt-shift-h = "move left";
          alt-shift-j = "move down";
          alt-shift-k = "move up";
          alt-shift-l = "move right";

          # Resize commands
          alt-minus = "resize smart -50";
          alt-equal = "resize smart +50";

          # Workspace navigation
          alt-3 = "workspace 3";
          alt-9 = "workspace 9";
          alt-a = "workspace AI";
          alt-c = "workspace Chrome";
          alt-e = "workspace Emacs";
          alt-i = "workspace IDEs";
          alt-n = "workspace Notion";
          alt-s = "workspace Slack";
          alt-t = "workspace Terminal";
          alt-z = "workspace Zoom";

          # Move to workspace
          alt-shift-3 = [
            "move-node-to-workspace 3"
            "workspace 3"
          ];
          alt-shift-9 = [
            "move-node-to-workspace 9"
            "workspace 9"
          ];
          alt-shift-a = [
            "move-node-to-workspace AI"
            "workspace AI"
          ];
          alt-shift-c = [
            "move-node-to-workspace Chrome"
            "workspace Chrome"
          ];
          alt-shift-e = [
            "move-node-to-workspace Emacs"
            "workspace Emacs"
          ];
          alt-shift-i = [
            "move-node-to-workspace IDEs"
            "workspace IDEs"
          ];
          alt-shift-n = [
            "move-node-to-workspace Notion"
            "workspace Notion"
          ];
          alt-shift-s = [
            "move-node-to-workspace Slack"
            "workspace Slack"
          ];
          alt-shift-t = [
            "move-node-to-workspace Terminal"
            "workspace Terminal"
          ];
          alt-shift-z = [
            "move-node-to-workspace Zoom"
            "workspace Zoom"
          ];

          # Workspace switching
          alt-tab = "workspace-back-and-forth";
          alt-shift-tab = "move-workspace-to-monitor --wrap-around next";

          # Mode switching
          alt-shift-semicolon = "mode service";
        };

        service.binding = {
          esc = [
            "reload-config"
            "mode main"
          ];
          r = [
            "flatten-workspace-tree"
            "mode main"
          ];
          f = [
            "layout floating tiling"
            "mode main"
          ];
          backspace = [
            "close-all-windows-but-current"
            "mode main"
          ];

          # Join commands
          alt-shift-h = [
            "join-with left"
            "mode main"
          ];
          alt-shift-j = [
            "join-with down"
            "mode main"
          ];
          alt-shift-k = [
            "join-with up"
            "mode main"
          ];
          alt-shift-l = [
            "join-with right"
            "mode main"
          ];

          # Volume controls
          down = "volume down";
          up = "volume up";
          shift-down = [
            "volume set 0"
            "mode main"
          ];
        };

        resize.binding = {
          h = "resize width -50";
          j = "resize height +50";
          k = "resize height -50";
          l = "resize width +50";
          enter = "mode main";
          esc = "mode main";
        };
      };

      # Window detection rules
      on-window-detected = [
        # Chrome
        {
          "if".app-id = "com.google.Chrome";
          run = "move-node-to-workspace Chrome";
        }
        # Emacs
        {
          "if".app-id = "org.gnu.Emacs";
          run = "move-node-to-workspace Emacs";
        }
        {
          "if".app-name-regex-substring = "emacs.*";
          run = "move-node-to-workspace Emacs";
        }
        # Terminal apps
        {
          "if".app-id = "com.googlecode.iterm2";
          run = "move-node-to-workspace Terminal";
        }
        {
          "if".app-id = "com.github.wez.wezterm";
          run = "move-node-to-workspace Terminal";
        }
        # AI apps
        {
          "if".app-id = "com.openai.chat";
          run = "move-node-to-workspace AI";
        }
        {
          "if".app-name-regex-substring = "ChatGPT";
          run = "move-node-to-workspace AI";
        }
        {
          "if".app-name-regex-substring = "Gemini";
          run = "move-node-to-workspace AI";
        }
        {
          "if".app-name-regex-substring = "AI diegoa";
          run = "move-node-to-workspace AI";
        }
        {
          "if".app-name-regex-substring = "Perplexity";
          run = "move-node-to-workspace AI";
        }
        {
          "if".app-id = "ai.perplexity.mac";
          run = "move-node-to-workspace AI";
        }
        # Notion
        {
          "if".app-id = "notion.id";
          run = "move-node-to-workspace Notion";
        }
        # Slack
        {
          "if".app-id = "com.tinyspeck.slackmacgap";
          run = "move-node-to-workspace Slack";
        }
        # Zoom
        {
          "if".app-id = "us.zoom.xos";
          run = "move-node-to-workspace Zoom";
        }
        # 1Password
        {
          "if".app-id = "com.1password.1password";
          run = "move-node-to-workspace 9";
        }
        # IDEs
        {
          "if".app-id = "com.todesktop.230313mzl4w4u92"; # Cursor
          run = "move-node-to-workspace IDEs";
        }
        {
          "if".app-id = "com.microsoft.VSCode";
          run = "move-node-to-workspace IDEs";
        }
        # Floating apps
        {
          "if".app-id = "com.apple.finder";
          run = "layout floating";
        }
        {
          "if".app-id = "com.apple.ActivityMonitor";
          run = "layout floating";
        }
        {
          "if".app-id = "com.apple.Preview";
          run = "layout floating";
        }
        {
          "if".app-id = "com.apple.QuickTimePlayerX";
          run = "layout floating";
        }
        {
          "if".app-id = "com.apple.SecurityAgent";
          run = "layout floating";
        }

        # Catch-all tiling rule with fallback
        {
          check-further-callbacks = true;
          "if".app-name-regex-substring = ".*";
          run = [
            "layout tiling"
            "move-node-to-workspace 3"
          ];
        }
        {
          "if".app-name-regex-substring = ".*";
          run = "layout floating";
        }
      ];
    };
  };
}
