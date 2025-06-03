{ pkgs, ... }:

{
  programs.starship = {
    enable = true;
    settings = {
      # Get editor completions based on the config schema
      "$schema" = "https://starship.rs/config-schema.json";

      ###################
      # Enabled modules #
      ###################

      line_break = {
        disabled = false;
      };

      ruby = {
        disabled = false;
      };

      character = {
        disabled = false;
        success_symbol = "[➜](bold green)";
        error_symbol = "[➜](bold red)";
      };

      aws = {
        disabled = false;
        format = "on [$symbol($profile )(\\($region\\) )(\\[$duration\\] )]($style)";
      };

      git_branch = {
        disabled = false;
      };

      git_status = {
        disabled = false;
      };

      git_state = {
        disabled = false;
      };

      golang = {
        disabled = false;
      };

      rust = {
        disabled = false;
      };

      cmd_duration = {
        disabled = false;
        min_time = 2000;
        show_milliseconds = true;
        show_notifications = true;
      };

      time = {
        disabled = false;
        format = "[$time]($style) ";
        time_format = "%l:%M:%S %P";
        utc_time_offset = "local";
        style = "italic dimmed white";
      };

      directory = {
        disabled = false;
        style = "blue";
      };

      nodejs = {
        disabled = false;
      };

      python = {
        disabled = false;
      };

      terraform = {
        disabled = false;
      };

      nix_shell = {
        disabled = false;
      };

      direnv = {
        disabled = false;
      };

      # =====================================================
      # Custom modules
      # =====================================================

      custom.direnv = {
        detect_files = [ ".envrc" ];
        format = "[ direnv](bold bright-yellow)";
        disabled = false;
      };

      custom.devbox = {
        when = "test -n \"$IN_NIX_SHELL\"";
        # command = "echo devbox";
        # style = "dimmed fg:white bg:black";
        format = "[ \\[devbox\\]](bold white)";
        disabled = false;
      };

      custom.kubeinfo = {
        command = "sed -n 's/KUBE_CONTEXT=\\(.*\\)/Kube Context: \\1,/p; s/KUBE_NAMESPACE=\\(.*\\)/namespace: \\1/p' .kube-env | paste -sd ' ' -";
        detect_files = [ ".kube-env" ];
        format = " [$output]($style)";
        disabled = false;
      };

      # =====================================================
      # Disabled modules
      # =====================================================

      ####################
      # Disabled modules #
      ####################

      username = {
        disabled = true;
      };
      hostname = {
        disabled = true;
      };
      localip = {
        disabled = true;
      };
      shlvl = {
        disabled = true;
      };
      singularity = {
        disabled = true;
      };
      kubernetes = {
        disabled = true;
      };
      vcsh = {
        disabled = true;
      };
      fossil_branch = {
        disabled = true;
      };
      fossil_metrics = {
        disabled = true;
      };
      git_commit = {
        disabled = true;
      };
      git_metrics = {
        disabled = true;
      };
      hg_branch = {
        disabled = true;
      };
      pijul_channel = {
        disabled = true;
      };
      docker_context = {
        disabled = true;
      };
      package = {
        disabled = true;
      };
      c = {
        disabled = true;
      };
      cmake = {
        disabled = true;
      };
      cobol = {
        disabled = true;
      };
      daml = {
        disabled = true;
      };
      dart = {
        disabled = true;
      };
      deno = {
        disabled = true;
      };
      dotnet = {
        disabled = true;
      };
      elixir = {
        disabled = true;
      };
      elm = {
        disabled = true;
      };
      erlang = {
        disabled = true;
      };
      fennel = {
        disabled = true;
      };
      guix_shell = {
        disabled = true;
      };
      haskell = {
        disabled = true;
      };
      haxe = {
        disabled = true;
      };
      helm = {
        disabled = true;
      };
      java = {
        disabled = true;
      };
      julia = {
        disabled = true;
      };
      kotlin = {
        disabled = true;
      };
      gradle = {
        disabled = true;
      };
      lua = {
        disabled = true;
      };
      nim = {
        disabled = true;
      };
      ocaml = {
        disabled = true;
      };
      opa = {
        disabled = true;
      };
      perl = {
        disabled = true;
      };
      php = {
        disabled = true;
      };
      pulumi = {
        disabled = true;
      };
      purescript = {
        disabled = true;
      };
      raku = {
        disabled = true;
      };
      rlang = {
        disabled = true;
      };
      red = {
        disabled = true;
      };
      scala = {
        disabled = true;
      };
      solidity = {
        disabled = true;
      };
      swift = {
        disabled = true;
      };
      typst = {
        disabled = true;
      };
      vlang = {
        disabled = true;
      };
      vagrant = {
        disabled = true;
      };
      zig = {
        disabled = true;
      };
      buf = {
        disabled = true;
      };
      conda = {
        disabled = true;
      };
      meson = {
        disabled = true;
      };
      spack = {
        disabled = true;
      };
      memory_usage = {
        disabled = true;
      };
      gcloud = {
        disabled = true;
      };
      openstack = {
        disabled = true;
      };
      azure = {
        disabled = true;
      };
      env_var = {
        disabled = true;
      };
      crystal = {
        disabled = true;
      };
      sudo = {
        disabled = true;
      };
      jobs = {
        disabled = true;
      };
      battery = {
        disabled = true;
      };
      status = {
        disabled = true;
      };
      os = {
        disabled = true;
      };
      container = {
        disabled = true;
      };
      shell = {
        disabled = true;
      };
    };
  };
}
