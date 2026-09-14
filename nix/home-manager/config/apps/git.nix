{
  machineConfig,
  lib,
  ...
}:

{
  # Generated from the profile's declared verification identities.
  home.file.".ssh/allowed_signers".text = lib.concatMapStringsSep "\n" (
    id: "${id.email} ${id.key}"
  ) machineConfig.signing_identities;

  # Delta is now a separate program in Home Manager 25.11
  programs.delta = {
    enable = true;
    enableGitIntegration = true;
    options = {
      dark = true;
      line-numbers = true;
      navigate = true;
      side-by-side = false;
      syntax-theme = "tokyonight_night";
      plus-color = "#004400";
      minus-color = "#440000";
    };
  };

  programs.git = {
    enable = true;

    settings = {
      user = {
        name = "Diego Alvarez";
        email = machineConfig.personal_email;
      };
      init = {
        defaultBranch = "main";
      };
      branch = {
        sort = "-committerdate";
      };
      core = {
        commentChar = "@";
      };
      diff = {
        colorMoved = "default";
      };
      fetch = {
        prune = true;
        pruneTags = true;
      };
      github = {
        user = "d1egoaz";
      };
      merge = {
        conflictStyle = "zdiff3";
      };
      pull = {
        ff = "only";
      };
      push = {
        default = "current";
      };
      rebase = {
        updateRefs = true;
      };
      maintenance = {
        gc = {
          enabled = true;
          schedule = "weekly";
        };
      };
      advice = {
        detachedHead = false;
      };
    };

    signing = {
      key = machineConfig.git_signing_key;
      signByDefault = true;
    };

    settings = {
      gpg = {
        format = "ssh";
        ssh = {
          # Use the platform OpenSSH signer, not 1Password.
          program = "/usr/bin/ssh-keygen";
          allowedSignersFile = "~/.ssh/allowed_signers";
        };
      };
    };

    # Global gitignore
    ignores = [
      ".DS_STORE"
      "TAGS"
      ".claude/settings.local.json"
    ];

    # Git attributes for different file types
    attributes = [
      "*.el diff=lisp"
      "*.go diff=golang"
      "*.lisp diff=lisp"
      "*.md diff=markdown"
      "*.rb diff=ruby"
      "*.rs diff=rust"
    ];

    # Include work-specific config for work directories
    includes = lib.optionals (machineConfig.work_org != "") [
      {
        condition = "gitdir:${machineConfig.work_dir}/";
        contents = {
          user = {
            email = machineConfig.work_email;
            name = "Diego Alvarez";
            signingKey = machineConfig.git_signing_key;
          };
        };
      }
    ];
  };
}
