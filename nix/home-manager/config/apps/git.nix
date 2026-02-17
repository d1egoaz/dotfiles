{
  machineConfig,
  lib,
  profile,
  ...
}:

{
  # SSH allowed signers for git signature verification
  # Generated from machineConfig.signing_identities - each profile declares what it trusts
  home.file.".ssh/allowed_signers".text = lib.concatMapStringsSep "\n" (
    id: "${id.email} ${id.key}"
  ) machineConfig.signing_identities;

  # Work SSH public key for 1Password agent matching (office profile only)
  # This allows SSH config to specify which key to use for work repos
  home.file.".ssh/github-work-auth.pub" = lib.mkIf (profile == "office") {
    text = "${machineConfig.ssh_signing_key}\n";
  };

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
      key = machineConfig.ssh_signing_key;
      signByDefault = true;
    };

    settings = {
      gpg = {
        format = "ssh";
        ssh = {
          program = "/Applications/1Password.app/Contents/MacOS/op-ssh-sign";
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
            signingKey = machineConfig.ssh_signing_key;
          };
          url = {
            # Work repos: use work SSH key (handles both HTTPS and SSH URLs)
            "git@github.com-work:${machineConfig.work_org}/" = {
              insteadOf = "https://github.com/${machineConfig.work_org}/";
            };
            "git@github.com-work:" = {
              insteadOf = "git@github.com:${machineConfig.work_org}/";
            };
            # Other repos: HTTPS -> SSH
            "git@github.com:" = {
              insteadOf = "https://github.com/";
            };
          };
        };
      }
    ];
  };
}
