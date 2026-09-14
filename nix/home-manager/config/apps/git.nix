{
  config,
  machineConfig,
  lib,
  ...
}:

{
  # SSH allowed signers for git signature verification
  # Generated from machineConfig.signing_identities - each profile declares what it trusts
  # Keep the declarative baseline separate. The helper records generated profile
  # keys in the local file; activation rebuilds the active trust store from both
  # sources so removed managed identities are revoked on the next switch.
  home.file.".ssh/allowed_signers.managed".text = lib.concatMapStringsSep "\n" (
    id: "${id.email} ${id.key}"
  ) machineConfig.signing_identities;

  home.activation.gitAllowedSigners = config.lib.dag.entryAfter [ "writeBoundary" ] ''
    MANAGED="$HOME/.ssh/allowed_signers.managed"
    LOCAL="$HOME/.ssh/allowed_signers.local"
    ACTIVE="$HOME/.ssh/allowed_signers"
    TMP="$(mktemp "$HOME/.ssh/.allowed_signers.XXXXXX")"

    cat "$MANAGED" > "$TMP"
    if [ -f "$LOCAL" ]; then
      printf '\n' >> "$TMP"
      cat "$LOCAL" >> "$TMP"
    fi
    chmod 600 "$TMP"
    mv "$TMP" "$ACTIVE"
  '';

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
          # Use the platform OpenSSH signer. It reads the profile key loaded
          # into the native ssh-agent, rather than invoking 1Password for each
          # commit.
          program = "/usr/bin/ssh-keygen";
          # This file starts with the managed historical keys and is extended
          # only by the explicit local bootstrap helper with the active public
          # key, so new signatures verify locally as well as on GitHub.
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
