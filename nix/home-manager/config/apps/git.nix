_:

{
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
        email = "info@diegoa.ca";
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
      key = "4DF4C58193BBB0863AB37A6DC63945863D4B9E77";
      signByDefault = true;
      signer = "gpg";
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
    includes = [
      {
        condition = "gitdir:~/work/";
        contents = {
          user = {
            email = "diego.alvarez@chime.com";
            name = "Diego Alvarez";
            signingKey = "4DF4C58193BBB0863AB37A6DC63945863D4B9E77";
          };
          url = {
            "git@github.com:" = {
              insteadOf = "https://github.com/";
            };
          };
        };
      }
    ];
  };
}
