{ pkgs, ... }:

let
  yamlFormat = pkgs.formats.yaml { };
in
{
  # Homebrew owns the GitHub CLI binary. Home Manager keeps its configuration
  # and Git credential-helper wiring declarative without installing pkgs.gh.
  xdg.configFile."gh/config.yml".source = yamlFormat.generate "gh-config.yml" {
    version = "1";
    editor = "";
    git_protocol = "https";
    aliases = {
      pc = "pr checkout";
      pv = "pr view";
    };
  };

  programs.git.settings.credential = {
    "https://github.com".helper = [
      ""
      "/opt/homebrew/bin/gh auth git-credential"
    ];
    "https://gist.github.com".helper = [
      ""
      "/opt/homebrew/bin/gh auth git-credential"
    ];
  };
}
