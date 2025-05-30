# treefmt.nix
{ pkgs, ... }:
{
  # Used to find the project root
  projectRootFile = "flake.nix";

  # Enable formatters for different file types
  programs = {
    # Nix formatting
    nixfmt.enable = true;
    nixfmt.package = pkgs.nixfmt-rfc-style;

    # Shell script formatting
    shfmt.enable = true;
    shellcheck.enable = true;

    # YAML formatting
    yamlfmt.enable = true;

    # JSON formatting
    prettier = {
      enable = true;
      includes = [
        "*.json"
        "*.md"
      ];
    };

    # TOML formatting
    taplo.enable = true;
  };

  # Additional settings can be configured here
  settings = {
    # Global excludes
    global.excludes = [
      # Ignore generated files
      "flake.lock"
      # Ignore hidden directories
      ".git/"
      ".direnv/"
    ];

    # Formatter-specific excludes can be added here if needed
    # formatter.nixfmt.excludes = [ "some-file.nix" ];
  };
}
