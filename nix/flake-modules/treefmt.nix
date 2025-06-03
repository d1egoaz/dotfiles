{ inputs, ... }:
{
  imports = [
    inputs.treefmt-nix.flakeModule
  ];

  perSystem =
    { pkgs, ... }:
    {
      # Treefmt configuration for code formatting
      treefmt = {
        # Use the project root as reference
        projectRootFile = "flake.nix";

        # Configure formatters
        programs = {
          # Nix code formatting
          nixfmt = {
            enable = true;
            package = pkgs.nixfmt-rfc-style;
          };

          # Shell script formatting
          shfmt.enable = true;
          shellcheck.enable = true;

          # Lua formatting
          stylua.enable = true;

          # YAML formatting
          yamlfmt.enable = true;

          # TOML formatting
          taplo.enable = true;

          # Prettier for various file types
          prettier = {
            enable = true;
            includes = [
              "*.json"
              "*.md"
              "*.yaml"
              "*.yml"
            ];
          };
        };

        # Additional settings
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
      };
    };
}
