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

          # Prettier for various file types
          prettier = {
            enable = true;
            includes = [
              "*.md"
              "*.json"
              "*.yaml"
              "*.yml"
            ];
          };
        };
      };
    };
}
