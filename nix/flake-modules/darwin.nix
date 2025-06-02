{ self, inputs, ... }:
{
  flake.darwinConfigurations =
    let
      # Helper function to create macOS systems
      mkDarwinSystem =
        {
          system ? "aarch64-darwin",
          user,
          hostname,
          hostCasks ? [ ],
        }:
        let
          pkgs = inputs.nixpkgs.legacyPackages.${system};
        in
        inputs.darwin.lib.darwinSystem {
          inherit system;
          modules = [
            # Apply basic nixpkgs config
            {
              nixpkgs.config.allowUnfree = true;
            }

            # Shared macOS system configuration with host-specific casks
            (import "${inputs.self}/shared/macos-system.nix" {
              inherit pkgs user;
              hostCasks = hostCasks;
            })

            # Home Manager integration
            inputs.home-manager.darwinModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                users.${user} = import "${inputs.self}/shared/macos-home.nix";
                extraSpecialArgs = { inherit inputs; };
              };
            }
          ];
          # Pass the user parameter to all modules
          specialArgs = { inherit user; };
        };
    in
    {
      # macOS System Configurations
      office-mbp = mkDarwinSystem {
        user = "diego.albeiroalvarezzuluag";
        hostname = "office-mbp";
        hostCasks = [
          "slack"
          "notion"
        ];
      };

      personal-mbp = mkDarwinSystem {
        user = "diego";
        hostname = "personal-mbp";
        hostCasks = [ "discord" ];
      };

      personal-mini = mkDarwinSystem {
        user = "diegoalvarez";
        hostname = "personal-mini";
        hostCasks = [ "discord" ];
      };
    };
}
