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
            # Apply basic nixpkgs config with emacs overlay
            {
              nixpkgs.config.allowUnfree = true;
              nixpkgs.overlays = [ inputs.emacs-overlay.overlays.default ];
            }

            # macOS system configuration
            (import ../systems/darwin/default.nix {
              inherit pkgs user;
              hostCasks = hostCasks;
            })

            # Home Manager integration
            inputs.home-manager.darwinModules.home-manager
            {
              home-manager = {
                useGlobalPkgs = true;
                useUserPackages = true;
                users.${user} = import ../home-manager;
                extraSpecialArgs = { inherit inputs user; };
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
        hostCasks = [
          "discord"
          "nordvpn"
        ];
      };

      personal-mini = mkDarwinSystem {
        user = "diegoalvarez";
        hostname = "personal-mini";
        hostCasks = [
          "discord"
          "nordvpn"
        ];
      };
    };
}
