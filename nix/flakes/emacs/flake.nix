{
  description = "Custom Emacs configuration with native compilation and patches";

  inputs = {
    nixpkgs = { };
    emacs-overlay = { };
    flake-utils = { };
  };

  outputs =
    {
      self,
      nixpkgs,
      emacs-overlay,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        # Import nixpkgs for the specific system AND apply the overlay
        pkgs = import nixpkgs {
          inherit system;
          overlays = [ emacs-overlay.overlay ];
        };
        # --- Explicitly enable native compilation ---
        # Get the emacs-unstable from the overlay and override its build arguments
        my-emacs-with-native-comp =
          (pkgs.emacs-unstable.override {
            withImageMagick = true;
            withMailutils = true;
            # 2025-05-12 https://github.com/NixOS/nixpkgs/issues/395169
            # withNativeCompilation = true;
            withTreeSitter = true;
            withWebP = true;
          }).overrideAttrs
            (old: {
              patches = (old.patches or [ ]) ++ [
                (pkgs.fetchpatch {
                  url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-30/round-undecorated-frame.patch";
                  sha256 = "sha256-uYIxNTyfbprx5mCqMNFVrBcLeo+8e21qmBE3lpcnd+4=";
                })
                (pkgs.fetchpatch {
                  url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-30/system-appearance.patch";
                  sha256 = "sha256-3QLq91AQ6E921/W9nfDjdOUWR8YVsqBAT/W9c1woqAw=";
                })
                (pkgs.fetchpatch {
                  url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/fix-window-role.patch";
                  sha256 = "sha256-+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
                })
              ];
            });
        # -------------------------------------------
      in
      {
        # Expose the custom emacs package
        packages.default = my-emacs-with-native-comp;
      }
    );
}
