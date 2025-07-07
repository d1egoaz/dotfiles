{ pkgs, ... }:

let
  # ================================================================
  # macOS Enhancement Patches
  # ================================================================

  # Patches from homebrew-emacs-plus for better macOS integration
  macosPatches = [
    # Round undecorated frame corners for better macOS appearance
    (pkgs.fetchpatch {
      name = "round-undecorated-frame.patch";
      url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-30/round-undecorated-frame.patch";
      sha256 = "sha256-uYIxNTyfbprx5mCqMNFVrBcLeo+8e21qmBE3lpcnd+4=";
    })

    # System appearance integration (dark/light mode detection)
    (pkgs.fetchpatch {
      name = "system-appearance.patch";
      url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-30/system-appearance.patch";
      sha256 = "sha256-3QLq91AQ6E921/W9nfDjdOUWR8YVsqBAT/W9c1woqAw=";
    })

    # Fix window role for proper macOS window management
    (pkgs.fetchpatch {
      name = "fix-window-role.patch";
      url = "https://raw.githubusercontent.com/d12frosted/homebrew-emacs-plus/master/patches/emacs-28/fix-window-role.patch";
      sha256 = "sha256-+z/KfsBm1lvZTZNiMbxzXQGRTjkCFO4QPlEK35upjsE=";
    })
  ];

  # ================================================================
  # Custom Emacs Build with macOS Enhancements
  # ================================================================

  # Build Emacs with native compilation and macOS-specific patches
  customEmacs = pkgs.emacs-unstable.override {
    # Enable multimedia and modern features
    withImageMagick = false;
    withMailutils = false;
    withTreeSitter = true;
    withWebP = true;
    # Native compilation disabled due to nixpkgs issue #395169
    withNativeCompilation = true;
  };

in
# Apply macOS enhancement patches from homebrew-emacs-plus
customEmacs.overrideAttrs (old: {
  patches = (old.patches or [ ]) ++ macosPatches;
})
