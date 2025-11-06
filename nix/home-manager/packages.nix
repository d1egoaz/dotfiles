{
  pkgs,
  profile,
  ...
}:

# Ensure profile is valid (compile-time check).
assert builtins.elem profile [
  "personal"
  "office"
];

# ---------------------------------------------------------------------------
# Package Sets
# ---------------------------------------------------------------------------
# The `profile` argument (passed through `specialArgs` in mkDarwinSystem) is
# used to decide which of the profile-specific sets is merged with the common
# packages.
# ---------------------------------------------------------------------------

let
  base = import ../profiles/base.nix { inherit pkgs; };
  cfg = import ../profiles/${profile}.nix { inherit pkgs base; };
in
{
  home.packages = cfg.hmPackages;
}
