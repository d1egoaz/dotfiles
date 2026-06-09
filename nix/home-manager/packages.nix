{ profileCfg, ... }:

# ---------------------------------------------------------------------------
# Package Sets
# ---------------------------------------------------------------------------
# profileCfg (base + selected profile, merged in lib/mkDarwinSystem.nix and
# passed through extraSpecialArgs) is the single source of truth for package
# sets; do not re-import the profile files here.
# ---------------------------------------------------------------------------

{
  home.packages = profileCfg.hmPackages;
}
