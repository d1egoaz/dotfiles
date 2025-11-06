{ pkgs, base }:
{
  # ---------------- System packages ----------------
  systemPackages =
    base.systemPackages
    ++ (with pkgs; [
    ]);

  # ---------------- Home-Manager packages ----------------
  hmPackages =
    base.hmPackages
    ++ (with pkgs; [
    ]);
}
