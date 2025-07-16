{ pkgs, base }:

{
  # ---------------- Homebrew ----------------
  taps = base.taps ++ [
    "1debit/chime"
  ];

  casks = base.casks ++ [
    "notion" # Notion workspace
    "slack" # Slack messenger
  ];

  brews = base.brews ++ [
  ];

  # // is the same as lib.trivial.mergeAttrs
  masApps = base.masApps // { };

  # ---------------- System packages ----------------
  systemPackages =
    base.systemPackages
    ++ (with pkgs; [
    ]);

  # ---------------- Home-Manager packages ----------------
  hmPackages = base.hmPackages ++ (with pkgs; [ ]);
}
