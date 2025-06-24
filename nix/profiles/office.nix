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
    "compass"
  ];

  masApps = base.masApps // { };

  # ---------------- System packages ----------------
  systemPackages = base.systemPackages ++ (with pkgs; [
  ]);

  # ---------------- Home-Manager packages ----------------
  hmPackages = base.hmPackages ++ (with pkgs; [ ]);
}
