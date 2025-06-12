{ pkgs, base }:

# Office profile: extends `base` with company-specific tools.

{
  # ---------------- Homebrew ----------------
  taps = base.taps ++ [
    "1debit/chime"
  ];

  casks = base.casks ++ [
    "notion" # Notion workspace
    "slack" # Slack messenger
  ];

  brews = base.brews ++ [ ];
  masApps = base.masApps // { };

  # ---------------- Home-Manager packages ----------------
  hmPackages = base.hmPackages ++ (with pkgs; [ ]);
}
