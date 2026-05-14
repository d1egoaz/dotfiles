{
  config,
  lib,
  machineConfig,
  profile,
  ...
}:

{
  # ============================================================================
  # Alfred Configuration
  # ============================================================================
  # Public values (model, provider, endpoint, cache spec name) are baked in at
  # build time. 1Password references live in SOPS because Alfred workflows don't
  # inherit shell env vars.
  # ============================================================================

  # Generated config for Alfred text rewrite workflows. Runtime logic lives in
  # bin/files/alfred-llm-rewrite so the script is testable without Nix escaping.
  home.file = {
    ".local/bin/alfred-openai-rewrite".source =
      config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dotfiles/bin/files/alfred-llm-rewrite";

    ".config/alfred-llm-rewrite/config.env".text = ''
      OP_ACCOUNT=${lib.escapeShellArg machineConfig.op_account}
      BASE_URL=${lib.escapeShellArg machineConfig.llm.base_url}
      MODEL=${lib.escapeShellArg machineConfig.llm.model}
      PROVIDER=${lib.escapeShellArg machineConfig.llm.provider}
      CACHE_NAME=${lib.escapeShellArg "alfred-llm-rewrite-${profile}"}
      SECRET_ENV=ALFRED_LLM_API_KEY
      SOPS_SPEC_FILE="$HOME/dotfiles/secrets/codex.yaml"
    '';
  };

  # Set Alfred preferences folder to dotfiles location (office only)
  # This runs at login to ensure Alfred knows where to find its synced preferences
  launchd.agents.alfred-set-preferences = lib.mkIf (profile == "office") {
    enable = true;
    config = {
      ProgramArguments = [
        "/usr/bin/defaults"
        "write"
        "com.runningwithcrayons.Alfred-Preferences"
        "syncfolder"
        "${config.home.homeDirectory}/dotfiles/alfred"
      ];
      RunAtLoad = true;
    };
  };
}
