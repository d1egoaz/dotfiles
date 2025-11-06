{ config, ... }:

let
  homeDir = config.home.homeDirectory;
in
{
  # Use sops-nix to manage secrets across all profiles.
  sops = {
    # Use your GnuPG home so sops-nix can find the private key (PGP recipients)
    gnupg.home = "${homeDir}/.gnupg";

    # Do not use SSH keys for decryption in HM context
    gnupg.sshKeyPaths = [ ];
    age.sshKeyPaths = [ ];

    defaultSopsFile = ../../profiles/office.private.sops.json;

    # decrypted secret symlinks
    defaultSymlinkPath = "${homeDir}/.local/state/sops-nix/secrets";

    secrets.OPENAI_API_KEY = { };
    secrets.HOMEBREW_GITHUB_API_TOKEN = { };
  };
}
