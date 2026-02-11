{
  office = {
    op_account = "CHANGE_ME.1password.com";
    op_vault = "CHANGE_ME";
    work_email = "CHANGE_ME@example.com";
    go_private = "github.com/CHANGE_ME/*";
    # SSH signing key from 1Password (the public key string)
    # Create with: op item create --vault <vault> --category "SSH Key" --title "github-work-auth" --generate-password='[recipe:ed25519]'
    # Retrieve with: op item get "github-work-auth" --vault <vault> --fields "public key"
    ssh_signing_key = "ssh-ed25519 AAAA...";
  };
  personal = {
    op_account = "my.1password.com";
    op_vault = "Private";
    work_email = "";
    go_private = "";
    # SSH signing key from 1Password (the public key string)
    ssh_signing_key = "ssh-ed25519 AAAA...";
  };
}
