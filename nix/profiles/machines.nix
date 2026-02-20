# Machine-specific configuration by profile.
#
# This file contains per-profile settings that differ between work and personal
# machines. Nothing here is actually secret:
#   - Public SSH keys (by definition, public)
#   - Email addresses (public)
#   - 1Password account domains (not sensitive)
#   - Organization names (public)
#   - LLM provider config (public APIs)
#
# Edit this file directly when:
#   - Setting up a new machine profile
#   - Rotating SSH signing keys
#   - Changing 1Password vault configuration
#   - Adding work-specific paths or org names
#   - Changing LLM provider/model for Alfred workflows
let
  # Shared keys (referenced in multiple profiles)
  keys = {
    personal = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMokDMEcQ3ZatK2LLEJQOAs6CIxcklr3HT9IrYRu3A24";
    work = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGyQc3z3srPRhWbHoR9BmixXrfXmTECEw7YL4WklpKBT";
  };
  emails = {
    personal = "info@diegoa.ca";
    work = "diego.alvarez@chime.com";
  };
in
{
  office = {
    # 1Password configuration
    op_account = "chimebank.1password.com";
    op_vault = "Employee";

    # Git identity
    personal_email = emails.personal;
    work_email = emails.work;

    # Go private modules (for GOPRIVATE env var)
    go_private = "github.com/1debit/*";

    # SSH signing key for this profile (used for signing commits)
    ssh_signing_key = keys.work;

    # Git signature verification - all email+key pairs this profile trusts
    # Office machine works on both work repos AND personal repos (dotfiles)
    signing_identities = [
      {
        email = emails.personal;
        key = keys.personal;
      } # dotfiles from personal machine
      {
        email = emails.personal;
        key = keys.work;
      } # dotfiles from work machine
      {
        email = emails.work;
        key = keys.work;
      } # work repos
    ];

    # Work-specific paths and identifiers
    work_org = "1debit"; # GitHub organization for URL rewrites
    work_dir = "~/work"; # Base directory for work repositories
    emacs_additional_dir = "$HOME/dotfiles-private/chime"; # Private emacs config
    aws_region = "us-east-1"; # Default AWS region

    # LLM configuration (for Alfred workflows)
    # key_item must exist in op_vault above
    llm = {
      provider = "OpenAI";
      model = "gpt-5-nano";
      base_url = "https://api.openai.com/v1";
      key_item = "OpenAI API";
    };
  };

  personal = {
    # 1Password configuration
    op_account = "my.1password.com";
    op_vault = "Private";

    # Git identity
    personal_email = emails.personal;
    work_email = "";

    # No private Go modules
    go_private = "";

    # SSH signing key for this profile
    ssh_signing_key = keys.personal;

    # Git signature verification - personal machine only needs personal key
    signing_identities = [
      {
        email = emails.personal;
        key = keys.personal;
      }
    ];

    # Personal profile doesn't need work-specific config
    work_org = "";
    work_dir = "";
    emacs_additional_dir = "";
    aws_region = ""; # Use AWS CLI default or profile-based

    # LLM configuration (for Alfred workflows)
    # key_item must exist in op_vault above
    llm = {
      provider = "Cerebras";
      model = "gpt-oss-120b";
      base_url = "https://api.cerebras.ai/v1";
      key_item = "Cerebras API";
    };
  };
}
