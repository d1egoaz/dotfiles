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
  bot_email = "261106496+R2-Claw2@users.noreply.github.com";

  # Shared keys (referenced in multiple profiles)
  keys = {
    personal = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMokDMEcQ3ZatK2LLEJQOAs6CIxcklr3HT9IrYRu3A24";
    personal_signing = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAINoAtC71w/g4F0zEYxF9kvl/jkeJ30v2QZtNRK8E9etk";
    personal_signing_legacy = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHTEk1pRN0nd7IAVLqkgQvyNYmCqAVl37AQjz9yExiX8";
    r2claw2_bot = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIIBOaHihcw7t7AAUrDs3+yBawLuYcOJOTEi5+/SWZRBe";
    work = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGyQc3z3srPRhWbHoR9BmixXrfXmTECEw7YL4WklpKBT";
    work_signing = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJnBxAfxKcz/D7Pu1MestMop4PJ6uDTi/uY9EiAqPwY4";
  };
  emails = {
    personal = "info@diegoa.ca";
    work = "diego.alvarez@chime.com";
  };
  personal_signing_identities = [
    {
      email = emails.personal;
      key = keys.personal;
    } # legacy personal auth key
    {
      email = emails.personal;
      key = keys.personal_signing;
    } # personal signing key
    {
      email = emails.personal;
      key = keys.personal_signing_legacy;
    } # personal signing key retired 2026-09-21
  ];
in
{
  office = {
    # 1Password configuration
    op_account = "chimebank.1password.com";
    op_vault = "Employee";

    # Git identity
    git_name = "Diego Alvarez";
    git_email = emails.personal;
    work_email = emails.work;

    # Go private modules (for GOPRIVATE env var)
    go_private = "github.com/1debit/*";

    # Git commits use this profile's OpenSSH signing public key.
    git_signing_key = "~/.ssh/codex-signing-office-ed25519.pub";
    git_signing_private_key = ".ssh/codex-signing-office-ed25519";
    git_signing_use_keychain = true;

    # Git signature verification - all email+key pairs this profile trusts
    # Office machine works on both work repos AND personal repos (dotfiles)
    signing_identities = [
      {
        email = emails.personal;
        key = keys.personal;
      } # dotfiles from personal machine
      {
        email = emails.personal;
        key = keys.personal_signing;
      } # dotfiles from personal machine (signing key)
      {
        email = emails.personal;
        key = keys.personal_signing_legacy;
      } # historical dotfiles from personal machine (retired signing key)
      {
        email = emails.personal;
        key = keys.work;
      } # dotfiles from work machine
      {
        email = emails.personal;
        key = keys.work_signing;
      } # dotfiles from office signing key
      {
        email = emails.work;
        key = keys.work;
      } # work repos
      {
        email = emails.work;
        key = keys.work_signing;
      } # work repos signed on office
    ];

    # Work-specific paths and identifiers
    work_org = "1debit"; # GitHub organization for URL rewrites
    work_dir = "~/work"; # Base directory for work repositories
    emacs_additional_dir = "$HOME/dotfiles-private/chime"; # Private emacs config
    aws_region = "us-east-1"; # Default AWS region

    # LLM configuration (for Alfred workflows)
    # API key refs live in secrets/op-env-cache.yaml under op_env_cache_specs.
    llm = {
      provider = "OpenAI";
      model = "gpt-6-luna";
      base_url = "https://api.openai.com/v1";
    };
  };

  personal = {
    # 1Password configuration
    op_account = "my.1password.com";
    op_vault = "Private";

    # Git identity
    git_name = "Diego Alvarez";
    git_email = emails.personal;
    work_email = "";

    # No private Go modules
    go_private = "";

    # Git commits use this profile's OpenSSH signing public key.
    git_signing_key = "~/.ssh/codex-signing-personal-ed25519.pub";
    git_signing_private_key = ".ssh/codex-signing-personal-ed25519";
    git_signing_use_keychain = true;

    # Git signature verification - personal machine only needs personal key
    signing_identities = personal_signing_identities;

    # Personal profile doesn't need work-specific config
    work_org = "";
    work_dir = "";
    emacs_additional_dir = "";
    aws_region = ""; # Use AWS CLI default or profile-based

    # LLM configuration (for Alfred workflows)
    # API key refs live in secrets/op-env-cache.yaml under op_env_cache_specs.
    llm = {
      provider = "Cerebras";
      model = "zai-glm-4.7";
      base_url = "https://api.cerebras.ai/v1";
    };
  };

  # mac-mini4 is a dedicated automation host. Use the R2-Claw2 bot identity
  # globally while retaining the personal identities for signature verification.
  personal-mini = {
    git_name = "Wild Robot";
    git_email = bot_email;
    git_signing_key = "~/.ssh/r2claw2-bot.pub";
    git_signing_private_key = ".ssh/r2claw2-bot";
    git_signing_use_keychain = false;
    signing_identities = personal_signing_identities ++ [
      {
        email = bot_email;
        key = keys.r2claw2_bot;
      }
    ];
  };
}
