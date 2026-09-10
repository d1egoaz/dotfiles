## Configuration layering

Codex does not document a general `include`, `import`, or `config.d` mechanism
for `config.toml`. Compose the shared config with the selected profile fragment
through the repository's supported activation workflow.

- Newer Codex versions do not use a top-level `profile = "name"` selector in
  `config.toml`.
- A profile fragment must be empty/comment-only or begin with a TOML section
  header. Validate this before concatenation so keys cannot land in the wrong
  section.
- Keep machine-only and work-only endpoints, project trust, hooks, approvals,
  and skills in ignored local fragments. If a local fragment is missing, use
  the documented fallback and report the warning.
- A shared parent `.codex/config.toml` is unreliable for nested Git repos when
  each nested `.git` resolves as a separate root. Use the repo-local config or
  a managed symlink when one source of truth is required.

Local encrypted-state entries are generic resources. Use the repository's
local-state command to sync them; activation restores missing files but does
not replace encrypted state or make plaintext files runtime-decrypted.
