# Signed publication commands

Use Conventional Commit types: `fix`, `feat`, `build`, `chore`, `ci`, `docs`,
`style`, `refactor`, `perf`, `test`, or `revert`. Mark breaking changes with `!`
or a `BREAKING CHANGE:` footer.

```text
<type>[scope]: <description>

[body]

Assisted-by: [Exact model identifier] via [Tool]
```

```bash
# Verify the commit footer and signature
git log -1 --pretty=%B | rg -n '^Assisted-by: .+ via .+$'
git verify-commit HEAD

# Update a PR body (gh pr edit --body is deprecated)
gh api repos/OWNER/REPO/pulls/NUMBER -X PATCH -f body='...'

# Verify the PR body footer
gh pr view NUMBER --json body --jq '.body' | rg -n '^Assisted-by: .+ via .+$'
```

## Signing fails

A passphrase prompt from `git commit -S` means the signing key is not in the
agent that `SSH_AUTH_SOCK` points to. GitHub SSH uses 1Password through
`IdentityAgent`; signing uses the native macOS agent. Some hosts start
sessions with a stale 1Password `SSH_AUTH_SOCK`, so compare both agents:

```bash
ssh-add -l
SSH_AUTH_SOCK="$(launchctl getenv SSH_AUTH_SOCK)" ssh-add -l
```

If the key is missing from the native agent, the user enrolls it once
(interactive passphrase):

```bash
SSH_AUTH_SOCK="$(launchctl getenv SSH_AUTH_SOCK)" ssh-add --apple-use-keychain -t 86400 ~/.ssh/codex-signing-${PROFILE}-ed25519
```

Then sign through the native agent for that command only:

```bash
SSH_AUTH_SOCK="$(launchctl getenv SSH_AUTH_SOCK)" git commit -S
```
