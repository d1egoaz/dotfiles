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

# Update a PR body
gh api repos/OWNER/REPO/pulls/NUMBER -X PATCH -f body='...'

# Verify the PR body footer
gh pr view NUMBER --json body --jq '.body' | rg -n '^Assisted-by: .+ via .+$'
```

After activation, enroll the profile signer once:

```bash
ssh-add --apple-use-keychain -t 86400 ~/.ssh/codex-signing-${PROFILE}-ed25519
```

Use `gh api ... -X PATCH` for PR body updates; `gh pr edit --body` is
deprecated.

## Publication handoff

Bind checkout/branch, starting HEAD, exact owned/staged diff, allowed actions,
verification, and attribution evidence. Return blockers on unexpected state or
missing authority; do not repeat uncertain side effects.
