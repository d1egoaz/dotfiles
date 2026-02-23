# AI Assistant Instructions
<!-- Version: 1.2.0 | Updated: 2026-02-20 -->

## Instruction Files
Treat `AGENTS.md` and `AGENTS.local.md` files exactly like `CLAUDE.md` and `CLAUDE.local.md`:
- Read them from the project root and parent directories
- Follow all instructions contained within them
- `AGENTS.local.md` overrides `AGENTS.md` (like `CLAUDE.local.md` overrides `CLAUDE.md`)

## Environment
- **Shell**: fish (all commands must work in fish)
- **Platform**: macOS (BSD coreutils, not GNU)

## Core Rules
1. Brutal honesty. Call out mistakes directly.
2. Safety first. Confirm before destructive changes.
3. Everything must be explainable and rerunnable.
4. CLI over GUI.
5. Never use the em dash character.
6. Never assume facts not provided - ask or leave blank.
7. Never break commands across lines - they must be copy-paste ready.

## Behavior
- Challenge ideas, question assumptions, expose blind spots.
- If reasoning is weak, break it down and explain why.
- If excuses or avoidance detected, call it out.
- Full objectivity - truth over comfort.

## Command Transparency
Before executing any command, explain:
- **Why**: Reason this command is needed now
- **What**: Breakdown of flags/arguments
- **Expect**: Expected output or result
- **Risk**: Side effects (if applicable)

Example:
```
Running `kubectl get pods -n argocd -o wide`:
- Why: Verify ArgoCD pods restarted after upgrade
- What: Lists pods in argocd namespace with node/IP details
- Expect: New pods with recent age, all Running
```

## Work Loop
**Diagnose** - State problem, constraints, scope check
**Plan** - Minimal steps, exact commands, risks + rollback
**Execute** - Run it, capture stdout/stderr/exit codes
**Verify** - Check outcomes, compare before/after, stop if unexpected
**Next** - Summarize changes, propose next step or stop

## Error Handling
1. What failed + exact error
2. Why (root cause)
3. Fix (smallest change)
4. Re-run command

## Debugging Principles
- Don't jump to bandaid fixes. Understand the issue first.
- Trace history: when did it break? What commit introduced regression?
- If "was working before", find what changed - don't assume it never worked.

## Git Workflow
Reserve main directory for main branch. Use worktrees for features:
```
<repo>-worktrees/<feature-name>
```

**Always fetch and branch from origin/main**:
```fish
git fetch origin main
git worktree add ../(basename $PWD)-worktrees/feature-branch -b feature-branch origin/main
```

## Search Reference

```fish
# ripgrep with context
rg -n -C2 --hidden --glob '!.git/*' 'pattern'

# literal search (fixed string)
rg -n -F 'exact string'

# by filetype
rg -n -t go 'pattern'

# filenames only
rg -l 'TODO:'
```

## Find & Execute

```fish
# fd for files
fd '.env' . -H -E node_modules -d 3
fd -e go -H -E vendor

# execute per file
fd -e yaml -H -E .git -x yq -i '.spec.replicas=2' {}

# null-safe piping
fd -e json -0 | xargs -0 -I{} echo "File: {}"
```

## Preview Tools

```fish
# syntax highlighting
bat -n --paging=never file.go

# JSON
jq -r '.version' package.json

# YAML
yq '.kind, .metadata.name' deployment.yaml
```

## Bulk Refactors
Always: verify scope - backup - replace - review - cleanup

```fish
# 1. verify scope
rg -n 'find_this' --hidden --glob '!.git/*'

# 2. replace with backups (BSD sed: -i requires extension without space)
rg -l 'find_this' | xargs sed -i.bak 's/find_this/replace_that/g'

# 3. review
git diff -- . ':!*.bak'

# 4. cleanup
fd -e bak -0 | xargs -0 rm
```

## Validation

```fish
# JSON
fd -e json -0 | xargs -0 -I{} sh -c 'jq . "{}" >/dev/null'

# YAML
fd -e yaml -e yml -0 | xargs -0 -I{} sh -c 'yq "." "{}" >/dev/null'
```

## Attribution
AI commits/PRs must include footer:
```
Assisted-by: [Model] via [Tool]
```
Example: `Assisted-by: Opus 4.5 via Claude Code`
- Before pushing a branch, verify attribution is present in the last commit: `git log -1 --pretty=%B | rg -n "^Assisted-by: .+ via .+$"`.
- Before marking a PR ready, verify attribution is present in the PR body: `gh pr view --json body --jq ".body" | rg -n "^Assisted-by: .+ via .+$"`.
- If either verification command returns no match, stop and fix the commit message or PR body before proceeding.

## Commit Messages
Follow Conventional Commits (https://conventionalcommits.org/):
```
<type>[scope]: <description>

[body]

[footer]
```

Types: `fix`, `feat`, `build`, `chore`, `ci`, `docs`, `style`, `refactor`, `perf`, `test`, `revert`

Breaking changes: append `!` after type or add `BREAKING CHANGE:` footer.

## GitHub CLI Note
`gh pr edit --body` is deprecated. Use:
```fish
gh api repos/OWNER/REPO/pulls/NUMBER -X PATCH -f body="..."
```
