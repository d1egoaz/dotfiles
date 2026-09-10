## Validation runbook

Run checks from the dotfiles repository root, narrowing the set to changed
files where possible:

```fish
jq . config/codex/hooks.json >/dev/null
taplo check config/codex/config.toml
for file in config/codex/agents/*.toml; do taplo check "$file"; done
taplo check config/codex/profiles/personal.toml
test ! -f config/codex/profiles/work.local.toml || taplo check config/codex/profiles/work.local.toml
codex debug prompt-input hooks-json-smoke
bin/files/dotfiles-local-state check
just check
```

For a profile-concat check, write the shared config and profile fragment to a
temporary file, validate it with `taplo`, then remove that temporary file.
Validate skill frontmatter and `agents/openai.yaml` with PyYAML when available;
otherwise use Ruby YAML parsing. If the Codex prompt-input check cannot read
its session directory due to sandboxing, rerun the same command with scoped
escalation.
