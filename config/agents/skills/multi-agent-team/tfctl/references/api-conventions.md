# tfctl API conventions

Use `tfctl api PATH --jq '<expr>'` for JSON extraction. `--jq` implies `--json`;
do not pass both or pipe to external `jq`. Use `--markdown` for human output.

Resolve placeholders with `-p`, for example:

```bash
tfctl api /workspaces/{workspace}/runs -p workspace=NAME --jq '.data[]'
```

Workspace children use `/workspaces/{workspace}/...`: vars, runs, varsets,
remote-state-consumers, configuration-versions, notification-configurations,
and state-versions. Only the workspace resource itself uses
`/organizations/{organization}/workspaces/NAME`. Policy checks are run-scoped:
`/runs/{run-id}/policy-checks`.

Do not try org-nested workspace child paths, `/workspaces/{workspace}/policy-checks`,
or `/organizations/{org}/varsets`; use the shapes above and the full
`{organization}` placeholder.

Responses are JSON:API envelopes. Read resource fields from `data.attributes`
and links from `data.relationships.<name>.data`. `null` is final. Use `--all`
for pagination, or `--page-size 1 --jq '.meta.pagination.["total-count"]'` for
counts.

Smart defaults may resolve `{organization}` from the active profile and
`{workspace}` from a local `cloud {}` block. Existing `ws-...`, `run-...`,
`team-...`, `prj-...`, and `varset-...` IDs pass through unchanged.

Discover unknown operations only when needed:

```bash
tfctl api schema search "KEYWORD" --json
tfctl api schema get OPERATION_ID
```
