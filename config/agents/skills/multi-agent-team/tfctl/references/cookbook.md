# tfctl cookbook

```bash
# Workspace count and search
tfctl api /organizations/{organization}/workspaces --page-size 1 --jq '.meta.pagination.["total-count"]'
tfctl api /organizations/{organization}/workspaces -f 'search[name]=TERM' --jq '.data[] | {id, name: .attributes.name, current_run: .relationships.["current-run"].data}'
tfctl api /organizations/{organization}/workspaces/NAME --jq '.data.relationships.["current-run"].data.id'

# Filter workspaces
tfctl api /organizations/{organization}/workspaces --all --jq '.data[] | select(.attributes.["terraform-version"] | startswith("1.8")) | .attributes.name'
tfctl api /organizations/{organization}/workspaces --all --jq '.data[] | select(.attributes.name | test("^temp-|^old-") | not) | .attributes.name'
tfctl api /organizations/{organization}/workspaces --all --jq '.data[] | select(.attributes.["vcs-repo"] != null and .attributes.["vcs-repo"].identifier == "org/repo") | {id, name: .attributes.name}'

# Current run and run list
tfctl run status NAME_OR_ID
tfctl api /workspaces/{workspace}/runs -p workspace=NAME --jq '.data[] | {id, status: .attributes.status}'
tfctl api /workspaces/{workspace}/runs -p workspace=NAME --all --jq '[.data[].attributes.status] | group_by(.) | map({status: .[0], count: length})'

# Variables, state, configuration versions, notifications
tfctl api /workspaces/{workspace}/vars -p workspace=NAME --jq '.data[] | {key: .attributes.key, category: .attributes.category, sensitive: .attributes.sensitive}'
tfctl api /workspaces/{workspace}/current-state-version -p workspace=NAME --jq '.data | {serial: .attributes.serial, status: .attributes.status}'
tfctl api /workspaces/{workspace}/configuration-versions -p workspace=NAME --jq '.data[] | {id, source: .attributes.source}'
tfctl api /workspaces/{workspace}/notification-configurations -p workspace=NAME --jq '.data[] | {id, type: .attributes.["destination-type"]}'

# Variable sets and policy checks
tfctl api /organizations/{organization}/varsets --all --jq '.data[] | {name: .attributes.name, id: .id}'
tfctl api /organizations/{organization} --jq '.data | {id, name: .attributes.name, created_at: .attributes.["created-at"], terraform_version_default: .attributes.["terraform-version"]}'
tfctl api /runs/{run-id}/policy-checks --jq '.data[] | {id, status: .attributes.status, enforced: .attributes.["enforcement-level"]}'

# Plan/apply log URL for a completed run
tfctl api /runs/RUN_ID --jq '.data.relationships | {plan: .plan.data.id, apply: .apply.data.id}'
tfctl api /plans/PLAN_ID --jq '.data.attributes.["log-read-url"]'

# Team access requires team resolution, then team-workspaces
tfctl api /organizations/{organization}/teams -f 'filter[names]=TEAM' --jq '.data[0].id'
tfctl api /team-workspaces -f 'filter[team][id]=TEAM_ID' --jq '.data[] | {workspace_id: .relationships.workspace.data.id, access: .attributes.access}'
```

Use server-side filters when available. Empty results are the answer; never
pivot to a different organization, workspace, run, or resource to prove the
endpoint works.
