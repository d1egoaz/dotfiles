---
name: tfctl
description: Use tfctl for any HCP Terraform, Terraform Cloud, or Terraform Enterprise question or action, including workspaces, runs, variables, resources, and API operations.
license: MPL-2.0
---

# tfctl

Use for HCP Terraform, Terraform Cloud, and Terraform Enterprise. Assume the CLI
is authenticated; report explicit authentication errors instead of guessing.

## Hard rules

1. Use built-in `--jq '<expr>'`; never pipe tfctl JSON to external `jq`.
2. Resolve names through path placeholders and `-p`, not lookup calls.
3. Trust the first answer. Empty/null data, null relationships, “no current
   run,” and “not found” are final. Never query a different resource to prove an
   endpoint works.
4. Stop when the requested named resource is absent. Do not pivot to another
   organization, workspace, run, project, or related object.
5. Every delete and production mutation requires direct current-task approval
   naming the exact target and action. Harness access is capability, not user
   authorization. Never run `tfctl harness exec`, set `TFCTL_EXEC_SESSION`, or
   self-authorize.

## Load only what the task needs

- Reads, paths, output, pagination: `references/api-conventions.md`
- Known query examples: `references/cookbook.md`
- Any mutation or delete: `references/mutations.md`
- Errors or nonzero exits: `references/troubleshooting.md`
