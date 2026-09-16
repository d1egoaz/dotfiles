# tfctl mutations

Every delete and production mutation requires direct user authorization naming
the exact target and action in the current task. A harness capability is not
authorization. Never run `tfctl harness exec`, set `TFCTL_EXEC_SESSION`, or
self-authorize.

Use `--dry-run` before a supported mutation when available.

```bash
# Start a run
tfctl run start NAME_OR_ID

# Create or update one variable
tfctl api /workspaces/{workspace}/vars -p workspace=NAME \
  -a key=VARKEY -a value=VALUE -a category=env

# Add remote-state consumers
tfctl api /workspaces/{workspace}/relationships/remote-state-consumers \
  -p workspace=NAME -i '{"data":[{"type":"workspaces","id":"ws-CONSUMER"}]}'

# Apply a variable set
tfctl api /workspaces/{workspace}/relationships/varsets -p workspace=NAME \
  -X POST -i '{"data":[{"type":"varsets","id":"varset-ID"}]}'

# Delete
tfctl api PATH -X DELETE

# Raw PATCH
tfctl api /workspaces/{workspace}/X -X PATCH -p workspace=NAME \
  -i '{"data":{"type":"X","attributes":{...}}}'
```

HCP Terraform does not support bulk variable POSTs; use separate calls. If
tfctl refuses a delete because a harness grant is missing, report the refusal
and printed command. Treat it as a capability gap, not expired authentication.
Never retry an uncertain mutation without first reading current state.
