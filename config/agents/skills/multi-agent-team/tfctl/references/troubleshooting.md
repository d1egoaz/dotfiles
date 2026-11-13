# tfctl troubleshooting

| Code | Meaning | Action |
|---|---|---|
| 0 | Success | Stop |
| 1 | Informational or usage error | Read stderr; “no current run” is final |
| 2 | Not found or invalid auth | Verify the requested identifier; do not try another resource |
| 3 | Expired or invalid token | Run `tfctl auth status` (and `--profile` if one is set) and report it; ask whether the user already has a long-lived token before handing them `tfctl auth login` |
| 4 | Network error | Retry after a brief delay |
| 5 | Rate limit or server error | Retry with backoff |
| 6 | Resource error state | Read the returned plan/run error |

`data: []`, `data: null`, a null relationship, “not found,” and “no current
run” are valid final answers. Do not change flags, endpoints, organizations,
workspaces, or IDs to manufacture a non-empty result.
