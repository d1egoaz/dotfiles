## Codex runtime controls

Use these names only when the current host exposes them. They are model-visible
runtime tools, not lifecycle events or universal public API methods.

### Native subagents

- `list_agents`: inspect current state.
- `send_message`: query or redirect a running agent.
- `wait_agent`: wait for a mailbox update.
- `followup_task`: start more work when an agent is idle.
- `interrupt_agent`: interrupt only for unsafe work, invalidated scope,
  conflicting writers, repeated failure without progress, explicit cancellation,
  or another concrete reason continued execution is undesirable.

Before interrupting a running subagent, inspect its status and send a focused
status request or redirect. Ask for completed work, current action, blockers,
remaining work, and a pause before additional writes. A long-running tool call
may deliver that message only at its next safe boundary. If an agent is idle
and more work is needed, send a follow-up or resume it instead of spawning a
duplicate.

### Visible tasks

- `wait_threads`: wait for the first of up to eight tasks to complete or need
  attention; use bounded waits and cursors.
- `read_thread`: inspect detailed state and results.
- `send_message_to_thread`: deliver a visible follow-up.

Do not open a duplicate task merely to obtain status or deliver new context.

### Direct App Server integration

Approximate public mappings are `thread/read`, `turn/steer`, `turn/start`, and
`turn/interrupt`. Lifecycle notifications include `thread/started`,
`item/started`, `item/completed`, and `turn/completed`. Do not call these names
as model tools unless the runtime actually exposes that API.
