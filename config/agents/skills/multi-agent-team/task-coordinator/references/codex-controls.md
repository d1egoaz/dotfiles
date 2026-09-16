# Runtime controls

Use only controls exposed by the current host.

- Native: `list_agents`, `send_message`, `wait_agent`, `followup_task`,
  `interrupt_agent`.
- Visible: `wait_threads`, `read_thread`, `send_message_to_thread`.
- App Server: `thread/read`, `turn/steer`, `turn/start`, `turn/interrupt`;
  notifications: `thread/started`, `item/started`, `item/completed`,
  `turn/completed`. These are not universal model tools.

Before interrupting a native agent, request completed work, current action,
blockers, remaining work, and a pause. Interrupt only when continuation is
undesirable; resume idle agents instead of duplicating them.

Visible tasks ask the user directly for approval. The lead waits and never
relays approval or duplicates a task for status/context.
