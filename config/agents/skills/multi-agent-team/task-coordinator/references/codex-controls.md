# Runtime controls

Use exposed controls and inspect their schemas. Model and effort must be runtime
fields, not just prompt text. If selection is unsupported, keep work in the lead
or explain the available route.

Spawn native subagents with `fork_turns: "none"` and put the whole handoff,
including what the lead already knows, in the message. `all` copies the parent
history into every child (15-30k extra tokens each), and forked children
inherit the parent turn context, which hides their real route in audits. A
turn count copies that many recent turns; use it only when the child must see
them verbatim.

Before finishing, wait for every writer subagent to report; never end the lead
while a writer may still be editing. A child that ends without a result (a
pending tool call, an aborted turn, or a timed-out wait) is not done: list it
under **Found** with its last known state, and reconstruct state before any
retry of side-effectful work.

Wait for events with bounded waits; inspect changed results or blockers. Reuse
idle agents with the host's follow-up/resume control, not a status message.
Request status or redirect before interrupting, allowing a bounded response;
interrupt immediately for unsafe work, conflicting writers, or cancellation.
Do not duplicate tasks to obtain status or deliver context.

Title changes never replace a direct user question, an approval request, or a
recorded error. Native subagent names are immutable routing identifiers, not
status indicators.

Native subagents report approval blockers to their parent, which uses the host's
approval flow. Visible tasks ask the user directly. Continue independent
authorized work while approval is pending.
