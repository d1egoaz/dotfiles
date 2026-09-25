# Runtime controls

Use exposed controls and inspect their schemas. Model and effort must be runtime
fields, not just prompt text. If selection is unsupported, keep work in the lead
or explain the available route.

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
