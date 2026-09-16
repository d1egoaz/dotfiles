# Capability routing

Use the shared AI instructions and agent TOML for configured routes. Assess
scope, ambiguity, judgment, verification quality, and consequence of failure.
Choose the cheapest capable model and effort, counting likely retries and review.
These are capability gates, not an additive score or model quota.

Start with Luna for bounded, objectively checkable work; Terra for exploration,
synthesis, or correctness review; Sol for deep ambiguity remaining after
splitting the work. Lower effort when reliable; raise it for a specific gap.
Use a named role only when its model, effort, and permissions fit. Otherwise
select an explicit route and include the role's boundaries in the prompt.

Explain each delegation in one line: outcome, model/effort, and why it fits.
Example: "Verify rollout evidence: Terra/xhigh for conflicting lifecycle states."

Reassess material follow-ups. Escalate for demonstrated capability gaps, not a
fixed retry count. Missing access, approval, data, or tools is a blocker, not a
reason for a stronger model. Read current state before retrying an uncertain
side effect. Reuse agents while their route fits; stop an old writer before
replacing it. Configuration edits do not change an active agent's model.
