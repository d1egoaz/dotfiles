---
name: multi-agent-routing
description: Choose outcome-aware Codex models and route subagents for single-agent or multi-agent work. Use when the user asks which model to use, right-size model usage, reduce model cost, choose GPT-5.6 Sol/Terra/Luna, set reasoning effort, prioritize quality or latency, route a lead or subagent, delegate, run parallel agents, or invokes `$multi-agent-routing`. Covers model escalation, de-escalation, task decomposition, API or fast-mode boundaries, and safe integration across coding, review, incidents, and Terraform/IAM work.
---

# Multi-Agent Routing

## Objective

Optimize expected outcome value, not raw token price:

1. Meet the user's quality and time target.
2. Minimize total task cost among routes likely to meet that target.
3. Include retries, tool loops, reasoning tokens, latency, and cost of a wrong result.

The cheapest failed attempt is not cheap. The most expensive model is not automatically the safest.

For multi-agent work, the lead owns scope, decomposition, risky decisions, integration, and the final answer. Use subagents only when the user explicitly asks for subagents, delegation, or parallel work, or when active runtime instructions otherwise authorize it.

## Runtime Facts

- `AGENTS.md` and skills guide routing but cannot replace the active task's model.
- A new task inherits `config/codex/config.toml` unless the user selects an override.
- An existing task keeps its active model until the user changes it or a supported runtime control changes a later turn.
- Reading a file, writing a file, running a command, or loading a skill stays on the active model. It does not become a Luna operation.
- A differently sized subagent is a separate task with separate context and usage. Spawn one only for a meaningful independent unit, never for one tool call.

## Current Calibration

OpenAI API standard prices as of 2026-07-30:

| Model | Input per 1M | Output per 1M | Relative token price |
|------|--------------:|---------------:|----------------------|
| `gpt-5.6-luna` | $0.20 | $1.20 | 0.04x Sol, 0.1x Terra |
| `gpt-5.6-terra` | $2.00 | $12.00 | 0.4x Sol |
| `gpt-5.6-sol` | $5.00 | $30.00 | 1x |

These API prices are a routing signal, not a Codex subscription bill. OpenAI says the Luna and Terra reductions also lower Codex and ChatGPT Work credit usage. Re-check current official pricing before making future dollar claims.

The supplied Artificial Analysis screenshots appear to use the previous Luna and Terra prices. A simple price-only rescale gives this approximate aggregate comparison:

| Route | Intelligence index | Approximate cost per benchmark task |
|------|-------------------:|------------------------------------:|
| Luna high | 46 | $0.026 |
| Luna xhigh | 49 | $0.038 |
| Luna max | 51 | $0.058 |
| Terra medium | 46 | $0.136 |
| Terra high | 49 | $0.256 |
| Terra xhigh | 52 | $0.360 |
| Sol high | 56 | $0.620 |
| Sol xhigh | 58 | $0.940 |

Treat those costs as estimates, not live reruns. The benchmark mix includes caching and different token types.

Task-specific results matter:

- Terminal-Bench: Luna xhigh 78, Luna max 81, Terra max 88, Sol high 87, Sol xhigh 90.
- Agentic tool use: Luna max 27, Sol high 31, Sol xhigh 33.
- SciCode: Luna max 53, Sol high 57, Sol xhigh 56.

Luna is the general value winner after the price cut. Sol still earns its premium for quality-first coding and agentic reliability. Terra is squeezed in aggregate value and should be an evidence-backed exception, not the standing default.

Reasoning levels share a model's per-token rate, but they do not have the same per-task cost. In the supplied token chart, Luna xhigh used about 12k output tokens per task while Luna max used about 19k. Max is still inexpensive in absolute terms, but it can consume roughly 1.6x the output tokens and materially more latency.

## Routing Modes

| Mode | Route | Use when |
|------|-------|----------|
| `utility` | Luna low | No-op polling, exact metadata lookup, deterministic formatting, or a tiny reversible check |
| `routine` | Luna medium | Bounded search, extraction, drafting, or mechanical work with strong verification |
| `value` | Luna xhigh | Default new task, normal implementation, investigation, review, and synthesis |
| `value-max` | Luna max | Quality matters, latency is acceptable, and Luna's extra reasoning can plausibly close the gap |
| `quality` | Sol high | Coding quality, agentic reliability, time, or cost of failure matters more than model spend |
| `frontier` | Sol xhigh | Hard architecture, unresolved root cause, subtle security boundaries, or high-consequence ambiguity |
| `hardest` | Sol max | Exceptional quality-first problem with a clear evaluation target after xhigh is plausibly insufficient |

Use Terra only when a measured task or latency constraint places it between Luna and Sol. Do not route to Terra merely because it is named "balanced."

`ultra` is a separate parallel mode, not a normal reasoning step. Use it only for explicit latency-sensitive frontier work where the extra total usage is justified.

## Selection Test

1. Is the work a no-op, exact lookup, or deterministic transform? Use Luna low.
2. Is it bounded and cheaply verified? Use Luna medium.
3. Is this a normal task where quality still matters? Use Luna xhigh.
4. Would more Luna reasoning likely solve it and is latency acceptable? Use Luna max.
5. Do coding, tool-use reliability, time, or cost of error justify the premium? Use Sol high.
6. Is there a genuinely unresolved frontier decision? Use Sol xhigh.
7. Use Sol max only after naming the evaluation target and why xhigh may be insufficient.

Risk changes approval and verification rigor. It does not select the model by keyword alone. Production, Terraform, IAM, incidents, security, or multiple repositories do not automatically require Sol.

## Fast And API Routes

- Fast mode is an explicit latency purchase. For a quality-first rush task, use Sol high or xhigh with fast mode when available.
- OpenAI states Sol Fast mode can cost 2x while running up to 2.5x faster.
- A model override does not change authentication, billing, provider, or service tier.
- Codex subscription OAuth and direct OpenAI API-key billing are separate routes.
- Never silently move a subscription-backed task to an API key. Confirm the route is configured and the user wants the paid latency tradeoff.

## Workflow

1. Diagnose the outcome target.
   - State the objective, scope, time sensitivity, and verification contract.
   - Separate task difficulty from cost of error.
   - Identify whether quality, latency, or spend is the dominant constraint.

2. Check the active route.
   - If the active task already matches, continue locally.
   - If the whole task is mismatched, recommend the correct model before expensive work.
   - Do not spawn a child merely to escape the current model.

3. Decide whether to delegate.
   - Delegate only bounded, independent work that materially saves time or improves verification.
   - Keep tightly coupled, tiny, unclear, destructive, or live mutation work local unless the user explicitly authorizes otherwise.
   - Use isolated context by default. Include parent history only when the child genuinely needs it.

4. Route by role.
   - Lead/conductor: Luna xhigh by default; Sol high or xhigh for quality-first or frontier work.
   - Evidence scout: Luna medium; low only for exact deterministic collection.
   - Implementation worker: Luna xhigh normally; Sol high for difficult coding or agentic execution.
   - Verification agent: Luna high or xhigh for behavioral review; medium for exact command capture.
   - Scope auditor: Luna xhigh; Sol high for a disputed high-consequence safety claim.
   - Terraform/IAM auditor: Luna xhigh for known patterns and evidence gathering; Sol high or xhigh for novel semantics, destructive migration design, or unresolved final safety judgment.
   - Scribe: Luna medium from verified facts; high for sensitive wording.

5. Integrate and right-size again.
   - Review delegated output before trusting it.
   - Escalate when observed failures show the route cannot follow the contract, misses material ambiguity, or loops on tools.
   - Pass a concise state summary instead of restarting the full task.
   - Do not keep downstream work on Sol by inertia when a separable Luna task can own it.

## Delegation Prompt Template

```text
Role: <evidence scout | implementation worker | verifier | scope auditor | scribe>
Model: <model> with <reasoning effort>, because <quality, latency, and verification reason>
Task: <bounded objective>
Scope: <read scope or disjoint write set>
Constraints: You are not alone in the codebase. Do not revert edits made by others. Preserve unrelated changes.
Output: <exact evidence, changed files, tests, risks, or draft needed>
```

## Validate The Routing Mix

Run:

```bash
~/dotfiles/bin/files/codex-model-usage --root ~/work --root ~/dotfiles
```

The audit reads `turn_context` metadata only, counts unique turns, excludes `codex-auto-review`, and compares the current 7 days with the previous 7 days. It is a routing proxy, not billable token or dollar usage.

Look for:

- Luna becoming the dominant model for normal work.
- Luna xhigh carrying the value workload.
- Luna low staying concentrated in utility work.
- Sol high or xhigh appearing for explicit quality-first work.
- Terra use having a measured reason.
- Sol low or medium being rare because Luna xhigh usually offers better aggregate value.
- Max and ultra having a stated quality or latency reason.

Do not enforce a universal Sol percentage cap. Inspect task classifications before calling a costlier mix wasteful.

## Guardrails

- A cheaper model is appropriate only when failure is visible or the benchmark supports it.
- A stronger model does not authorize broader scope, destructive actions, production mutation, or weaker verification.
- Bound retries and tool loops with a stopping condition.
- Prefer no delegation over poor delegation.
- Preserve unrelated work and keep final decisions in the lead task.
