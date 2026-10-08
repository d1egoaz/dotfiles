import type { ModelUsage } from 'claude-code'

// What the board knows about models: price and context window, from the
// claude-api skill's tables as cached on 2026-10-06.

// Cost is an estimate, never a bill: Anthropic first-party rates (Bedrock and Vertex price
// separately), USD per million tokens, from the claude-api skill's model table and
// migration notes as cached on 2026-10-06. Cache writes are priced at the 1-hour
// TTL (2x input) that Claude Code sessions use; usage does not say which TTL a
// write had, so a 5-minute write reads 60% high.

type Rates = { input: number; output: number; cacheWrite: number; cacheRead: number }

const rates = (input: number, output: number, cacheRead: number): Rates => ({
  input,
  output,
  cacheWrite: input * 2,
  cacheRead,
})

// `family-major` with no single-digit minor after it, so `sonnet-5` is not
// `sonnet-5-5` but still matches a dated id such as `sonnet-5-20260101`.
const exact = (id: string): RegExp => new RegExp(`${id}(?!-\\d(?:\\D|$))`)

// First match wins.
const PRICES: [RegExp, Rates | ((promptTokens: number) => Rates)][] = [
  [exact('(?:fable|mythos)-5-1'), rates(10, 50, 0.25)],
  [exact('(?:fable|mythos)-5'), rates(10, 50, 1)],
  [exact('opus-5-5'), rates(4, 20, 0.2)],
  [exact('opus-5'), rates(5, 25, 0.5)],
  [exact('opus-4-[678]'), rates(5, 25, 0.5)],
  [exact('sonnet-5-5'), rates(2, 10, 0.2)],
  [exact('sonnet-5'), rates(2, 10, 0.2)],
  [exact('sonnet-4-6'), rates(3, 15, 0.3)],
  // Two rate cards, by prompt length.
  [exact('haiku-5-5'), prompt => (prompt > 100_000 ? rates(0.5, 2.5, 0.05) : rates(0.1, 0.5, 0.01))],
  [exact('haiku-4-5'), rates(1, 5, 0.1)],
]

/** The request's estimated cost in USD, or null for a model the table does not know. */
export const costOf = (model: string, u: ModelUsage): number | null => {
  const id = model.toLowerCase()
  const hit = PRICES.find(([re]) => re.test(id))?.[1]
  if (!hit) return null
  const input = u.input_tokens || 0
  const output = u.output_tokens || 0
  const write = u.cache_creation_input_tokens || 0
  const read = u.cache_read_input_tokens || 0
  const r = typeof hit === 'function' ? hit(input + write + read) : hit
  return (input * r.input + output * r.output + write * r.cacheWrite + read * r.cacheRead) / 1e6
}

export const fmtCost = (usd: number): string => `$${usd < 10 ? usd.toFixed(2) : usd.toFixed(1)}`

// The documented context window. Claude Code runs each model at its default
// window when no compaction override is set; unknown models get none, so the
// board shows absolute tokens without a percentage rather than guess.
const WINDOWS: [RegExp, number][] = [
  [exact('haiku-4-5'), 200_000],
  [exact('(?:fable|mythos)-5(?:-1)?'), 1_000_000],
  [exact('opus-5(?:-5)?'), 1_000_000],
  [exact('opus-4-[678]'), 1_000_000],
  [exact('sonnet-5(?:-5)?'), 1_000_000],
  [exact('sonnet-4-6'), 1_000_000],
  [exact('haiku-5-5'), 1_000_000],
]

export const windowOf = (model: string): number | null =>
  WINDOWS.find(([re]) => re.test(model.toLowerCase()))?.[1] ?? null
