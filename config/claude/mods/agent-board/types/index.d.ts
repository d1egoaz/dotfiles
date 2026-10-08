export type RunStatus = 'running' | 'done' | 'failed'

export type Run = {
  /** The subagent's id, as its loop's events carry it in `agentId`. */
  id: string
  type: string
  description: string
  model: string
  effort?: string
  status: RunStatus
  startedAt: number
  endedAt?: number
  /** Size of the latest request: what the subagent's context holds now. */
  contextTokens: number
  /** Running total across the subagent's requests, as `tokensOf` counts them. */
  tokens: number
  /** Cache reads across the subagent's requests: the cached prefix each request re-read. */
  cacheReadTokens: number
  /** Estimated USD across the subagent's requests (see hooks/models.ts). */
  costUsd: number
  /** Some request ran on a model the price table does not know: the cost is a floor. */
  isCostPartial?: boolean
  steps: number
  /** 2 and up when the same task was delegated again (a retry after review). */
  round?: number
}

export type Panel = {
  isCompact: boolean
  isDoneCollapsed: boolean
  /** The pane opens by itself once per session, on the first subagent. */
  hasAutoOpened: boolean
}

declare module 'claude-code' {
  interface PluginState {
    'agent-board': {
      runs: Run[]
      now: number
      panel: Panel
    }
  }
}
