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
  steps: number
}

declare module 'claude-code' {
  interface PluginState {
    'agent-board': {
      runs: Run[]
      now: number
    }
  }
}
