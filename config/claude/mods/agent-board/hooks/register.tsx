import { atom, read, update } from 'claude-code'
import type { EngineInterface, ModelUsage, Register } from 'claude-code'

import type { Run, RunStatus } from '../types'
import { BAND_H, HEADER_H, ROW_H, bandSvg, headerSvg, runSvg } from './art'

// A rewrite of johnnyvizz/claude-kit's savvy-progress with a smaller footprint:
// no model-facing tools, no settings or env reads, no price table. It only
// observes subagent events and draws what it saw (crabs on desktop, text elsewhere).

const runs = atom({ plugin: 'agent-board', key: 'runs' } as const, [])
const now = atom({ plugin: 'agent-board', key: 'now' } as const, 0)

const PANE = 'agent-board'
const MAX_RUNS = 100
const MAX_TEXT = 80
const ACCENT = '#8f8cf4'

const GLYPH: Record<RunStatus, string> = { running: '●', done: '✓', failed: '✗' }
const COLOR: Record<RunStatus, string> = { running: ACCENT, done: 'green', failed: 'red' }

// Descriptions and types are model-written: cap them and drop control characters
// so a hostile prompt cannot push escape sequences or megabytes into the board.
const clean = (s: unknown): string =>
  String(s ?? '')
    // Whole ANSI CSI/OSC sequences first, so no printable tail like `[31m` is left behind.
    .replace(/\u001b\[[0-?]*[ -/]*[@-~]|\u001b\][^\u0007\u001b]*(?:\u0007|\u001b\\)?/g, '')
    .replace(/[\u0000-\u001f\u007f-\u009f]/g, ' ')
    .trim()
    .slice(0, MAX_TEXT)

// `spawn-subagent-implement` -> `implement`; `plugin:agent` -> `agent`.
const roleOf = (type: string): string => type.replace(/^[^:]*:/, '').replace(/^spawn-subagent-/, '') || type

const modelName = (id: string): string => {
  const m = /(fable|opus|sonnet|haiku)-(\d+)(?:-(\d{1,2})(?!\d))?/i.exec(id)
  if (!m) return id.replace(/^claude-/, '') || '?'
  const [, family = '', major = '', minor] = m
  return `${family.charAt(0).toUpperCase()}${family.slice(1).toLowerCase()} ${major}${minor ? '.' + minor : ''}`
}

// Everything the latest request carried in and produced: the context it now holds.
const contextOf = (u: ModelUsage): number =>
  (u.input_tokens || 0) + (u.cache_read_input_tokens || 0) + (u.cache_creation_input_tokens || 0) + (u.output_tokens || 0)

// What one request adds to a run's running total.
const tokensOf = (u: ModelUsage): number => {
  // TODO(human): decide what "tokens" means on the board, then return it.
  return 0
}

const fmtTokens = (n: number): string =>
  n >= 1e6 ? `${(n / 1e6).toFixed(1)}M` : n >= 1e3 ? `${Math.round(n / 1e3)}k` : `${Math.round(n)}`

const fmtTime = (ms: number): string => {
  const s = Math.max(0, Math.round(ms / 1000))
  const m = Math.floor(s / 60)
  return `${m}:${String(s % 60).padStart(2, '0')}`
}

const elapsed = (r: Run, at: number): number => (r.endedAt ?? Math.max(at, r.startedAt)) - r.startedAt

async function togglePane($: EngineInterface): Promise<boolean> {
  if ((await $.ui.panes()).some(p => p.id === PANE)) {
    await $.ui.close({ id: PANE })
    return false
  }
  const at = await $.clock.now()
  await update($, now, () => at)
  await $.ui.open({ id: PANE, title: 'Agents' })
  return true
}

export const register: Register = on => {
  // A second session.start in the same load (a /clear) must not stack another ticker.
  let ticker: { cancel: () => void } | undefined

  on('session.start', async ($, e, next) => {
    const started = await next(e)
    await $.command.register({
      name: 'agent-board',
      description: 'Show or hide the subagent board: running and finished subagents with model, tokens and time',
    })
    ticker?.cancel()
    // Ticks the running clocks once a second; idle when nothing runs.
    ticker = $.clock.every(1000, () => {
      void (async () => {
        if (!(await read($, runs)).some(r => r.status === 'running')) return
        const at = await $.clock.now()
        await update($, now, () => at)
      })()
    })
    return started
  })

  on('command.run', { command: 'agent-board' }, async $ => ({
    text: (await togglePane($)) ? 'Agent board opened.' : 'Agent board closed.',
  }))

  on('agent.spawn', async ($, e, next) => {
    const started = await next(e)
    if (started.deny !== undefined || !started.agentId) return started

    const at = await $.clock.now()
    const run: Run = {
      id: started.agentId,
      type: clean(e.subagentType),
      description: clean(e.description),
      model: clean(started.model),
      status: 'running',
      startedAt: at,
      contextTokens: 0,
      tokens: 0,
      steps: 0,
    }
    await update($, runs, list => [...list.filter(r => r.id !== run.id), run].slice(-MAX_RUNS))
    await update($, now, () => at)
    return started
  })

  // Each model request inside a subagent: live context and totals.
  on('turn.step', async function* ($, e, next) {
    const result = yield* next(e)
    const usage = result.usage
    const id = e.agentId
    if (!id || !usage) return result

    await update($, runs, list =>
      list.map(r =>
        r.id !== id
          ? r
          : {
              ...r,
              model: clean(usage.model || r.model),
              effort: typeof e.effort === 'string' ? e.effort : r.effort,
              // A resumed subagent runs again.
              status: 'running',
              endedAt: undefined,
              contextTokens: contextOf(usage),
              tokens: r.tokens + tokensOf(usage),
              steps: r.steps + 1,
            },
      ),
    )
    return result
  })

  on('turn.complete', async ($, e, next) => {
    const id = e.agentId
    if (id) {
      const at = await $.clock.now()
      const usage = e.usage
      await update($, runs, list =>
        list.map(r => {
          if (r.id !== id) return r
          // No step was seen (it ran before a reload): take the turn's own sum.
          const fallback =
            r.steps === 0 && usage
              ? { model: clean(usage.model || r.model), contextTokens: contextOf(usage), tokens: tokensOf(usage) }
              : {}
          const status: RunStatus = e.reason === 'answer' ? 'done' : 'failed'
          return { ...r, ...fallback, status, endedAt: at }
        }),
      )
      await update($, now, () => at)
    }
    return next(e)
  })

  on('ui.render', { component: 'Pane', requestId: PANE }, async ($, e) => {
    const ui = $.ui.resolve(e)
    const { Box, Text, Button } = ui
    const list = await read($, runs)
    const at = await read($, now)
    const running = list.filter(r => r.status === 'running').reverse()
    const finished = list.filter(r => r.status !== 'running').reverse()
    const total = list.reduce((s, r) => s + r.tokens, 0)
    const clearButton = (
      <Button
        key="clear"
        label="Clear"
        plain
        onPress={() => update($, runs, l => l.filter(r => r.status === 'running'))}
      />
    )

    if (e.surface === 'desktop' && 'Svg' in ui) {
      const { Svg } = ui
      const W = Math.max(240, Math.min(900, (e.props.bodyColumns || 40) * 8 - 8))
      const since = list.length ? Math.min(...list.map(r => r.startedAt)) : at
      const card = (r: Run) => {
        const role = roleOf(r.type)
        const who = `${role} · ${modelName(r.model)}${r.effort ? ` · ${r.effort}` : ''}`
        const stats = `ctx ${fmtTokens(r.contextTokens)} · ${fmtTokens(r.tokens)} tok · ${r.steps} steps · ${fmtTime(elapsed(r, at))}`
        return (
          <Svg
            key={r.id}
            source={runSvg(W, r, role, who, stats, total ? r.tokens / total : 0)}
            alt={`${r.description || r.type}: ${who}, ${r.status}`}
            width={W}
            height={ROW_H}
          />
        )
      }
      const tiles: [string, string][] = [
        ['Agents', `${running.length} / ${list.length}`],
        ['Tokens', fmtTokens(total)],
        ['Time', fmtTime(Math.max(0, at - since))],
      ]
      return (
        <Box flexDirection="column">
          <Svg
            source={headerSvg(W, tiles)}
            alt={`${list.length} agents, ${fmtTokens(total)} tokens`}
            width={W}
            height={HEADER_H}
          />
          {list.length === 0 && <Text dimColor>No subagents yet.</Text>}
          {running.length > 0 && <Text dimColor>Running · {running.length}</Text>}
          {running.map(card)}
          {finished.length > 0 && (
            <Box flexDirection="row" gap={1}>
              <Text dimColor>Finished · {finished.length}</Text>
              {clearButton}
            </Box>
          )}
          {finished.map(card)}
        </Box>
      )
    }

    const row = (r: Run) => (
      <Box key={r.id} flexDirection="column" marginBottom={1}>
        <Text bold wrap="truncate-end">
          <Text color={COLOR[r.status]}>{GLYPH[r.status]}</Text> {r.description || r.type}
        </Text>
        <Text dimColor wrap="truncate-end">
          {'  '}
          {roleOf(r.type)} · {modelName(r.model)}
          {r.effort ? ` · ${r.effort}` : ''}
        </Text>
        <Text dimColor wrap="truncate-end">
          {'  '}ctx {fmtTokens(r.contextTokens)} · {fmtTokens(r.tokens)} tokens · {r.steps} steps · {fmtTime(elapsed(r, at))}
        </Text>
      </Box>
    )

    return (
      <Box flexDirection="column">
        <Text dimColor>
          {list.length} agents · {fmtTokens(total)} tokens
        </Text>
        {list.length === 0 && <Text dimColor>No subagents yet.</Text>}
        {running.length > 0 && <Text bold>Running · {running.length}</Text>}
        {running.map(row)}
        {finished.length > 0 && (
          <Box flexDirection="row" gap={1}>
            <Text bold>Finished · {finished.length}</Text>
            {clearButton}
          </Box>
        )}
        {finished.map(row)}
      </Box>
    )
  })

  // A one-line band above the prompt, only while a subagent is running.
  on('ui.render', { component: 'AbovePrompt' }, async ($, e, next) => {
    if (e.props.hasSurvey) return next(e)
    const running = (await read($, runs)).filter(r => r.status === 'running')
    if (running.length === 0) return next(e)

    const ui = $.ui.resolve(e)
    const { Box, Text, Button } = ui
    const at = await read($, now)
    const since = Math.min(...running.map(r => r.startedAt))
    const roles = running.map(r => roleOf(r.type))
    const summary = `${running.length} running · ${[...new Set(roles)].join(', ')} · ${fmtTime(Math.max(0, at - since))}`
    const boardButton = <Button key="agent-board-open" label="Board" plain onPress={() => void togglePane($)} />

    if ('Svg' in ui) {
      const { Svg } = ui
      // About 8 CSS px per column, less room for the button.
      const width = Math.max(180, Math.min(1600, (e.props.bodyColumns || 100) * 8 - 72))
      return (
        <Box flexDirection="row" alignItems="center" gap={1}>
          <Svg source={bandSvg(width, roles, summary)} alt={summary} width={width} height={BAND_H} />
          {boardButton}
        </Box>
      )
    }

    return (
      <Box flexDirection="row" gap={1}>
        <Text color={ACCENT}>●</Text>
        <Text wrap="truncate-end">{summary}</Text>
        {boardButton}
      </Box>
    )
  })
}
