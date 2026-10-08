import { atom, read, update } from 'claude-code'
import type { EngineInterface, ModelUsage, Register } from 'claude-code'

import type { Panel, Run, RunStatus } from '../types'
import {
  BAND_H,
  COMPACT_H,
  CRAB_IMAGE,
  ROW_H,
  bandSvg,
  compactSvg,
  costumeOf,
  crabRgba,
  headerHeight,
  headerSvg,
  runSvg,
} from './art'
import { costOf, fmtCost, windowOf } from './models'

// A rewrite of johnnyvizz/claude-kit's savvy-progress with a smaller footprint:
// no model-facing tools, no settings or env reads. It only observes subagent
// events and draws what it saw (crabs on desktop, text elsewhere); cost is an
// estimate from a dated price table in models.ts.

const runs = atom({ plugin: 'agent-board', key: 'runs' } as const, [])
const now = atom({ plugin: 'agent-board', key: 'now' } as const, 0)
const panel = atom({ plugin: 'agent-board', key: 'panel' } as const, {
  isCompact: false,
  isDoneCollapsed: false,
  hasAutoOpened: false,
} satisfies Panel)

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

// Descriptions compared loosely: case, spacing and punctuation do not make a new task.
const sameTask = (a: string, b: string): boolean => {
  const norm = (s: string) => s.toLowerCase().replace(/[^\p{L}\p{N}]+/gu, ' ').trim()
  return norm(a) !== '' && norm(a) === norm(b)
}

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

// What one request adds to a run's running total: the tokens it newly processed.
// Cache reads are left out: every step re-reads the cached prefix, so counting
// them makes a long run's total mostly the same conversation read again.
const tokensOf = (u: ModelUsage): number =>
  (u.input_tokens || 0) + (u.output_tokens || 0) + (u.cache_creation_input_tokens || 0)

// A run's cost as shown: `≈` always, `+` when some request's model had no price.
const showCost = (usd: number, isPartial?: boolean): string => `≈${fmtCost(usd)}${isPartial ? '+' : ''}`

const fmtContext = (r: Run): string => {
  const window = windowOf(r.model)
  const pct = window ? ` (${Math.min(100, Math.round((r.contextTokens / window) * 100))}%)` : ''
  return `ctx ${fmtTokens(r.contextTokens)}${pct}`
}

const fmtTokens = (n: number): string =>
  n >= 1e6 ? `${(n / 1e6).toFixed(1)}M` : n >= 1e3 ? `${Math.round(n / 1e3)}k` : `${Math.round(n)}`

const fmtTime = (ms: number): string => {
  const s = Math.max(0, Math.round(ms / 1000))
  const m = Math.floor(s / 60)
  return `${m}:${String(s % 60).padStart(2, '0')}`
}

// Which leg pair a running crab lifts this second; the once-a-second redraw walks it.
const legsAt = (r: Run, at: number): 'la' | 'lb' | null =>
  r.status !== 'running' ? null : Math.floor(at / 1000) % 2 ? 'la' : 'lb'

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
    const description = clean(e.description)
    const earlier = (await read($, runs)).filter(r => r.id !== started.agentId && sameTask(r.description, description))
    const run: Run = {
      id: started.agentId,
      type: clean(e.subagentType),
      description,
      model: clean(started.model),
      status: 'running',
      startedAt: at,
      contextTokens: 0,
      tokens: 0,
      cacheReadTokens: 0,
      costUsd: 0,
      steps: 0,
      round: earlier.length + 1,
    }
    await update($, runs, list => [...list.filter(r => r.id !== run.id), run].slice(-MAX_RUNS))
    await update($, now, () => at)
    // Once per session; a pane the person closed stays closed after that.
    if (!(await read($, panel)).hasAutoOpened) {
      await update($, panel, p => ({ ...p, hasAutoOpened: true }))
      // Best effort: a surface that cannot seat the pane now must not fail the spawn.
      $.ui.open({ id: PANE, title: 'Agents' }).catch(() => {})
    }
    return started
  })

  // Each model request inside a subagent: live context and totals.
  on('turn.step', async function* ($, e, next) {
    const result = yield* next(e)
    const usage = result.usage
    const id = e.agentId
    if (!id || !usage) return result

    const model = clean(usage.model || e.model)
    const cost = costOf(model, usage)
    await update($, runs, list =>
      list.map(r =>
        r.id !== id
          ? r
          : {
              ...r,
              model,
              effort: typeof e.effort === 'string' ? e.effort : r.effort,
              // A resumed subagent runs again.
              status: 'running',
              endedAt: undefined,
              contextTokens: contextOf(usage),
              tokens: r.tokens + tokensOf(usage),
              cacheReadTokens: (r.cacheReadTokens ?? 0) + (usage.cache_read_input_tokens || 0),
              costUsd: (r.costUsd ?? 0) + (cost ?? 0),
              isCostPartial: r.isCostPartial || cost === null,
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
          const model = usage ? clean(usage.model || r.model) : r.model
          const cost = usage ? costOf(model, usage) : null
          const fallback =
            r.steps === 0 && usage
              ? {
                  model,
                  contextTokens: contextOf(usage),
                  tokens: tokensOf(usage),
                  cacheReadTokens: usage.cache_read_input_tokens || 0,
                  costUsd: cost ?? 0,
                  isCostPartial: cost === null,
                }
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
    const cached = list.reduce((s, r) => s + (r.cacheReadTokens ?? 0), 0)
    const cost = list.reduce((s, r) => s + (r.costUsd ?? 0), 0)
    const isCostPartial = list.some(r => r.isCostPartial)
    const p = await read($, panel)
    // The engine's priced total for the whole session (main loop included), not an estimate.
    const sessionUsd = await $.session
      .usage()
      .then(u => u.cost?.usd)
      .catch(() => undefined)
    const sessionCost = sessionUsd === undefined ? '—' : fmtCost(sessionUsd)
    const compactButton = (
      <Button
        key="compact"
        label={p.isCompact ? 'Expand' : 'Collapse'}
        plain
        onPress={() => update($, panel, prev => ({ ...prev, isCompact: !prev.isCompact }))}
      />
    )
    const doneButton = (
      <Button
        key="done"
        label={`${p.isDoneCollapsed ? '▸' : '▾'} Finished · ${finished.length}`}
        plain
        onPress={() => update($, panel, prev => ({ ...prev, isDoneCollapsed: !prev.isDoneCollapsed }))}
      />
    )
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
        const who = `${role} · ${modelName(r.model)}${r.effort ? ` · ${r.effort}` : ''}${(r.round ?? 1) > 1 ? ` · round ${r.round}` : ''}`
        const stats = `${fmtContext(r)} · ${fmtTokens(r.tokens)} tok · ${fmtTokens(r.cacheReadTokens ?? 0)} cached · ${r.steps} steps · ${fmtTime(elapsed(r, at))}`
        const runCost = showCost(r.costUsd ?? 0, r.isCostPartial)
        return (
          <Svg
            key={r.id}
            source={runSvg(W, r, role, who, runCost, stats, total ? r.tokens / total : 0)}
            alt={`${r.description || r.type}: ${who}, ${r.status}`}
            width={W}
            height={ROW_H}
          />
        )
      }
      const time = fmtTime(Math.max(0, at - since))
      const summary = `${showCost(cost, isCostPartial)} · ${fmtTokens(total)} tok · ${fmtTokens(cached)} cached · ${time}`

      if (p.isCompact) {
        const roles = [...running, ...finished].map(r => ({ role: roleOf(r.type), isRunning: r.status === 'running' }))
        return (
          <Box flexDirection="column" gap={1}>
            <Svg source={compactSvg(W, roles, summary)} alt={`${list.length} agents, ${summary}`} width={W} height={COMPACT_H} />
            {compactButton}
          </Box>
        )
      }

      const tiles: [string, string][] = [
        ['Agents', `${list.length}`],
        ['Running', `${running.length}`],
        ['Time', time],
        ['Tokens', fmtTokens(total)],
        ['Cache reads', fmtTokens(cached)],
        ['Agents cost', showCost(cost, isCostPartial)],
        ['Session cost', sessionCost],
      ]
      return (
        <Box flexDirection="column">
          <Svg
            source={headerSvg(W, tiles)}
            alt={`${list.length} agents, ${summary}`}
            width={W}
            height={headerHeight(W, tiles.length)}
          />
          {compactButton}
          {list.length === 0 && <Text dimColor>No subagents yet.</Text>}
          {running.length > 0 && <Text dimColor>Running · {running.length}</Text>}
          {running.map(card)}
          {finished.length > 0 && (
            <Box flexDirection="row" gap={1}>
              {doneButton}
              {clearButton}
            </Box>
          )}
          {!p.isDoneCollapsed && finished.map(card)}
        </Box>
      )
    }

    const lines = (r: Run) => (
      <Box flexDirection="column" flexShrink={1}>
        <Text bold wrap="truncate-end">
          <Text color={COLOR[r.status]}>{GLYPH[r.status]}</Text> {r.description || r.type}
        </Text>
        <Text dimColor wrap="truncate-end">
          {'  '}
          {roleOf(r.type)} · {modelName(r.model)}
          {r.effort ? ` · ${r.effort}` : ''}
          {(r.round ?? 1) > 1 ? ` · round ${r.round}` : ''}
        </Text>
        <Text dimColor wrap="truncate-end">
          {'  '}{fmtContext(r)} · {fmtTokens(r.tokens)} tokens · {fmtTokens(r.cacheReadTokens ?? 0)} cached · {r.steps} steps · {fmtTime(elapsed(r, at))}
        </Text>
      </Box>
    )
    // The crab as pixels where the terminal draws them (Ghostty, kitty); the
    // `alt` stands in elsewhere, so a terminal without graphics loses nothing.
    const row = (r: Run) => {
      if (!('Image' in ui)) {
        return (
          <Box key={r.id} marginBottom={1}>
            {lines(r)}
          </Box>
        )
      }
      const { Image } = ui
      const role = roleOf(r.type)
      return (
        <Box key={r.id} flexDirection="row" gap={1} marginBottom={1}>
          <Image
            source={{ rgba: crabRgba(costumeOf(role), legsAt(r, at)), ...CRAB_IMAGE }}
            columns={6}
            rows={3}
            alt={GLYPH[r.status]}
          />
          {lines(r)}
        </Box>
      )
    }

    return (
      <Box flexDirection="column">
        <Text dimColor>
          {list.length} agents · {showCost(cost, isCostPartial)} · {fmtTokens(total)} tokens · {fmtTokens(cached)} cached · session {sessionCost}
        </Text>
        {list.length === 0 && <Text dimColor>No subagents yet.</Text>}
        {running.length > 0 && <Text bold>Running · {running.length}</Text>}
        {running.map(row)}
        {finished.length > 0 && (
          <Box flexDirection="row" gap={1}>
            {doneButton}
            {clearButton}
          </Box>
        )}
        {!p.isDoneCollapsed && finished.map(row)}
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

    // The terminal's table has an Svg too, which draws nothing there: ask for the surface.
    if (e.surface !== 'terminal' && 'Svg' in ui) {
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

    // Two rows of room: a walking crab per running agent (up to four), as pixels.
    if ('Image' in ui && e.props.maxRows >= 2) {
      const { Image } = ui
      return (
        <Box flexDirection="row" alignItems="center" gap={1}>
          {running.slice(0, 4).map(r => (
            <Image
              key={`crab-${r.id}`}
              source={{ rgba: crabRgba(costumeOf(roleOf(r.type)), legsAt(r, at)), ...CRAB_IMAGE }}
              columns={4}
              rows={2}
              alt="●"
            />
          ))}
          <Text wrap="truncate-end">
            {running.length > 4 ? `+${running.length - 4}  ` : ''}
            {summary}
          </Text>
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
