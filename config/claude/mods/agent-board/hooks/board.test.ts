import { expect, mock, test } from 'claude-code/testing'

import { CRAB_IMAGE, crabRgba } from './art'
import { costOf, windowOf } from './models'

const SPAWN = {
  tool_use_id: 'tu1',
  prompt: 'Map the auth flow',
  description: 'Map auth flow\u001b[31m',
  subagentType: 'spawn-subagent-explore',
  provider: { plugin: 'engine', tier: 'core' },
  parentModel: 'claude-opus-5-5',
  background: false,
  fork: false,
} as const

const PANE_PROPS = {
  title: 'Agents',
  isFocused: true,
  bodyColumns: 60,
  placement: 'dock',
  scroll: { offset: 0, bodyRows: 40 },
  view: {},
} as const

test('a finished subagent shows on the board, control characters stripped', async ($, on) => {
  mock.clock(on)
  on('agent.spawn', async () => ({ model: 'claude-opus-5-5', agentId: 'a1' }))
  on('turn.complete', async ($, e) => ({ text: e.answer }))

  await $.agent.spawn(SPAWN)
  await $.turn.complete({
    turnId: 't1',
    agentId: 'a1',
    answer: 'done',
    durationMs: 5,
    isAborted: false,
    reason: 'answer',
  })

  const term = await $.ui.mount({ plugin: 'agent-board', surface: 'terminal', component: 'Pane', requestId: 'agent-board', props: PANE_PROPS })
  expect(await term.find({ text: /Finished · 1/ })).toBeDefined()
  expect(await term.find({ text: /Map auth flow/ })).toBeDefined()
  expect(await term.find({ text: /\u001b/ })).toBeUndefined()
  expect(await term.find({ text: /explore · Opus 5\.5/ })).toBeDefined()

  const desk = await $.ui.mount({ plugin: 'agent-board', surface: 'desktop', component: 'Pane', requestId: 'agent-board', props: PANE_PROPS })
  expect(await desk.find({ text: /Finished · 1/ })).toBeDefined()
  const svgs = await desk.findAll({ type: 'Svg' })
  const alts = svgs.map(s => String(s.props.alt))
  expect(alts).toContain('Map auth flow: explore · Opus 5.5, done')
  const header = String(svgs[0]?.props.source)
  expect(header).toMatch(/>Agents<.*>1<.*>Running<.*>0</s)
  expect(svgs.every(s => !String(s.props.source).includes('\u001b'))).toBe(true)
})

test('the band shows walking crabs only while a subagent runs', async ($, on) => {
  mock.clock(on)
  on('agent.spawn', async () => ({ model: 'claude-sonnet-5-5', agentId: 'a2' }))
  on('ui.render', async ($, e) => h($.ui.resolve(e).Box, {}))

  const BAND = { hasSurvey: false, isWorking: true, maxRows: 3, bodyColumns: 100, scroll: { offset: 0, bodyRows: 3 }, view: {} } as const
  const before = await $.ui.mount({ plugin: 'agent-board', surface: 'desktop', component: 'AbovePrompt', props: BAND })
  expect(await before.find({ type: 'Svg' })).toBeUndefined()

  await $.agent.spawn({ ...SPAWN, subagentType: 'spawn-subagent-review', description: '<script>x</script>' })
  const band = await $.ui.mount({ plugin: 'agent-board', surface: 'desktop', component: 'AbovePrompt', props: BAND })
  const svg = await band.find({ type: 'Svg' })
  expect(String(svg?.props.alt)).toMatch(/^1 running · review · 0:00$/)
  expect(String(svg?.props.source)).toContain('c-heavy run')
})

test('a refused spawn adds nothing', async ($, on) => {
  mock.clock(on)
  on('agent.spawn', async () => ({ deny: 'no' }))

  await $.agent.spawn(SPAWN)

  const ui = await $.ui.mount({ plugin: 'agent-board', surface: 'terminal', component: 'Pane', requestId: 'agent-board', props: PANE_PROPS })
  expect(await ui.find({ text: /No subagents yet/ })).toBeDefined()
})

test('the terminal draws crabs as pixels in the pane and the band', async ($, on) => {
  mock.clock(on)
  on('agent.spawn', async () => ({ model: 'claude-sonnet-5-5', agentId: 'a3' }))
  on('ui.render', async ($, e) => h($.ui.resolve(e).Box, {}))

  await $.agent.spawn({ ...SPAWN, subagentType: 'spawn-subagent-gather' })

  const pane = await $.ui.mount({ plugin: 'agent-board', surface: 'terminal', component: 'Pane', requestId: 'agent-board', props: PANE_PROPS })
  const image = await pane.find({ type: 'Image' })
  expect(image?.props).toMatchObject({ columns: 6, rows: 3 })
  const source = image?.props.source as { rgba: string; width: number; height: number }
  expect(source).toMatchObject({ width: 120, height: 112 })
  const px = Uint8Array.fromBase64(source.rgba)
  expect(px.length).toBe(120 * 112 * 4)
  // The body's clay at art (15, 15), scaled by 4; the corner stays transparent.
  const at = (x: number, y: number) => [...px.slice((y * 120 + x) * 4, (y * 120 + x) * 4 + 4)]
  expect(at(60, 60)).toEqual([0xd9, 0x77, 0x57, 255])
  expect(at(0, 111)[3]).toBe(0)

  const BAND = { hasSurvey: false, isWorking: true, maxRows: 3, bodyColumns: 100, scroll: { offset: 0, bodyRows: 3 }, view: {} } as const
  const band = await $.ui.mount({ plugin: 'agent-board', surface: 'terminal', component: 'AbovePrompt', props: BAND })
  expect((await band.find({ type: 'Image' }))?.props).toMatchObject({ columns: 4, rows: 2 })
  expect(await band.find({ text: /1 running · gather/ })).toBeDefined()
})

test('a running crab alternates its legs; a finished one stands', () => {
  expect(crabRgba('careful', 'la')).not.toBe(crabRgba('careful', 'lb'))
  expect(crabRgba('careful', null)).not.toBe(crabRgba('careful', 'la'))
  expect(CRAB_IMAGE).toEqual({ width: 120, height: 112 })
})

test('tokens count new work, not cache reads; context counts everything', async ($, on) => {
  mock.clock(on)
  on('agent.spawn', async () => ({ model: 'claude-sonnet-5-5', agentId: 'a4' }))
  on('turn.complete', async ($, e) => ({ text: e.answer }))

  await $.agent.spawn(SPAWN)
  await $.turn.complete({
    turnId: 't4',
    agentId: 'a4',
    answer: 'done',
    durationMs: 5,
    isAborted: false,
    reason: 'answer',
    usage: {
      model: 'claude-sonnet-5-5',
      input_tokens: 1000,
      output_tokens: 500,
      cache_creation_input_tokens: 2000,
      cache_read_input_tokens: 40000,
    },
  })

  const ui = await $.ui.mount({ plugin: 'agent-board', surface: 'terminal', component: 'Pane', requestId: 'agent-board', props: PANE_PROPS })
  expect(await ui.find({ text: /ctx 44k \(4%\) · 4k tokens · 40k cached/ })).toBeDefined()
  expect(await ui.find({ text: /1 agents · ≈\$0\.02 · 4k tokens · 40k cached/ })).toBeDefined()

  const desk = await $.ui.mount({ plugin: 'agent-board', surface: 'desktop', component: 'Pane', requestId: 'agent-board', props: PANE_PROPS })
  const [header, row] = (await desk.findAll({ type: 'Svg' })).map(s => String(s.props.source))
  expect(header).toMatch(/>Cache reads<.*>40k</s)
  expect(row).toContain('4k tok · 40k cached')
  expect(row).toContain('≈$0.02')
  expect(header).toMatch(/>Agents cost<.*>≈\$0\.02</s)
  expect(header).toMatch(/>Session cost<.*>—</s)
})

const USAGE = { input_tokens: 1_000_000, output_tokens: 0, cache_creation_input_tokens: 0, cache_read_input_tokens: 0 }

test('cost uses each model documented rate and knows when it does not know', () => {
  expect(costOf('claude-opus-5-5', USAGE)).toBe(4)
  expect(costOf('claude-opus-5', USAGE)).toBe(5)
  expect(costOf('claude-sonnet-5-5', USAGE)).toBe(2)
  expect(costOf('claude-sonnet-5-20260101', USAGE)).toBe(2)
  expect(costOf('claude-sonnet-4-6', USAGE)).toBe(3)
  expect(costOf('claude-fable-5-1', { ...USAGE, input_tokens: 0, cache_read_input_tokens: 1_000_000 })).toBe(0.25)
  expect(costOf('claude-fable-5', { ...USAGE, input_tokens: 0, cache_read_input_tokens: 1_000_000 })).toBe(1)
  expect(costOf('claude-opus-5-5', { ...USAGE, input_tokens: 0, cache_creation_input_tokens: 1_000_000 })).toBe(8)
  // Haiku 5.5's two rate cards: short prompts $0.10, past 100k $0.50.
  const cents = (n: number | null) => Math.round((n ?? -1) * 1e4) / 1e4
  expect(cents(costOf('claude-haiku-5-5', { ...USAGE, input_tokens: 50_000 }))).toBe(0.005)
  expect(cents(costOf('claude-haiku-5-5', { ...USAGE, input_tokens: 200_000 }))).toBe(0.1)
  expect(costOf('some-other-model', USAGE)).toBeNull()
})

test('Collapse shows the compact strip; the Finished toggle folds finished runs', async ($, on) => {
  mock.clock(on)
  on('agent.spawn', async () => ({ model: 'claude-opus-5-5', agentId: 'a5' }))
  on('turn.complete', async ($, e) => ({ text: e.answer }))
  await $.agent.spawn(SPAWN)
  await $.turn.complete({ turnId: 't5', agentId: 'a5', answer: 'done', durationMs: 5, isAborted: false, reason: 'answer' })

  const ui = await $.ui.mount({ plugin: 'agent-board', surface: 'desktop', component: 'Pane', requestId: 'agent-board', props: PANE_PROPS })
  expect(await ui.findAll({ type: 'Svg' })).toHaveLength(2)
  await ui.press({ key: 'done' })
  expect(await ui.findAll({ type: 'Svg' })).toHaveLength(1)
  expect((await ui.find({ key: 'done' }))?.props.label).toBe('▸ Finished · 1')

  await ui.press({ key: 'compact' })
  const strip = await ui.findAll({ type: 'Svg' })
  expect(strip).toHaveLength(1)
  expect(String(strip[0]?.props.source)).toContain('c-explore')
  expect((await ui.find({ key: 'compact' }))?.props.label).toBe('Expand')
})

test('the first subagent of a session opens the pane, once', async ($, on) => {
  mock.clock(on)
  const opened: string[] = []
  on('ui.open', async ($, e) => {
    opened.push(e.id)
    return { value: { isPlaced: true as const } }
  })
  on('agent.spawn', async ($, e) => ({ model: 'claude-opus-5-5', agentId: e.tool_use_id }))

  await $.agent.spawn({ ...SPAWN, tool_use_id: 'x1' })
  await $.agent.spawn({ ...SPAWN, tool_use_id: 'x2' })
  expect(opened).toEqual(['agent-board'])
})

test('context windows come from the documented table, none for unknown models', () => {
  expect(windowOf('claude-opus-5-5')).toBe(1_000_000)
  expect(windowOf('claude-sonnet-5')).toBe(1_000_000)
  expect(windowOf('claude-haiku-4-5-20251001')).toBe(200_000)
  expect(windowOf('claude-haiku-5-5')).toBe(1_000_000)
  expect(windowOf('claude-opus-4-5')).toBeNull()
  expect(windowOf('some-other-model')).toBeNull()
})

test('a task delegated again is round 2; the session total is the engine figure', async ($, on) => {
  mock.clock(on)
  on('agent.spawn', async ($, e) => ({ model: 'claude-opus-5-5', agentId: e.tool_use_id }))
  on('session.usage', async () => ({
    value: { startedAt: 0, context: { window: 1_000_000 }, rateLimits: [], cost: { usd: 1.234 } },
  }))

  await $.agent.spawn({ ...SPAWN, tool_use_id: 'r1', description: 'Review the diff' })
  await $.agent.spawn({ ...SPAWN, tool_use_id: 'r2', description: 'review the diff.' })
  await $.agent.spawn({ ...SPAWN, tool_use_id: 'r3', description: 'Something else' })

  const ui = await $.ui.mount({ plugin: 'agent-board', surface: 'desktop', component: 'Pane', requestId: 'agent-board', props: PANE_PROPS })
  const alts = (await ui.findAll({ type: 'Svg' })).map(s => String(s.props.alt))
  expect(alts.filter(a => a.includes('round 2'))).toHaveLength(1)
  expect(alts.some(a => a.includes('round 3'))).toBe(false)
  const header = String((await ui.findAll({ type: 'Svg' }))[0]?.props.source)
  expect(header).toMatch(/>Session cost<.*>\$1\.23</s)
})
