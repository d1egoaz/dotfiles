import { expect, mock, test } from 'claude-code/testing'

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
