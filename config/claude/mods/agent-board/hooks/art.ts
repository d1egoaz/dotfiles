// Desktop drawings for agent-board. The pixel crabs (CLAY through COSTUMES) are
// copied unchanged from johnnyvizz/claude-kit plugins/savvy-progress, commit
// 30084726, under the MIT License:
//
//   Copyright (c) 2026 johnnyvizz
//
//   Permission is hereby granted, free of charge, to any person obtaining a copy
//   of this software and associated documentation files (the "Software"), to deal
//   in the Software without restriction, including without limitation the rights
//   to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
//   copies of the Software, and to permit persons to whom the Software is
//   furnished to do so, subject to the following conditions: The above copyright
//   notice and this permission notice shall be included in all copies or
//   substantial portions of the Software. THE SOFTWARE IS PROVIDED "AS IS",
//   WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED.
//
// Everything else here is agent-board's own. Every string that reaches markup
// goes through `xml`; the surface draws the result as an image either way.

import type { Run, RunStatus } from '../types'

export const FONT = "-apple-system,BlinkMacSystemFont,'SF Pro Text','Segoe UI',sans-serif"

export const xml = (s: string): string =>
  s.replace(/[&<>"']/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' })[c] ?? c)

// Rough advance of system UI text, in em; good enough to fit a line.
const charEm = (ch: string): number => (/[\s.,:;'|!il1()[\]]/.test(ch) ? 0.3 : /[A-Zmw@%]/.test(ch) ? 0.72 : 0.56)

export const textWidth = (s: string, size: number): number => [...s].reduce((w, ch) => w + charEm(ch) * size, 0)

export const fitText = (s: string, size: number, maxW: number): string => {
  if (textWidth(s, size) <= maxW) return s
  let out = ''
  for (const ch of s) {
    if (textWidth(out + ch + '…', size) > maxW) break
    out += ch
  }
  return out + '…'
}

// Pixel Clawd from DockCrab (Clawdy): a 24×18 crab on a 30×28 grid, one costume per tier.
// The body keeps the brand clay; the tier's color lives in the costume's accent.
const CLAY = '#D97757'
const INK = '#1F1E1D'

// `cls` puts a pixel in a named group: `bd` (the default) is the body and its
// costume, `la`/`lb` the leg pairs, anything else a prop with its own motion.
type Fill = (x: number, y: number, w: number, h: number, c: string, cls?: string) => void

const stamp = (f: Fill, x: number, y: number, rows: string[], map: Record<string, string>, cls?: string): void =>
  rows.forEach((row, dy) => [...row].forEach((ch, dx) => map[ch] && f(x + dx, y + dy, 1, 1, map[ch] ?? '', cls)))

// `armCls` lets a raised claw travel with the prop it holds.
const crabBody = (f: Fill, armFront = 0, armCls?: string): void => {
  f(7, 10, 16, 12, CLAY)
  f(3, 14, 4, 4, CLAY)
  f(23, 14 + armFront, 4, 4, CLAY, armCls)
  f(9, 12, 2, 2, INK)
  f(19, 12, 2, 2, INK)
  f(7, 22, 2, 4, CLAY, 'la')
  f(17, 22, 2, 4, CLAY, 'la')
  f(11, 22, 2, 4, CLAY, 'lb')
  f(21, 22, 2, 4, CLAY, 'lb')
}

// Pure CSS, run by the compositor: no redraws. Periods divide one second, so the
// once-a-second redraw of a running row restarts them in phase. Every crab walks;
// each costume adds its prop's own motion on top.
const CRAB_CSS = `<style>
.run .la{animation:st .5s steps(1) infinite}.run .lb{animation:st .5s steps(1) infinite -.25s}
.run .bd{animation:bob .5s steps(1) infinite -.125s}
.run g{transform-box:fill-box}
@keyframes st{50%{transform:translateY(-1px)}}@keyframes bob{50%{transform:translateY(1px)}}
.c-fable.run{animation:float 1s ease-in-out infinite}
.c-fable.run .la,.c-fable.run .lb,.c-fable.run .bd{animation:none}
.c-fable.run .ant{animation:blink 1s steps(1) infinite}
.c-fable.run .star{animation:blink .5s steps(1) infinite -.25s}
@keyframes float{50%{transform:translateY(-2px)}}@keyframes blink{50%{opacity:.15}}
.c-heavy.run .it{animation:scan 1s steps(1) infinite}
.c-heavy.run .gl{animation:blink 1s steps(1) infinite -.5s}
@keyframes scan{25%{transform:translate(-1px,1px)}50%{transform:translate(-2px,2px)}75%{transform:translate(-1px,1px)}}
.c-careful.run .it{transform-origin:100% 100%;animation:twist .5s ease-in-out infinite}
@keyframes twist{50%{transform:rotate(-35deg)}}
.c-medium.run .pan{transform-origin:0 50%;animation:tilt 1s ease-in-out infinite}
.c-medium.run .egg{animation:flip 1s ease-in-out infinite}
@keyframes tilt{20%,40%{transform:rotate(-12deg)}}@keyframes flip{30%{transform:translateY(-5px) scaleY(-1)}60%{transform:translateY(0)}}
.c-light.run .la{animation-duration:.25s}.c-light.run .lb{animation-duration:.25s;animation-delay:-.125s}
.c-light.run .flag{transform-origin:0 50%;animation:wave .25s steps(1) infinite}
@keyframes wave{50%{transform:skewY(-12deg) scaleX(.85)}}
.c-explore.run .it{transform-origin:50% 100%;animation:fence .5s ease-in-out infinite}
@keyframes fence{50%{transform:rotate(25deg)}}
@media (prefers-reduced-motion: reduce){.run,.run g{animation:none!important}}
</style>`

const COSTUMES: Record<string, (f: Fill, t: string) => void> = {
  // Fable: astronaut in a glass dome; floats instead of walking, the antenna and the star blink.
  fable: (f, t) => {
    crabBody(f)
    f(6, 7, 18, 1, '#E6E8EE'); f(5, 8, 1, 14, '#E6E8EE'); f(24, 8, 1, 14, '#E6E8EE'); f(6, 22, 18, 1, '#C9CCD2')
    f(6, 8, 18, 14, 'rgba(169,214,245,.32)'); f(8, 9, 2, 1, '#fff'); f(8, 10, 1, 2, '#fff')
    f(14, 4, 2, 3, '#C9CCD2'); f(14, 2, 2, 2, t, 'ant'); f(13, 18, 4, 2, t)
    f(27, 3, 1, 3, '#F5C542', 'star'); f(26, 4, 3, 1, '#F5C542', 'star')
  },
  // Heavy: detective with a deerstalker; the magnifier sweeps and glints.
  heavy: (f, t) => {
    crabBody(f, -4, 'it')
    stamp(f, 6, 3, ['......bbbbbb......', '....bbcbbcbbbb....', '...bbbbbbbbbbbb...', '..bcbbcbbcbbcbbb..', '.bbbbbbbbbbbbbbbb.', 'dddddddddddddddddd'], { b: '#7A4A26', c: '#A0703F', d: '#5A3519' })
    f(6, 9, 18, 1, t)
    stamp(f, 23, 1, ['.kkk.', 'k...k', 'k...k', 'k...k', '.kkk.'], { k: '#3A3A3C' }, 'it')
    f(24, 2, 3, 3, 'rgba(169,214,245,.7)', 'it'); f(25, 6, 1, 4, '#7A4A26', 'it'); f(24, 2, 1, 1, '#fff', 'gl')
  },
  // Careful: engineer in a hard hat; the wrench turns a bolt.
  careful: (f, t) => {
    crabBody(f)
    stamp(f, 6, 4, ['.....yyyyyyyy.....', '...yyyyyhhyyyyy...', '..yyyyyyhhyyyyyy..', '..yyyyyyhhyyyyyy..', '.yyyyyyyhhyyyyyyy.', 'dddddddddddddddddd'], { y: '#F5C542', h: '#FBE08A', d: '#C99A1E' })
    f(13, 5, 4, 2, t)
    stamp(f, 0, 10, ['.s.s', 'sss.', '.s..', '.s..'], { s: '#8E929A' }, 'it')
  },
  // Medium: chef, the toque traced from DockCrab's Sprites.chefHat; tosses the omelette.
  medium: (f, t) => {
    crabBody(f, -4, 'pan')
    stamp(f, 6, 0, ['........lll.......', '.......lllll......', '.wwwwgwwwwwwgwwwww', 'wwwwwwwwwwwwwwwwww', 'wwwwwwwwwwwwwwwwww', 'wwwwwgwwwwwggwwwww', '.wwwwgwwwwwggwwwww', '.dddbbbbbbbbbbbbb.', '.dddbbbbbbbbbbbbb.', '.dddbbbbbbbbbbbbb.'], { w: '#F4F3EE', l: '#F7F6F2', g: '#D2D1C8', b: t, d: '#B45F43' })
    f(22, 8, 7, 2, '#4A4A48', 'pan'); f(26, 10, 1, 1, '#4A4A48', 'pan'); f(24, 7, 3, 1, '#F5B731', 'egg')
  },
  // Light: racer in a helmet; runs at double pace, the checkered flag flutters.
  light: (f, t) => {
    crabBody(f, -4)
    stamp(f, 6, 5, ['....rrrrrrrrrr....', '..rrrrrrwwrrrrrr..', '.rrrrrrrwwrrrrrrr.', '.rrrrrrrwwrrrrrrr.', '.rrrrrrrwwrrrrrrr.', '.kkkkkkkkkkkkkkkkr'], { r: t, w: '#F8F6F1', k: INK })
    f(25, 1, 1, 9, '#8E929A')
    stamp(f, 26, 1, ['wkwk', 'kwkw', 'wkwk'], { w: '#F8F6F1', k: INK }, 'flag')
  },
  // Explore: pirate scouting the code; the cutlass fences.
  explore: f => {
    crabBody(f)
    stamp(f, 5, 3, ['.kk..............kk.', '.kkk....kkkk....kkk.', '..kkkkkkkwwkkkkkkk..', '..kkkkkkkkkkkkkkkk..', '.gggggggggggggggggg.'], { k: '#55514C', w: '#F8F6F1', g: '#F5C542' })
    f(7, 11, 11, 1, INK); f(18, 11, 4, 3, INK)
    f(27, 6, 1, 9, '#C9CCD2', 'it'); f(26, 15, 3, 1, '#7A4A26', 'it')
  },
  other: f => crabBody(f),
}

// Which costume each of the dotfiles' roles wears; anything unknown is a plain crab.
const COSTUME_OF = new Map<string, string>([
  ['review', 'heavy'],
  ['implement', 'careful'],
  ['explore', 'explore'],
  ['Explore', 'explore'],
  ['gather', 'light'],
  ['Plan', 'medium'],
  ['general-purpose', 'fable'],
])

const COSTUME_COLOR = new Map<string, string>([
  ['heavy', '#D85A30'],
  ['careful', '#BA7517'],
  ['explore', '#378ADD'],
  ['light', '#1D9E75'],
  ['medium', '#7F77DD'],
  ['fable', '#8f8cf4'],
])

export const costumeOf = (role: string): string => COSTUME_OF.get(role) ?? 'other'
export const colorOf = (costume: string): string => COSTUME_COLOR.get(costume) ?? '#888780'

// Body and props nest inside `bd` so a prop rides the bob and adds its own motion;
// legs stay outside it and step on their own.
export const crab = (x: number, y: number, costume: string, isWalking: boolean, scale = 1.1): string => {
  const groups = new Map<string, string[]>([['bd', []]])
  const f: Fill = (cx, cy, w, h, c, cls = 'bd') => {
    if (!groups.has(cls)) groups.set(cls, [])
    groups.get(cls)?.push(`<rect x="${cx}" y="${cy}" width="${w}" height="${h}" fill="${c}"/>`)
  }
  const draw = Object.hasOwn(COSTUMES, costume) ? COSTUMES[costume] : undefined
  if (draw) draw(f, colorOf(costume))
  else crabBody(f)
  const group = (cls: string) => `<g class="${cls}">${(groups.get(cls) ?? []).join('')}</g>`
  const props = [...groups.keys()].filter(k => k !== 'bd' && k !== 'la' && k !== 'lb')
  const body = `<g class="bd">${(groups.get('bd') ?? []).join('')}${props.map(group).join('')}</g>`
  return `<g transform="translate(${x},${y}) scale(${scale})" shape-rendering="crispEdges"><g class="c-${costume}${isWalking ? ' run' : ''}">${body}${group('la')}${group('lb')}</g></g>`
}

// --- terminal crabs: the same costumes as RGBA pixels for the terminal's Image
// (kitty graphics in Ghostty or kitty; its `alt` text where the terminal cannot).

const ART_W = 30
const ART_H = 28
const PX = 4 // nearest-neighbour upscale, so the terminal's own scaling stays crisp

// `#rgb`, `#rrggbb` or `rgba(r,g,b,a)` as [r, g, b, a in 0..1]; null for anything else.
const rgbaOf = (c: string): [number, number, number, number] | null => {
  const hex = /^#([0-9a-f]{3}|[0-9a-f]{6})$/i.exec(c)?.[1]
  if (hex) {
    const n = parseInt(hex.length === 3 ? [...hex].map(d => d + d).join('') : hex, 16)
    return [(n >> 16) & 255, (n >> 8) & 255, n & 255, 1]
  }
  const m = /^rgba\((\d+),(\d+),(\d+),([\d.]+)\)$/.exec(c.replace(/\s/g, ''))
  return m ? [Number(m[1]), Number(m[2]), Number(m[3]), Math.min(1, Number(m[4]))] : null
}

const imageCache = new Map<string, string>()

/** Width and height of `crabRgba`'s picture, in pixels. */
export const CRAB_IMAGE = { width: ART_W * PX, height: ART_H * PX } as const

// `raised` lifts one leg pair 1px; drawing `la` then `lb` on alternate frames walks.
export const crabRgba = (costume: string, raised: 'la' | 'lb' | null): string => {
  const cacheKey = `${costume}:${raised ?? ''}`
  const hit = imageCache.get(cacheKey)
  if (hit) return hit

  // Paint at art scale with "over" compositing, as the SVG paints its rects in order.
  const art = new Float32Array(ART_W * ART_H * 4)
  const f: Fill = (x, y, w, h, c, cls) => {
    const src = rgbaOf(c)
    if (!src) return
    const [r, g, b, a] = src
    const lift = cls !== undefined && cls === raised ? 1 : 0
    for (let dy = 0; dy < h; dy++)
      for (let dx = 0; dx < w; dx++) {
        const X = x + dx
        const Y = y + dy - lift
        if (X < 0 || X >= ART_W || Y < 0 || Y >= ART_H) continue
        const i = (Y * ART_W + X) * 4
        const da = art[i + 3] ?? 0
        const oa = a + da * (1 - a)
        if (oa === 0) continue
        art[i] = (r * a + (art[i] ?? 0) * da * (1 - a)) / oa
        art[i + 1] = (g * a + (art[i + 1] ?? 0) * da * (1 - a)) / oa
        art[i + 2] = (b * a + (art[i + 2] ?? 0) * da * (1 - a)) / oa
        art[i + 3] = oa
      }
  }
  const draw = Object.hasOwn(COSTUMES, costume) ? COSTUMES[costume] : undefined
  if (draw) draw(f, colorOf(costume))
  else crabBody(f)

  const out = new Uint8Array(CRAB_IMAGE.width * CRAB_IMAGE.height * 4)
  for (let y = 0; y < CRAB_IMAGE.height; y++)
    for (let x = 0; x < CRAB_IMAGE.width; x++) {
      const s = (Math.floor(y / PX) * ART_W + Math.floor(x / PX)) * 4
      const o = (y * CRAB_IMAGE.width + x) * 4
      out[o] = Math.round(art[s] ?? 0)
      out[o + 1] = Math.round(art[s + 1] ?? 0)
      out[o + 2] = Math.round(art[s + 2] ?? 0)
      out[o + 3] = Math.round((art[s + 3] ?? 0) * 255)
    }
  const rgba = out.toBase64()
  imageCache.set(cacheKey, rgba)
  return rgba
}

const BASE_CSS = `<style>
.t{fill:#1f1f1f}.s{fill:#6b6b68}.k{fill:#ecebe8}.ln{stroke:#e4e4e1}.tile{fill:#f4f3f0}
@media (prefers-color-scheme: dark){.t{fill:#ececec}.s{fill:#a8a8a4}.k{fill:#2c2c2b}.ln{stroke:#333331}.tile{fill:#262625}}
.live{animation:p 1.6s ease-in-out infinite}@keyframes p{50%{opacity:.3}}
@media (prefers-reduced-motion: reduce){.live{animation:none}}
</style>`

const svg = (W: number, H: number, body: string): string =>
  `<svg xmlns="http://www.w3.org/2000/svg" width="${W}" height="${H}" viewBox="0 0 ${W} ${H}">${BASE_CSS}${CRAB_CSS}${body}</svg>`

const statusMark = (x: number, y: number, status: RunStatus, color: string): string => {
  if (status === 'running') return `<circle class="live" cx="${x}" cy="${y}" r="3.5" fill="${color}"/>`
  if (status === 'done')
    return `<path d="M${x - 5} ${y}l3.5 3.5 6.5-7" fill="none" stroke="#3B9C5F" stroke-width="1.8" stroke-linecap="round" stroke-linejoin="round"/>`
  return `<path d="M${x - 4} ${y - 4}l8 8M${x + 4} ${y - 4}l-8 8" stroke="#D0453F" stroke-width="1.8" stroke-linecap="round"/>`
}

// Tiles wrap: four a row where the pane is wide, three where it is narrow.
const TILE_H = 40
const TILE_GAP = 6
const perRow = (W: number): number => (W >= 520 ? 4 : 3)

export const headerHeight = (W: number, tiles: number): number =>
  Math.ceil(tiles / perRow(W)) * (TILE_H + TILE_GAP) - TILE_GAP + 4

export const headerSvg = (W: number, tiles: [string, string][]): string => {
  const n = perRow(W)
  const tw = (W - TILE_GAP * (n - 1)) / n
  const body = tiles
    .map(([k, v], i) => {
      const x = (i % n) * (tw + TILE_GAP)
      const y = Math.floor(i / n) * (TILE_H + TILE_GAP)
      return `<rect class="tile" x="${x}" y="${y}" width="${tw}" height="${TILE_H}" rx="8"/>
<text class="s" x="${x + 9}" y="${y + 16}" font-family="${FONT}" font-size="11">${xml(fitText(k, 11, tw - 14))}</text>
<text class="t" x="${x + 9}" y="${y + 33}" font-family="${FONT}" font-size="15" font-weight="600" font-variant-numeric="tabular-nums">${xml(fitText(v, 15, tw - 14))}</text>`
    })
    .join('')
  return svg(W, headerHeight(W, tiles.length), body)
}

export const COMPACT_H = 32

// Compact view: one crab per agent (running first, walking), then the totals on the right.
export const compactSvg = (W: number, roles: { role: string; isRunning: boolean }[], totals: string): string => {
  const room = Math.max(1, Math.floor((W - textWidth(totals, 12) - 24) / 34))
  const shown = roles.slice(0, room)
  const more = roles.length - shown.length
  const crabs = shown
    .map(({ role, isRunning }, i) => {
      const costume = costumeOf(role)
      const live = isRunning ? `<circle class="live" cx="${i * 34 + 31}" cy="4" r="3" fill="${colorOf(costume)}"/>` : ''
      return `<g opacity="${isRunning ? 1 : 0.55}">${crab(i * 34, 2, costume, isRunning, 1)}</g>${live}`
    })
    .join('')
  const plus = more ? `<text class="s" x="${shown.length * 34 + 4}" y="21" font-family="${FONT}" font-size="12">+${more}</text>` : ''
  return svg(
    W,
    COMPACT_H,
    `${crabs}${plus}<text class="s" x="${W}" y="21" text-anchor="end" font-family="${FONT}" font-size="12" font-variant-numeric="tabular-nums">${xml(totals)}</text>`,
  )
}

export const ROW_H = 66

// One subagent: its crab, what it does, who it is and what it cost, and a bar of
// its share of the session's tokens.
export const runSvg = (
  W: number,
  r: Run,
  role: string,
  line2: string,
  cost: string,
  stats: string,
  share: number,
): string => {
  const costume = costumeOf(role)
  const color = colorOf(costume)
  const textW = W - 42 - 22
  const costW = textWidth(cost, 11) + 10
  const fillW = Math.round(textW * Math.max(0, Math.min(1, share)))
  return svg(
    W,
    ROW_H,
    `${crab(0, 14, costume, r.status === 'running')}
<text class="t" x="42" y="18" font-family="${FONT}" font-size="13" font-weight="600">${xml(fitText(r.description || r.type, 13, textW))}</text>
<text x="42" y="34" font-family="${FONT}" font-size="11" fill="${color}">${xml(fitText(line2, 11, textW - costW))}</text>
<text class="t" x="${42 + textW}" y="34" text-anchor="end" font-family="${FONT}" font-size="11" font-weight="600" font-variant-numeric="tabular-nums">${xml(cost)}</text>
<text class="s" x="${42 + textW}" y="49" text-anchor="end" font-family="${FONT}" font-size="11" font-variant-numeric="tabular-nums">${xml(fitText(stats, 11, textW))}</text>
<rect class="k" x="42" y="55" width="${textW}" height="4" rx="2"/><rect x="42" y="55" width="${fillW}" height="4" rx="2" fill="${color}"/>
${statusMark(W - 8, 16, r.status, color)}
<line class="ln" x1="0" y1="65.5" x2="${W}" y2="65.5"/>`,
  )
}

export const BAND_H = 30

// The band: one walking crab per running role, then the summary text.
export const bandSvg = (W: number, roles: string[], text: string): string => {
  const fit = Math.max(1, Math.floor((W * 0.5) / 34))
  const shown = roles.slice(0, fit)
  const crabs = shown.map((role, i) => crab(i * 34, 0, costumeOf(role), true, 1)).join('')
  const more = roles.length - shown.length
  const x = shown.length * 34 + 6
  const label = `${more ? `+${more}  ` : ''}${text}`
  return svg(
    W,
    BAND_H,
    `${crabs}<text class="t" x="${x}" y="19" font-family="${FONT}" font-size="13" font-weight="500">${xml(fitText(label, 13, W - x))}</text>`,
  )
}
