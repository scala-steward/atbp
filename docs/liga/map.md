# Liga project map

Load only the section you need for the current task.

## Modules

| Module | Path | Owns |
|--------|------|------|
| `liga` | `liga/` | CLI renderers, Glicko2 engine, tournament lifecycle, HTTP serve, persistence |
| `liga-common` | `liga-common/` | Cross-compiled types, handicap math, race-to scopes/wizard, bounds |
| `liga-js` | `liga-js/` | Laminar director + audience SPAs, API client models |

`liga` depends on `liga-common.jvm` and embeds `liga-js` fastLinkJS output under web resources.

## Domain (JVM + shared)

### Ratings / periods
- `liga/.../glicko/` — Glicko2, leaderboard
- `liga/.../io/` — period load/write, data layout, codecs
- Batch period rules: `docs/ideas/liga-batch-period-ratings.md`

### Handicap
- `liga-common/.../handicap/` — suggestion + win probability (shared with JS preview)
- CLI: `liga/.../cli/HandicapRenderer.scala`

### Bracket topology
- `liga/.../bracket/` — gen, seeding, advancement, byes
- `liga-common/.../bracket/` — `RaceToScopes`, `RaceToWizard`, `TournamentBounds`
- Section race-to: `docs/ideas/section-aware-race-to.md`

### Tournament lifecycle
- `liga/.../tournament/` — phases, events, replay/resume, validation, period emission
- Match flow: ready → apply handicap → start → record result

## Serve / HTTP

- `liga/.../serve/` — `Server`, `Routes`, `DirectorRoutes`, `ServeContext`, static assets
- Director write APIs + audience/director read APIs
- Persistence: append-only JSON under tournament data dir

## Director UI (`liga-js`)

Active-phase layout (`DirectorApp.mainLayout`): CSS grid — bracket column + match panel column (`director.css` `.main-layout`).

| File | Role |
|------|------|
| `director/DirectorApp.scala` | Root app, phase routing, selection, actions |
| `director/BracketView.scala` | Match list by section/round |
| `director/BracketLayout.scala` | Grouping/order, labels, winner-side helper, actionable detection |
| `director/AppliedHandicapView.scala` | Player names + handicap markers on bracket rows |
| `director/MatchScoreView.scala` | Shared score/bye rendering for director + audience brackets |
| `director/MatchPanel.scala` | Ready / handicap / start / score controls |
| `director/WizardView.scala` | Pre-seed setup (roster, race-to, seed) |
| `director/DirectorGuidance.scala` | Copy / error hints |
| `api/Client.scala` + `Models.scala` | HTTP client + DTOs |

Bracket list order (`BracketLayout.groupMatches`): unfinished rounds above fully
Completed rounds; within each band Grand Final → Losers → Winners, later rounds
above earlier; within a round Ready → Pending → Started → Completed then `match.id`.
Empty Pending slots with neither player are hidden. Intent:
`docs/intent/director-audience-bracket-order.md`.

## Audience UI

- `audience/AudienceApp.scala`, `AudienceBracketView.scala`
- Separate entry (`MainAudience`); poll-based; read-only

## Docs index

| Doc | Use when |
|-----|----------|
| `docs/intent/liga.md` | Confirmed product intent |
| `docs/ideas/liga.md` | Architecture direction + MVP in/out |
| `docs/ideas/section-aware-race-to.md` | Per-scope race-to |
| `docs/ideas/liga-batch-period-ratings.md` | Period batching |
| `docs/ideas/bye-bracket-display.md` | Bye display |
| `docs/ideas/grand-final-labels.md` | GF labeling |
| `docs/intent/paste-roster.md` | Roster paste |
| `docs/intent/serve-without-tournament.md` | Serve without active tournament |
| `docs/intent/director-audience-bracket-order.md` | Director + audience list visibility/order |

## Known UX pressure (Active phase)

Large brackets bury live work under early/irrelevant slots; director scroll can take the side match panel out of view. Shipped list visibility/order: `docs/intent/director-audience-bracket-order.md` (panel placement unchanged).
