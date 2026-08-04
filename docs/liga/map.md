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
- `liga/.../tournament/MatchWaitTiming.scala` — API-only wait/Done Instant
  projection from the event log (not on domain `BracketMatch`)

## Serve / HTTP

- `liga/.../serve/` — `Server`, `Routes`, `DirectorRoutes`, `ServeContext`, static assets
- Director write APIs + audience/director read APIs
- Persistence: append-only JSON under tournament data dir
- Tournament JSON bracket matches may include optional `waitStartedAt` /
  `completedAt` (ISO Instants) from `MatchWaitTiming`; domain model unchanged

## Director UI (`liga-js`)

Active-phase layout (`DirectorApp.mainLayout`): CSS grid — bracket column + match panel column (`director.css` `.main-layout`).

| File | Role |
|------|------|
| `director/DirectorApp.scala` | Root app, phase routing, selection, actions |
| `director/BracketView.scala` | Match list by section/round; ~15s elapsed chip tick |
| `director/BracketLayout.scala` | Grouping/order, director Ready strip, elapsed chip helpers |
| `director/AppliedHandicapView.scala` | Player names + handicap markers on bracket rows |
| `director/MatchScoreView.scala` | Shared score/bye rendering for director + audience brackets |
| `director/MatchPanel.scala` | Ready / handicap / start / score controls |
| `director/WizardView.scala` | Pre-seed setup (roster, race-to, seed) |
| `director/DirectorGuidance.scala` | Copy / error hints |
| `api/Client.scala` + `Models.scala` | HTTP client + DTOs (incl. optional wait fields) |

Audience bracket order (`BracketLayout.groupMatches`): unfinished rounds above
fully Completed rounds; within each band Grand Final → Losers → Winners, later
rounds above earlier; within a round Ready → Pending → Started → Completed then
`match.id`. Empty Pending slots with neither player are hidden. Intent:
`docs/intent/director-audience-bracket-order.md`.

Director list (`BracketLayout.directorGroupMatches`): Live strip at top
(`Started`, round then seed index) with static started-time chip from
`startedAt`; Ready strip below sorted longest-wait first (equal wait → earlier
rounds, then seed index); lower list = `groupMatches` on non-Live ∩ non-Ready.
Elapsed `HH:mm` chip from `waitStartedAt` (Ready + Pending-with-one) or
`completedAt` (Done; none on bye). Spec: `SPEC.md` /
`docs/intent/td-match-wait-indicators.md`.


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
| `docs/intent/td-match-wait-indicators.md` | TD wait / Done elapsed + Ready strip intent |
| `docs/ideas/td-match-wait-indicators.md` | Wait-indicator direction / MVP in-out |
| `SPEC.md` | Wait-indicator acceptance + timing rule table |
| `tasks/plan.md` / `tasks/todo.md` | Wait-indicator task breakdown + checklist |

## Known UX pressure (Active phase)

Large brackets bury live work under early/irrelevant slots; director scroll can take the side match panel out of view. Shipped list visibility/order: `docs/intent/director-audience-bracket-order.md` (panel placement unchanged).
