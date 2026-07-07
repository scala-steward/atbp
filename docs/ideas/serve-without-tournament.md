# Serve Without Tournament

## Problem Statement

How might we let a billiards director start `liga serve` immediately at the venue,
see period standings, and define/run today's tournament entirely from the browser
— with the on-disk event log as the only source of truth?

## Recommended Direction

Remove `--new` and allow serve to start with no tournament directory. Period
leaderboard is available via `/api/leaderboard` from day one. The director UI
hosts a 4-step wizard (Define → Lock players → Race-to → Seed); each step
appends explicit events (`Created`, `PlayersLocked`, `RoundRaceToSet`,
`BracketSeeded`). The tournament directory is created lazily on the first wizard
action (`Created`). After seed, existing bracket/match controls are unchanged.
Audience phase switching (period → entrants → live bracket) ships after the
director wizard MVP.

## Key Assumptions to Validate

- [ ] Director can run a full tournament with audience on placeholder/period view only
- [ ] `PlayersLocked` event gives unambiguous resume step after serve restart
- [ ] Optional tournament dir doesn't break LAN audience polling (`/api/leaderboard` works with no dir)
- [ ] Guest players (not in period data) get 1500/RD 350 at seed time — verify in Seed.buildEvents

## MVP Scope

**In:**

- `liga serve` starts with zero incomplete tournaments (no `--new`)
- `ServeContext` with `Option[tournamentDir]`; leaderboard decoupled from tournament load
- New events + replay phase (`wizardPhase`)
- Director wizard UI for all 4 steps + resume to correct step
- Write APIs for create / update players / lock / race-to / seed
- Auto-resume sole incomplete tournament (unchanged rule)

**Out (this release):**

- Audience phase switching (lock leaderboard, live bracket) — follow-up
- Bracket/match control redesign
- Multiple incomplete tournament resolution UI
- CLI tournament creation

## Not Doing (and Why)

- **Derived wizard phase without new events** — ambiguous lock boundary; explicit events preferred
- **Audience wizard views in v1** — scope control; director path is the blocker for venue day
- **Multiple concurrent incomplete tournaments** — unchanged; explicit error is fine
- **WebSockets for audience** — polling model is established and sufficient

## Open Questions

- Exact `PlayersLocked` payload shape (player list snapshot vs reference to Created.players)?
- Should `/api/tournament` return 404 or `{ phase: "none" }` for JS client ergonomics?
- Wizard "Define" step: can players be edited after lock, or is lock irreversible?
