# Serve Without Tournament

## Problem Statement

How might we let a billiards director start `liga serve` immediately at the venue,
see period standings, and define/run today's tournament entirely from the browser
— with the on-disk event log as the only source of truth?

## Recommended Direction

Remove `--new` and allow serve to start with no tournament directory. Period
leaderboard is available via `/api/leaderboard` from day one. The director UI
hosts a 4-step wizard (Define → Lock players → Race-to → Seed); each step
appends explicit events (`Created`, `PlayersSet`, `PlayersLocked`,
`RoundRaceToSet`, `BracketSeeded`). The tournament directory is created lazily on the first wizard
action (`Created`). After seed, existing bracket/match controls are unchanged.
Audience phase switching (period → entrants → live bracket) ships after the
director wizard MVP.

## Key Assumptions to Validate

- [ ] Director can run a full tournament with audience on placeholder/period view only
- [ ] `PlayersLocked` event gives unambiguous resume step after serve restart
- [ ] Optional tournament dir doesn't break LAN audience polling (`/api/leaderboard` works with no dir)
- [x] Guest players (not in period data) get 1500/RD 350 at seed time — extend `Seed.resolveRatings`

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

## Decisions

### `PlayersLocked` payload — empty marker

`PlayersLocked` carries **no player list** (unit payload). The roster lives in
prior `PlayersSet` events (see below); lock is an irreversible phase gate.

Replay adds `playersLocked: Boolean` (or derives it from the event's presence).
`Seed.buildEvents` requires `playersLocked` before seeding.

**Why not snapshot players in the lock event?** The list is already in the log
via `PlayersSet`. A snapshot duplicates data and can drift if we ever add
correction events. The lock event's job is to freeze the *phase*, not duplicate
the roster.

**Companion event: `PlayersSet`** — emitted during Define (and on resume at that
step). Payload: `List[Player]`. Replay replaces `state.players`. Rejected once
`PlayersLocked` exists. `TournamentCreated` carries **name only** (players start
empty).

### `/api/tournament` when no tournament — `200` + `phase: "none"`

Return **HTTP 200** with a `TournamentResponse` that includes a `phase` field:

| `phase` | Meaning |
| --- | --- |
| `none` | No tournament dir yet |
| `defining` | `Created` (+ optional `PlayersSet`), not locked |
| `locked` | `PlayersLocked`, race-to incomplete |
| `raceTo` | All round race-to values set, not seeded |
| `active` | Bracket seeded, tournament in progress |
| `completed` | `TournamentCompleted` replayed |

When `phase` is `none`, other fields are empty defaults (`name: ""`, `players:
[]`, `bracket: null`, etc.). `DirectorApp` routes on `phase` instead of treating
missing tournament as an error.

**Why not 404?** The director SPA polls `/api/tournament` on mount. A 404 forces
every client through failure handling for the normal pre-tournament state. The
contract fixture in `ApiClientContractSpec` already uses an empty tournament
envelope — extend it with `phase` rather than branching on status codes.

### Player edits after lock — no (irreversible)

Once `PlayersLocked` is appended, **no further `PlayersSet` events** are
accepted. The director must verify the roster on the Define step before locking;
the lock step is the signal for audience to switch to the entrant leaderboard
(when audience work ships).

If the roster is wrong after lock: abandon the incomplete tournament dir and
start a new one from Define. No unlock/re-lock in v1 — it would invalidate the
audience phase contract and complicate replay validation.

### Guest player ratings at seed (resolves assumption)

`Seed.resolveRatings` today fails on unknown players (`MissingPlayerError`).
Change it to assign **1500 / RD 350 / 0 W–L** (repo `Tuning` defaults) for
names not in period data. Period players keep computed ratings. This happens at
`BracketSeeded`, not at lock — the lock-phase entrant view looks up period
ratings at read time with the same default fallback.
