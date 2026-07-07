# Spec: Serve Without Tournament

> **Source:** [docs/intent/serve-without-tournament.md](../intent/serve-without-tournament.md) · [docs/ideas/serve-without-tournament.md](../ideas/serve-without-tournament.md)  
> **Parent:** [docs/specs/liga.md](liga.md)  
> **Status:** Specify — approved (ready for Plan when requested)

## Assumptions (confirmed)

1. **Director-first MVP** — full 4-step wizard on `/`; audience phase switching ships in a follow-up. Audience may remain a stub polling `/api/leaderboard`.
2. **Lazy tournament creation** — no `tournament-*` directory until Define (`TournamentCreated`). `liga serve` starts with `tournamentDir = None` when zero incomplete dirs exist.
3. **New event types** — `PlayersSet` and `PlayersLocked` (empty payload). Lock is irreversible.
4. **No legacy data** — no migration or backward-compat paths for old `--new` event logs. All tournaments go through the wizard; `Seed` always requires `PlayersLocked`.
5. **No tournament abandon API** — wrong roster after lock: director deletes the `tournament-*` dir manually.
6. **Player count validation at lock** — 8–64 players required before `PlayersLocked`.
7. **Race-to round numbers** — topology-derived via `BracketRounds.requiredKeys(playerCount)` (not fixed 1–7). Wizard shows one input per required round key; pre-fill 7.
8. **Stack unchanged** — ZIO HTTP, Laminar, append-only JSON replay, localhost director / `--lan` audience bind rules per parent spec.

---

## Objective

Let a tournament director run `liga serve` at the venue with **no CLI tournament setup**, see the **period leaderboard** immediately, and define/run today's double-elimination event entirely from the **Director UI** via a **staged wizard** — while the on-disk event log remains the single source of truth.

### Users

| User | Surface | Goal |
| --- | --- | --- |
| Tournament director | `http://127.0.0.1:5442/` | Start serve instantly; define roster, lock, set race-to, seed; run matches |
| Spectator | `http://<host>:5442/audience` | Read-only display (phase-aware views deferred; period leaderboard when wired) |

### User stories

1. As a director, I run `atbp liga serve --data ./club/` with **no** incomplete tournament and the server starts successfully.
2. As a director, I see the **period leaderboard** and a control to **begin defining** today's tournament without using the terminal.
3. As a director, I complete a **4-step wizard** (Define → Lock → Race-to → Seed) that appends events to the log; restarting `liga serve` mid-wizard resumes at the correct step.
4. As a director, after seed I use the **existing** bracket and match controls unchanged.
5. As a director, I can add **guest players** not in period data; they receive default ratings (1500 / RD 350) at seed time.

### Success criteria

- [ ] `atbp liga serve [--data <dir>] [--port <n>] [--lan]` starts when **zero** incomplete `tournament-*` dirs exist (no `--new` flag).
- [ ] `GET /api/leaderboard` returns period-file ratings when **no** tournament dir is active (HTTP 200).
- [ ] `GET /api/tournament` returns HTTP 200 with `"phase": "none"` and empty defaults when no tournament dir exists.
- [ ] Director wizard persists each step via append-only events; replay derives `phase` unambiguously.
- [ ] `PlayersLocked` is **irreversible** — `PlayersSet` rejected after lock.
- [ ] `POST /api/tournament/seed` rejected unless `PlayersLocked` is replayed.
- [ ] Guest players not in period data seed with **1500 / RD 350 / 0–0** W–L.
- [ ] Sole incomplete tournament still auto-resumes on serve start; multiple incomplete dirs still fail with a list.
- [ ] All new replay/API behaviour covered by `liga/test`; `sbt --client fixup` clean before commit.

---

## Tech Stack

Inherits [docs/specs/liga.md](liga.md) — no new dependencies.

| Layer | Choice |
| --- | --- |
| CLI | zio-cli — remove `--new` from `liga serve` |
| HTTP / replay | ZIO HTTP, zio-json, append-only `EventLog` |
| Frontend | Scala.js + Laminar (`liga-js`) — wizard components under `director/` |
| Tests | zio-test (`*Spec.scala`) |

---

## Commands

```bash
# Format (before commit)
sbt --client fixup

# Compile + test
sbt --client compile
sbt --client liga/test
sbt --client "liga/testOnly *Resume*"
sbt --client "liga/testOnly *Serve*"
sbt --client "liga/testOnly *Replay*"
sbt --client "liga/testOnly *ApiClient*"

# Fast frontend loop
sbt --client ligaJs/fastLinkJS

# Serve (no --new)
sbt run   # then:
atbp liga serve --data ./club/
atbp liga serve --data ./club/ --lan
```

**Removed:**

```bash
atbp liga serve --new "Spring Open"   # no longer supported
```

**Resume rule (updated):**

| Incomplete dirs | Behaviour |
| --- | --- |
| 0 | Serve starts; `tournamentDir = None`; phase `none` |
| 1 | Auto-resume that dir; phase from replay |
| >1 | Fail at startup with dir list (unchanged) |

---

## Project Structure

Touches existing modules only:

```
liga/src/main/scala/ph/samson/atbp/liga/
  model/Types.scala                    → PlayersSetPayload, PlayersLockedPayload; TournamentState.playersLocked
  tournament/events/TournamentEvent.scala → PlayersSet, PlayersLocked cases
  tournament/EventLog.scala            → filename slugs for new events
  tournament/EventCodec.scala         → codecs
  tournament/Replay.scala              → apply new events; phase helper
  tournament/Tournament.scala        → command handlers: setPlayers, lockPlayers, setRoundRaceTo
  bracket/BracketRounds.scala           → requiredKeys(playerCount) pure helper
  tournament/Seed.scala                → lock guard + guest default ratings
  tournament/Resume.scala              → optional dir; remove --new path
  serve/ServeContext.scala             → Option[tournamentDir]; decouple leaderboard load
  serve/Routes.scala                   → tournament/leaderboard read paths for no-dir case
  serve/ApiJson.scala                  → TournamentPhase enum on TournamentResponse
  serve/DirectorRoutes.scala           → wizard POST routes

liga/src/test/scala/ph/samson/atbp/liga/
  tournament/ReplaySpec.scala
  tournament/ResumeSpec.scala
  tournament/SeedSpec.scala            → new or extended
  serve/ReadApiSpec.scala
  serve/WriteApiSpec.scala
  js/ApiClientContractSpec.scala

liga-js/src/main/scala/ph/samson/atbp/liga/js/
  api/Models.scala                     → TournamentPhase, request types
  api/Client.scala                     → wizard POST methods
  director/DirectorApp.scala         → route on phase
  director/WizardView.scala            → new: Define / Lock / Race-to / Seed steps
  director/LeaderboardView.scala       → new: period leaderboard panel

cli/src/main/scala/ph/samson/atbp/cli/Liga.scala  → remove --new option

docs/
  specs/serve-without-tournament.md    → this spec
```

---

## Domain Changes

### Tournament phase

`TournamentPhase` is derived from replay state (and whether a tournament dir exists):

| `phase` | Condition |
| --- | --- |
| `none` | No tournament dir |
| `defining` | `Created` replayed; `PlayersLocked` absent |
| `locked` | `PlayersLocked` replayed; required round race-to values incomplete |
| `raceTo` | All required round race-to values set; bracket not seeded |
| `active` | `BracketSeeded`; not `TournamentCompleted` |
| `completed` | `TournamentCompleted` |

See [Race-to round keys](#race-to-round-keys) for how `requiredRounds` is computed.

### Race-to round keys

When a match goes ready, `MatchLifecycle.resolveRaceTo` looks up `state.roundRaceTo(round)` where `round` is parsed from the match id:

| Match id | Round key used |
| --- | --- |
| `wb-2-1` | `2` |
| `lb-4-1` | `4` |
| `gf-1` | `log₂(bracketSize)` (e.g. `3` for 8 players) |

The wizard must collect a race-to value for **every distinct round key** that will appear in the seeded bracket. Two approaches:

| Approach | Wizard UX | Correctness |
| --- | --- | --- |
| **A. Topology-derived** *(chosen)* | One input per required round; count grows with player count (4 rounds for 8 players, up to 10 for 64) | Always matches what `resolveRaceTo` will look up |
| **B. Fixed 1–7** *(rejected)* | Always 7 inputs, pre-filled 7 | Works for ≤16 players; **insufficient** for 32-player (needs 8) or 64-player (needs 10) brackets |

**Topology-derived implementation:** pure function `BracketRounds.requiredKeys(playerCount: Int): SortedSet[Int]` — build `BracketTopology(Seeding.bracketSize(playerCount))`, map each match id through `MatchLifecycle.bracketRound`, return distinct round integers. Wizard and `raceToComplete` both use this set.

**Examples (approach A):**

| Players | Bracket size | Required round keys |
| --- | --- | --- |
| 8 | 8 | 1, 2, 3, 4 |
| 16 | 16 | 1, 2, 3, 4, 5, 6 |
| 32 | 32 | 1 … 8 |
| 64 | 64 | 1 … 10 |

### New event types

| Event | JSON `type` | Payload | When |
| --- | --- | --- | --- |
| `PlayersSet` | `PlayersSet` | `{ "players": [{ "name": "Alice" }, ...] }` | Define step save |
| `PlayersLocked` | `PlayersLocked` | `{}` | Lock step confirm |

`TournamentCreated` on first wizard submit: `{ "name": "<director input>", "players": [] }`. Creates `tournament-<YYYYMMDD>-<slug>/` and `000001-created.json`.

Replay rules:

- `PlayersSet` → replaces `state.players` (rejected if `playersLocked` or bracket seeded).
- `PlayersLocked` → sets `playersLocked = true` (rejected if already locked, no players, or player count ∉ [8, 64]).
- `RoundRaceToSet` → unchanged (rejected after bracket seeded).
- `TournamentCreated` always has `players: []`; roster comes only from `PlayersSet`.

### Guest player ratings

At seed, `Seed.resolveRatings` assigns period ratings when the name exists; otherwise:

```scala
PlayerRating(player, rating = 1500, rd = 350, wins = 0, losses = 0)
```

(Uses repo `glicko.Tuning` constants.)

---

## HTTP API

### Read (audience + director)

| Method | Path | When no tournament dir |
| --- | --- | --- |
| GET | `/api/leaderboard` | Period ratings from `--data` (unchanged source) |
| GET | `/api/tournament` | `200` + `{ "phase": "none", "name": "", "players": [], ... }` |

`TournamentResponse` gains `"phase": "<TournamentPhase>"` (string enum). Existing fields unchanged for `active` tournaments.

### Write (director localhost only)

| Method | Path | Body | Appends |
| --- | --- | --- | --- |
| POST | `/api/tournament/create` | `{ "name": "Spring Open" }` | `TournamentCreated` (+ creates dir on first call) |
| POST | `/api/tournament/players` | `{ "players": [{ "name": "..." }] }` | `PlayersSet` |
| POST | `/api/tournament/lock` | `{}` | `PlayersLocked` |
| POST | `/api/tournament/race-to` | `{ "roundRaceTo": { "1": 7, "2": 7 } }` | one `RoundRaceToSet` per entry |
| POST | `/api/tournament/seed` | `{ "roundRaceTo": {} }` optional | `BracketSeeded` (+ race-to events if not already set) |

All write routes return updated `TournamentResponse` JSON (same as existing match routes).

**Errors (400):** phase violations (e.g. lock before players set), player count bounds, duplicate lock, seed before lock.

---

## Director UI

Route `DirectorApp` on `tournament.phase`:

| Phase | UI |
| --- | --- |
| `none` | Period leaderboard + “Start tournament” (name input → create) |
| `defining` | Name (read-only if set), player picker (period names + free-text add), save → `PlayersSet`, “Lock roster” disabled until 8–64 players |
| `locked` | Read-only roster + race-to inputs per required round (pre-filled 7) → `race-to` |
| `raceTo` | Summary + “Seed bracket” → `seed` |
| `active` / `completed` | Existing `BracketView` + `MatchPanel` (unchanged) |

On mount: fetch `/api/tournament` and `/api/leaderboard` in parallel.

---

## Code Style

Follow [docs/specs/liga.md](liga.md). Example — pure phase derivation at the replay layer:

```scala
object TournamentPhase {
  def derive(state: TournamentState, hasDir: Boolean): TournamentPhase =
    if (!hasDir) TournamentPhase.None
    else if (state.completed) TournamentPhase.Completed
    else if (state.bracket.nonEmpty) TournamentPhase.Active
    else if (!state.playersLocked) TournamentPhase.Defining
    else if (!raceToComplete(state)) TournamentPhase.Locked
    else TournamentPhase.RaceTo
}
```

Wizard command handlers live in `tournament.Tournament` (pure `Either[Error, Event]`); `ServeContext` appends at the edge.

---

## Testing Strategy

| Concern | Location | Approach |
| --- | --- | --- |
| New event replay | `ReplaySpec` | Fixture logs with `PlayersSet` / `PlayersLocked`; phase derivation |
| Lock irreversibility | `ReplaySpec` | `PlayersSet` after lock → replay error |
| Seed requires lock | `SeedSpec` | No `PlayersLocked` → seed rejected |
| Guest ratings | `SeedSpec` | Unknown name → 1500/350 |
| Resume no dir | `ResumeSpec` | Zero incomplete → `None`; remove `--new` tests |
| Leaderboard without dir | `ReadApiSpec` | GET `/api/leaderboard` with `Option` context |
| Phase none | `ReadApiSpec` | GET `/api/tournament` → `phase: none` |
| Wizard writes | `WriteApiSpec` | create → players → lock → race-to → seed sequence |
| JS contract | `ApiClientContractSpec` | `phase` field + new POST bodies parse |

**Manual (v1):** full wizard E2E in browser; resume after JVM kill mid-wizard.

---

## Boundaries

### Always do

- Run `sbt --client liga/test` before commits
- Run `sbt --client fixup` before commits (whole project)
- Append-only events — never mutate event files
- Keep replay / phase logic pure and unit-tested
- Director write routes localhost-only (`BindConfig.isLocalDirector`)

### Ask first

- Audience phase UI scope expansion beyond period leaderboard stub
- New dependencies

### Never do

- Re-introduce `--new` CLI flag
- Unlock / edit roster after `PlayersLocked`
- In-place mutation of event files
- 404 for normal pre-tournament `GET /api/tournament` state

---

## Out of Scope (this feature)

- Audience phase switching (entrant leaderboard, live bracket by phase)
- Tournament abandon/delete API
- Multiple incomplete tournament resolution UI
- Bracket / match control redesign post-seed
- WebSockets

---

## Resolved Decisions

| # | Topic | Decision |
| --- | --- | --- |
| 1 | Legacy event logs | **None.** No migration; wizard-only path. |
| 2 | Race-to batch POST | One request → multiple `RoundRaceToSet` events. |
| 3 | Define player picker | Period leaderboard multi-select **plus** free-text guest names. |
| 4 | Completed dir on serve | Serve only resumes **incomplete** dirs; completed dirs are never active. |

---

## Success Criteria Checklist (implementation gate)

Use this to exit Specify → Plan:

- [x] Human reviewed assumptions
- [x] Race-to round keys: topology-derived (option A)
- [x] API table and event schemas approved
- [x] Audience deferral accepted
- [x] No legacy compat
