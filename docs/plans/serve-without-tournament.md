# Implementation Plan: Serve Without Tournament

> **Source:** [docs/specs/serve-without-tournament.md](../specs/serve-without-tournament.md)  
> **Status:** Done

## Overview

Let a tournament director run `liga serve` with **no CLI tournament setup**, see the **period leaderboard** immediately, and define/run today's double-elimination event entirely from the **Director UI** via a **4-step wizard** (Define → Lock → Race-to → Seed). The on-disk event log remains the single source of truth. The `--new` CLI flag is removed; lazy tournament creation happens on the first wizard `POST /api/tournament/create`.

## Architecture Decisions

| Decision | Rationale |
|----------|-----------|
| **`ServeContext.tournamentDir: Option[File]`** | Spec mandate; decouples period leaderboard from tournament replay when no dir exists |
| **`Resume.resolve → Option[File]`** | Zero incomplete dirs → `None`; sole incomplete → `Some(dir)`; multiple → fail (unchanged) |
| **Lazy dir creation on `POST /api/tournament/create`** | Reuses `Resume.tournamentDirName` / `slugify`; first event is `TournamentCreated` with `players: []` |
| **`TournamentPhase` derived in replay layer** | Pure, unit-testable; shared by API and UI via `TournamentResponse.phase` |
| **`BracketRounds.requiredKeys(playerCount)`** | Topology-derived round keys match `MatchLifecycle.bracketRound` lookups at seed time |
| **Guest ratings at seed** | `1500 / RD 350 / 0–0` via `glicko.Tuning.Default`; period players keep computed ratings |
| **`PlayersLocked` irreversible** | `PlayersSet` rejected after lock; `Seed` requires `playersLocked` |
| **Director-first MVP** | Audience phase switching deferred; audience may remain period-leaderboard stub |
| **No legacy migration** | Wizard-only path; remove `--new` and tests that depend on it |

## Dependency Graph

```
Task 1: Domain model + event types + codec + EventLog slugs
    │
    ├── Task 2: BracketRounds.requiredKeys + TournamentPhase + Replay apply
    │       │
    │       └── Task 3: Tournament command handlers (setPlayers, lockPlayers, setRoundRaceTo)
    │               │
    │               └── Task 4: Seed lock guard + guest default ratings
    │
    ├── Task 5: Resume Option[File] + CLI remove --new
    │       │
    │       └── Task 6: ServeContext Option[tournamentDir] + lazy create
    │               │
    │               ├── Task 7: Read API (phase none, leaderboard without dir)
    │               │
    │               └── Task 8: Write API (wizard POST routes)
    │                       │
    │                       ├── Task 9: JS Models + ApiClient wizard methods
    │                       │
    │                       └── Task 10: Director UI (LeaderboardView, WizardView, phase routing)
    │
    └── Task 11: Integration tests + spec success-criteria gate
```

Implementation order follows the graph bottom-up: domain and replay first, then serve layer, then HTTP, then UI.

---

## Task List

### Phase 1: Domain & Replay

---

## Task 1: Domain model and new event types

**Description:** Add `playersLocked` to `TournamentState`, new payload types (`PlayersSetPayload`, `PlayersLockedPayload`), `PlayersSet` and `PlayersLocked` cases on `TournamentEvent`, JSON codecs, and EventLog filename slugs (`players-set`, `players-locked`).

**Acceptance criteria:**
- [x] `TournamentState` has `playersLocked: Boolean = false`
- [x] `PlayersSet` / `PlayersLocked` round-trip through `EventCodec`
- [x] `EventLog.filenameFor` produces `00000N-players-set.json` and `00000N-players-locked.json`

**Verification:**
- [x] Tests pass: `sbt --client "liga/testOnly *EventCodec*"`
- [x] Build succeeds: `sbt --client compile`

**Dependencies:** None

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/model/Types.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/events/TournamentEvent.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/EventCodec.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/EventLog.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/tournament/EventCodecSpec.scala`

**Estimated scope:** Small (1–2 files core + tests)

---

## Task 2: BracketRounds, TournamentPhase, and Replay rules

**Description:** Add `BracketRounds.requiredKeys(playerCount)` (topology-derived round keys), `TournamentPhase` enum with `derive(state, hasDir)` and `raceToComplete(state)`, and extend `Replay.applyEvent` for `PlayersSet` / `PlayersLocked` with validation rules from the spec.

**Acceptance criteria:**
- [x] `requiredKeys(8)` → `{1,2,3,4}`; `requiredKeys(64)` → `{1…10}`
- [x] Phase derivation matches spec table (`none`, `defining`, `locked`, `raceTo`, `active`, `completed`)
- [x] `PlayersSet` replaces `state.players`; rejected if `playersLocked` or bracket seeded
- [x] `PlayersLocked` sets `playersLocked = true`; rejected if already locked, no players, or count ∉ [8, 64]

**Verification:**
- [x] Tests pass: `sbt --client "liga/testOnly *Replay*"`
- [x] Tests pass: `sbt --client "liga/testOnly *Bracket*"` (new `BracketRoundsSpec` if separate)

**Dependencies:** Task 1

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/bracket/BracketRounds.scala` (new)
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/TournamentPhase.scala` (new)
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/Replay.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/tournament/ReplaySpec.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/bracket/BracketRoundsSpec.scala` (new)

**Estimated scope:** Medium (3–5 files)

---

## Task 3: Tournament wizard command handlers

**Description:** Add pure command handlers in `tournament.Tournament`: `setPlayers`, `lockPlayers`, `setRoundRaceTo` (batch), and `create` (produces `TournamentCreated` with empty players). Each returns `Either[Error, Event]` or `List[Event]` per existing conventions.

**Acceptance criteria:**
- [x] `setPlayers` validates not locked / not seeded
- [x] `lockPlayers` validates player count 8–64 and not already locked
- [x] `setRoundRaceTo` rejects after bracket seeded; accepts map of round → race-to
- [x] Errors surface as `MatchLifecycle.Error` or dedicated wizard errors with clear messages

**Verification:**
- [x] Tests pass: `sbt --client "liga/testOnly *Tournament*"` (extend or add wizard command tests)

**Dependencies:** Task 2

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/Tournament.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/tournament/TournamentSpec.scala`

**Estimated scope:** Small (1–2 files)

---

## Task 4: Seed lock guard and guest default ratings

**Description:** Require `state.playersLocked` before seeding. Change `resolveRatings` to assign `PlayerRating(player, Tuning.Default.initRating, Tuning.Default.initRd, 0, 0)` for names not in period data instead of `MissingPlayerError`.

**Acceptance criteria:**
- [x] Seed without `PlayersLocked` in replay → rejected with clear error
- [x] Guest player "Zara" not in period files seeds with 1500 / 350 / 0–0
- [x] Period players still use computed period ratings

**Verification:**
- [x] Tests pass: `sbt --client "liga/testOnly *Seed*"` (new `SeedSpec` or extend existing)

**Dependencies:** Task 2

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/Seed.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/tournament/SeedSpec.scala` (new)

**Estimated scope:** Small (1–2 files)

---

### Checkpoint: Domain
- [x] `ReplaySpec` covers `PlayersSet`, `PlayersLocked`, phase derivation, lock irreversibility
- [x] `SeedSpec` covers lock requirement and guest ratings
- [x] `sbt --client "liga/testOnly *Replay*"` and `*Seed*` pass

---

### Phase 2: Serve Infrastructure

---

## Task 5: Resume optional dir and CLI cleanup

**Description:** Change `Resume.resolve` to return `Option[File]`: `None` when zero incomplete dirs, `Some(dir)` for sole incomplete, fail for multiple. Remove `--new` option from `liga serve` CLI and delete `createNew` from serve startup path (creation moves to API). Update startup log message for no-tournament case.

**Acceptance criteria:**
- [x] Zero incomplete dirs → `ZIO.succeed(None)` (not an error)
- [x] Sole incomplete → `Some(dir)` (unchanged behaviour)
- [x] Multiple incomplete → fail with dir list (unchanged)
- [x] `--new` flag removed from CLI; help text updated
- [x] `ResumeSpec`: remove/replace `--new` tests; add zero-dir success test

**Verification:**
- [x] Tests pass: `sbt --client "liga/testOnly *Resume*"`
- [x] Manual: `atbp liga serve --data ./club/` starts when no tournament dirs exist (covered by `ServeCheckpointSpec`)

**Dependencies:** None (can start in parallel with Tasks 1–4, but Task 6 depends on this)

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/Resume.scala`
- `cli/src/main/scala/ph/samson/atbp/cli/Liga.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/tournament/ResumeSpec.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/serve/ServeCheckpointSpec.scala`

**Estimated scope:** Small (2–3 files)

---

## Task 6: ServeContext Option dir and lazy tournament creation

**Description:** Change `ServeContext.tournamentDir` to `Option[File]`. When `None`, `loadTournament` returns empty/default state; `loadLeaderboard` loads period files only; `nextSeq` / append require `Some(dir)`. Add `createTournament(name)` that creates dir + appends `TournamentCreated`, and wizard append helpers that set dir on first create.

**Acceptance criteria:**
- [x] `ServeContext(dataDir, tournamentDir = None)` compiles and works for read paths
- [x] `createTournament` creates `tournament-<date>-<slug>/` with `000001-created.json`
- [x] Existing match commands unchanged when `Some(dir)` with seeded bracket
- [x] All test helpers constructing `ServeContext` updated

**Verification:**
- [x] Tests pass: `sbt --client "liga/testOnly *Serve*"`
- [x] Build succeeds: `sbt --client compile`

**Dependencies:** Task 5

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/serve/ServeContext.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/serve/*.scala` (all specs using `ServeContext`)
- `liga/src/test/scala/ph/samson/atbp/liga/js/ApiClientContractSpec.scala`

**Estimated scope:** Medium (3–5 files)

---

### Checkpoint: Serve startup
- [x] `liga serve` starts with zero incomplete dirs (no `--new`)
- [x] Sole incomplete dir still auto-resumes; multiple still fails
- [x] `ResumeSpec` updated; `--new` tests removed

---

### Phase 3: HTTP API

---

## Task 7: Read API for no-tournament state

**Description:** Add `phase: TournamentPhase` to `TournamentResponse`. When `tournamentDir` is `None`, `GET /api/tournament` returns HTTP 200 with `phase: "none"` and empty defaults; `GET /api/leaderboard` returns period ratings without loading tournament replay.

**Acceptance criteria:**
- [x] `GET /api/tournament` with no dir → 200, `"phase": "none"`, empty name/players/bracket
- [x] `GET /api/leaderboard` with no dir → 200, period file ratings
- [x] Active tournament responses include correct `phase` (`active`, `defining`, etc.)
- [x] No 404 for normal pre-tournament state

**Verification:**
- [x] Tests pass: `sbt --client "liga/testOnly *ReadApi*"`

**Dependencies:** Task 2, Task 6

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/serve/ApiJson.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/serve/Routes.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/serve/ReadApiSpec.scala`

**Estimated scope:** Small (2–3 files)

---

## Task 8: Director wizard write routes

**Description:** Add director POST routes: `/api/tournament/create`, `/players`, `/lock`, `/race-to` (batch → multiple `RoundRaceToSet`). Wire to `ServeContext` + `Tournament` command handlers. Return updated `TournamentResponse`. Reject seed unless `PlayersLocked`.

**Acceptance criteria:**
- [x] Full wizard sequence persists events: create → players → lock → race-to → seed
- [x] Phase violations return HTTP 400 with message
- [x] `POST /api/tournament/seed` rejected when not locked
- [x] Race-to POST appends one event per map entry
- [x] All routes localhost-only (existing `directorOnly` guard)

**Verification:**
- [x] Tests pass: `sbt --client "liga/testOnly *WriteApi*"`

**Dependencies:** Task 3, Task 4, Task 6, Task 7

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/serve/DirectorRoutes.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/serve/ServeContext.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/serve/WriteApiSpec.scala`
- `liga/src/test/resources/periods/tournament-20260707-subok/` (fixture if needed)

**Estimated scope:** Medium (3–4 files)

---

### Checkpoint: API
- [x] `ReadApiSpec`: `phase: none`, leaderboard without tournament dir
- [x] `WriteApiSpec`: create → players → lock → race-to → seed sequence
- [x] `sbt --client "liga/testOnly *ReadApi*"` and `*WriteApi*` pass

---

### Phase 4: Director UI

---

## Task 9: JS API layer (Models + Client)

**Description:** Add `TournamentPhase` enum, `phase` field on `TournamentResponse`, request types for wizard POSTs, and `ApiClient` methods: `createTournament`, `setPlayers`, `lockPlayers`, `setRaceTo`, updated `seed`.

**Acceptance criteria:**
- [x] JS `TournamentResponse` decodes `phase` from server JSON
- [x] All wizard POST methods send correct JSON bodies
- [x] `ApiClientContractSpec` covers `phase` field and new request encoders

**Verification:**
- [x] Tests pass: `sbt --client "liga/testOnly *ApiClient*"`
- [x] Build succeeds: `sbt --client liga-js/fastLinkJS`

**Dependencies:** Task 7, Task 8

**Files likely touched:**
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/api/Models.scala`
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/api/Client.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/js/ApiClientContractSpec.scala`

**Estimated scope:** Small (2–3 files)

---

## Task 10: Director UI — LeaderboardView, WizardView, phase routing

**Description:** Replace the single `setupPanel` in `DirectorApp` with phase-based routing. Add `LeaderboardView` (period standings) and `WizardView` (Define / Lock / Race-to / Seed steps). Fetch `/api/tournament` and `/api/leaderboard` in parallel on mount. Pre-fill race-to inputs with 7; show one input per `requiredKeys` count (derive from locked player count or API `phase` + player count).

**Acceptance criteria:**
- [x] `phase: none` → period leaderboard + "Start tournament" name input
- [x] `phase: defining` → player picker (period multi-select + free-text guest) + save + lock (disabled until 8–64)
- [x] `phase: locked` → read-only roster + topology-derived race-to inputs
- [x] `phase: raceTo` → summary + seed button
- [x] `phase: active` / `completed` → existing `BracketView` + `MatchPanel` unchanged
- [x] Restart mid-wizard resumes at correct step (relies on server phase)

**Verification:**
- [x] Build succeeds: `sbt --client liga-js/fastLinkJS`
- [ ] Manual check: full wizard E2E in browser at `http://127.0.0.1:5442/` (optional v1 gate)

**Dependencies:** Task 9

**Files likely touched:**
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/director/DirectorApp.scala`
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/director/WizardView.scala` (new)
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/director/LeaderboardView.scala` (new)

**Estimated scope:** Medium (3–4 files)

---

### Checkpoint: UI
- [x] `ApiClientContractSpec` parses `phase` and new POST bodies
- [x] `sbt --client liga-js/fastLinkJS` succeeds
- [ ] Manual: full wizard E2E in browser (optional v1 gate)

---

### Phase 5: Gate

---

## Task 11: Integration tests and success-criteria gate

**Description:** Update `ServeCheckpointSpec` for no-dir startup and mid-wizard resume. Add test fixtures for wizard event sequences. Verify all spec success criteria. Run full test suite and fixup loop.

**Acceptance criteria:**
- [x] All items in spec "Success criteria" section checked off
- [x] `sbt --client liga/test` passes
- [x] `sbt --client fixup && git status` clean
- [x] No `--new` references remain in code or docs (except historical changelog if any)

**Verification:**
- [x] `sbt --client liga/test`
- [x] `sbt --client fixup && git status`
- [x] Spec success-criteria checklist in [docs/specs/serve-without-tournament.md](../specs/serve-without-tournament.md) reviewed

**Dependencies:** Tasks 1–10

**Files likely touched:**
- `liga/src/test/scala/ph/samson/atbp/liga/serve/ServeCheckpointSpec.scala`
- `liga/src/test/resources/tournaments/` (wizard fixture logs)
- `docs/specs/serve-without-tournament.md` (status → Implementing / Done when complete)

**Estimated scope:** Medium (3–5 files)

---

### Checkpoint: Complete
- [x] All spec success criteria met
- [x] `sbt --client liga/test` passes
- [x] `sbt --client fixup` leaves `git status` clean
- [x] Ready for review

---

## Remaining (optional / follow-up)

| Item | Notes |
|------|-------|
| Manual browser E2E | Optional v1 gate — wizard flow is covered by `WriteApiSpec` and `ServeCheckpointSpec` |

---

## Parallelization Opportunities

| Safe to parallelize | Must be sequential |
|---------------------|-------------------|
| Task 9 (JS models) after Task 8 API shapes are stable | Tasks 1 → 2 → 3 → 4 |
| Task 11 fixture JSON files while Tasks 7–8 in progress | Task 6 before Tasks 7–8 |
| Manual browser E2E after Task 10 | Task 5 before Task 6 |

---

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| `ServeContext` `Option[File]` ripples through all tests/helpers | Med | Update test fixtures in one pass (Task 6); grep for `ServeContext(` |
| Race-to completeness check diverges from `resolveRaceTo` | High | Single source: `BracketRounds.requiredKeys`; share in phase derivation and wizard UI |
| Existing seeded tournaments lack `PlayersLocked` in log | Low | Spec: no legacy compat; old partial logs outside wizard path are out of scope |
| Wizard create races with concurrent requests | Low | Director localhost-only; single director assumption unchanged |
| `TournamentCreated` with embedded players in old fixtures | Med | Replay still accepts payload players on `Created`; new wizard always emits `players: []` then `PlayersSet` |

---

## Open Questions

None — spec assumptions are confirmed and marked approved.

---

## Suggested Session Groupings

| Session | Tasks | Deliverable |
|---------|-------|-------------|
| 1 | 1–4 | Domain events, replay, seed rules tested |
| 2 | 5–6 | `liga serve` starts without tournament |
| 3 | 7–8 | Wizard API complete |
| 4 | 9–10 | Director wizard UI |
| 5 | 11 | Integration gate + merge-ready |

---

## Out of Scope (this plan)

- Audience phase switching (entrant leaderboard, live bracket by phase)
- Tournament abandon/delete API
- Multiple incomplete tournament resolution UI
- Bracket / match control redesign post-seed
- WebSockets
