# Implementation Plan: Liga

> **Source:** [docs/specs/liga.md](../specs/liga.md)  
> **Status:** Plan — open questions resolved; ready for implementation

## Overview

Liga adds `atbp liga` — a billiards club tool for **reproducible Glicko2 ratings**, **handicap suggestions**, and **director-run double-elimination tournaments** with a separate audience display. Work spans two new SBT modules (`liga` JVM core, `liga-js` Scala.js/Laminar frontend), wired into the existing `cli` module. The plan follows the spec's dependency order: pure core logic first, then persistence, then HTTP, then UI — with vertical slices where each phase leaves something runnable.

## Architecture Decisions

| Decision | Rationale |
|----------|-----------|
| **Separate `liga` + `liga-js` modules** | Spec requirement; keeps JVM tests fast; Scala.js only in frontend |
| **`liga` depends on `http`, not vice versa** | Reuse existing `http` utilities; serve lives in `liga/serve/` |
| **Pure logic in `object` modules, ZIO at edges** | Matches repo convention (`Plate`, `Hubad`); 100% unit-test coverage on math |
| **HOCON period files via zio-config-typesafe** | Already used in `md2c`, `plate`; no new parser dependency |
| **Append-only JSON events via zio-json** | Spec mandate; simpler than DB; crash recovery by replay |
| **glicko2 on JVM + Scala.js (`%%%`)** | Shared handicap preview in browser; server remains authoritative |
| **No auth in v1** | Director routes localhost-only; `--lan` exposes read-only audience |
| **Scala.js plugin added in Task 1** | Fail fast on build toolchain before investing in UI tasks |

## Dependency Graph

```
Task 1: SBT modules (liga + liga-js scaffold)
    │
    ├── Task 2: Domain model
    │       │
    │       ├── Task 3: Glicko2 wrapper
    │       │       │
    │       │       ├── Task 4: Period file I/O
    │       │       │       │
    │       │       │       ├── Task 5: Period discovery + leaderboard
    │       │       │       │       └── Task 6: `liga leaderboard` CLI
    │       │       │       │
    │       │       │       └── Task 22: Period file emission (after tournament)
    │       │       │
    │       │       └── Task 7: Handicap algorithm
    │       │               └── Task 8: `liga handicap` CLI
    │       │
    │       └── Task 9: Double-elim bracket engine
    │               │
    │               ├── Task 10: Event types + JSON codecs
    │               │       └── Task 11: Event log + replay
    │               │               └── Task 12: Tournament state machine
    │               │
    │               └── Task 13: HTTP server skeleton
    │                       ├── Task 14: Read API
    │                       ├── Task 15: Write API
    │                       ├── Task 16: Dual bind (`--lan`)
    │                       └── Task 17: `liga serve` CLI + resume
    │                               │
    │                               ├── Task 18: Scala.js build (can start after Task 1)
    │                               ├── Task 19: API fetch client
    │                               ├── Task 20: Director UI
    │                               └── Task 21: Audience UI
```

---

## Task List

### Phase 0: Foundation

---

## Task 1: SBT module scaffolding

**Description:** Add `liga` (JVM) and `liga-js` (Scala.js) projects to `build.sbt`, register dependencies (`glicko2`, `zio-json`, `better-files`, `zio-config-typesafe`, Laminar), wire `cli.dependsOn(liga)`, and confirm `sbt --client compile` succeeds for both modules.

**Acceptance criteria:**
- [x] `liga` module exists with package `ph.samson.atbp.liga`
- [x] `liga-js` module exists with `%%%` glicko2 dependency
- [x] `liga` aggregated in root; `cli` depends on `liga`
- [x] `sbt --client liga/test` runs (empty suite OK)
- [x] `sbt --client liga-js/fastLinkJS` compiles (empty main OK)

**Verification:**
- [x] `sbt --client compile` succeeds
- [x] `sbt --client liga/test` succeeds

**Dependencies:** None

**Files likely touched:**
- `build.sbt`
- `project/plugins.sbt` (add `sbt-scalajs`, `sbt-scalajs-crossproject` or direct ScalaJSPlugin)
- `project/Dependencies.scala`
- `liga/src/main/scala/.../package.scala` (placeholder)
- `liga-js/src/main/scala/.../package.scala` (placeholder)

**Estimated scope:** Medium (3–5 files)

---

## Task 2: Domain model types

**Description:** Define pure `case class` types for `Player`, `PlayerRating`, `MatchResult`, `Period`, `PeriodMatch`, `HandicapSuggestion`, `Bracket`, `BracketMatch`, `TournamentState`, and `TournamentEvent` payloads — no I/O, no ZIO.

**Acceptance criteria:**
- [x] `PlayerRating` holds rating, RD, career W–L
- [x] `PeriodMatch` captures scores, race-to, suggested/applied handicap
- [x] `BracketMatch` has lifecycle states: pending → ready → started → completed
- [x] Score expansion helper: `expandGames(scoreA, scoreB)` returns atomic game outcomes

**Verification:**
- [x] `sbt --client "liga/testOnly *model*"` — score expansion tests pass (`7-4` → 11 games)

**Dependencies:** Task 1

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/model/*.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/model/ScoreExpansionSpec.scala`

**Estimated scope:** Small (2–3 files)

---

## Task 3: Glicko2 wrapper + golden tests

**Description:** Wrap `com.github.mrdimosthenis` `glicko2` with repo tuning (1500 / 350 / 0.06 / τ 0.5). Implement `leaderboard(snapshot)` and per-game update folding over expanded match games.

**Acceptance criteria:**
- [ ] Default parameters match spec
- [ ] Golden-vector tests against published Glicko2 examples
- [ ] Property: RD decreases (or stays) after games played
- [ ] Property: ratings remain bounded for extreme inputs

**Verification:**
- [ ] `sbt --client "liga/testOnly *glicko*"`

**Dependencies:** Task 2

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/glicko/Glicko2.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/glicko/Tuning.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/glicko/Glicko2Spec.scala`
- `liga/src/test/resources/glicko/` (fixture vectors)

**Estimated scope:** Small (3–4 files)

---

### Checkpoint: Foundation
- [ ] `sbt --client liga/test` passes
- [ ] Domain + Glicko2 math verified before any I/O

---

### Phase 1: CLI — Ratings & Handicaps

---

## Task 4: Period file I/O (HOCON)

**Description:** Parse and write `*.liga` files as bare top-level HOCON (no wrapper key) using zio-config. Round-trip tests with fixture files.

**Acceptance criteria:**
- [ ] Parse all fields from spec (`name`, `completed`, `format`, `race-to`, `matches[]`)
- [ ] Write produces valid HOCON that re-parses identically
- [ ] Invalid HOCON surfaces clear errors
- [ ] `completed` date parsed as `LocalDate` (ordering key)

**Verification:**
- [ ] `sbt --client "liga/testOnly *io*"`
- [ ] Fixture round-trip in `liga/src/test/resources/periods/`

**Dependencies:** Task 2

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/io/PeriodCodec.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/io/PeriodWriter.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/io/PeriodIoSpec.scala`
- `liga/src/test/resources/periods/*.liga`

**Estimated scope:** Medium (4–5 files)

---

## Task 5: Period discovery + leaderboard computation

**Description:** Recursively discover `*.liga` under `--data`, exclude `tournament-*` dirs, sort by `completed` date ascending, error on duplicate dates, fold periods through Glicko2 to produce a reproducible leaderboard snapshot.

**Acceptance criteria:**
- [ ] Discovery ignores paths under `tournament-*`
- [ ] Ordering by `completed` only (filename/depth ignored)
- [ ] Duplicate `completed` dates → hard error with both file paths
- [ ] Same inputs → bit-for-bit identical output (deterministic sort of players)

**Verification:**
- [ ] `sbt --client "liga/testOnly *PeriodLoader*"` or equivalent
- [ ] Golden fixture: two period files → expected rating table

**Dependencies:** Tasks 3, 4

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/io/PeriodLoader.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/glicko/Leaderboard.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/io/DataLayout.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/io/PeriodLoaderSpec.scala`

**Estimated scope:** Medium (4 files)

---

## Task 6: `liga leaderboard` CLI command

**Description:** Wire `atbp liga leaderboard [--data <dir>]` as default subcommand. Render rating, RD, career W–L to stdout. Register in `Main.scala`.

**Acceptance criteria:**
- [ ] `--data` defaults to CWD
- [ ] Fixed-width table; sort by rating descending; rating as integer, RD to 1 decimal, W–L as `12-8`
- [ ] Command registered under `atbp liga` with `leaderboard` as default

**Verification:**
- [ ] `sbt --client "cli/runMain ph.samson.atbp.cli.Main liga leaderboard --data liga/src/test/resources/periods/"`
- [ ] CLI output formatting test

**Dependencies:** Task 5

**Files likely touched:**
- `cli/src/main/scala/ph/samson/atbp/cli/Liga.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/cli/LeaderboardRenderer.scala`
- `cli/src/main/scala/ph/samson/atbp/cli/Main.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/cli/LeaderboardRendererSpec.scala`

**Estimated scope:** Small (3–4 files)

---

## Task 7: Handicap suggestion algorithm

**Description:** Pure `Handicap.suggest(a, b, raceTo)` — weaker player by rating (tie → RD → alphabetical), binary search `h` in `[0, floor(0.75 × N)]` for ~50% win probability using per-game Glicko2 expected score with handicap offset.

**Acceptance criteria:**
- [ ] Cap never exceeds `floor(0.75 × N)`
- [ ] Equal ratings → handicap 0 (or minimal)
- [ ] Large rating gap → hits cap, doesn't overflow
- [ ] Same function usable from CLI and serve (pure, no ZIO)

**Verification:**
- [ ] `sbt --client "liga/testOnly *handicap*"` — edge cases, cap, symmetry

**Dependencies:** Task 3

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/handicap/Handicap.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/handicap/WinProbability.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/handicap/HandicapSpec.scala`

**Estimated scope:** Small (3 files)

---

## Task 8: `liga handicap` CLI command

**Description:** Wire `atbp liga handicap <player-a> <player-b> --race <n> [--data <dir>]`. Load period snapshot, look up players, print suggested spot for weaker player.

**Acceptance criteria:**
- [ ] Unknown player name → clear error
- [ ] Output shows weaker player, suggested handicap, race-to
- [ ] Uses frozen period-end ratings from discovered files

**Verification:**
- [ ] Manual: `atbp liga handicap Alice Bob --race 7 --data <fixtures>`
- [ ] CLI formatting test

**Dependencies:** Tasks 6, 7

**Files likely touched:**
- `cli/src/main/scala/ph/samson/atbp/cli/Liga.scala` (extend)
- `liga/src/main/scala/ph/samson/atbp/liga/cli/HandicapRenderer.scala`

**Estimated scope:** Small (1–2 files)

---

### Checkpoint: CLI Core
- [ ] `atbp liga leaderboard` and `atbp liga handicap` work against fixture data
- [ ] All `liga/test` green
- [ ] **Human review** before tournament work

---

### Phase 2: Bracket & Tournament Persistence

---

## Task 9: Double-elimination bracket engine

**Description:** Generate bracket for 8–64 players (round up to power of 2), seed by rating (1 vs N, 2 vs N−1), assign byes to lowest seeds, track winners/losers bracket advancement.

**Acceptance criteria:**
- [ ] Supports 8, 16, 32, 64 (and non-power-of-2 with byes)
- [ ] Seeding order deterministic from sorted ratings
- [ ] Bye matches auto-advance winner
- [ ] `advance(match, winner)` returns updated bracket + next matches ready

**Verification:**
- [ ] `sbt --client "liga/testOnly *bracket*"` — seeding, bye, advancement for each size

**Dependencies:** Task 2

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/bracket/BracketGen.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/bracket/Seeding.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/bracket/Advancement.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/bracket/BracketSpec.scala`

**Estimated scope:** Medium (4–5 files)

---

## Task 10: Tournament event types + JSON codecs

**Description:** Define all v1 event types (`TournamentCreated`, `RoundRaceToSet`, `BracketSeeded`, `MatchReady`, `HandicapApplied`, `MatchStarted`, `MatchResult`, `TournamentCompleted`) with zio-json codecs and `seq` field.

**Acceptance criteria:**
- [ ] Each event type matches spec payload shapes
- [ ] JSON round-trip for every event type
- [ ] `seq` is monotonically increasing

**Verification:**
- [ ] `sbt --client "liga/testOnly *EventCodec*"`

**Dependencies:** Tasks 2, 9

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/events/*.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/EventCodec.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/tournament/EventCodecSpec.scala`

**Estimated scope:** Medium (4–5 files)

---

## Task 11: Append-only event log + replay

**Description:** Write events as `000001-created.json`, etc. Replay by sorting on `seq`, fold into `TournamentState`. Detect incomplete tournaments (no `TournamentCompleted`).

**Acceptance criteria:**
- [ ] Events never mutated or deleted
- [ ] Replay from fixture dir reproduces expected state
- [ ] Crash mid-sequence: partial replay matches last consistent state
- [ ] `isComplete` = presence of `TournamentCompleted`

**Verification:**
- [ ] `sbt --client "liga/testOnly *replay*"`
- [ ] Fixtures in `liga/src/test/resources/tournaments/`

**Dependencies:** Task 10

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/EventLog.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/Replay.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/tournament/ReplaySpec.scala`
- `liga/src/test/resources/tournaments/*/`

**Estimated scope:** Medium (3–4 files)

---

## Task 12: Tournament state machine

**Description:** Orchestrate match lifecycle: ready → handicap applied → started (locked) → result → bracket advance. Enforce: no handicap edit after start, ratings frozen during tournament, per-round race-to defaults.

**Acceptance criteria:**
- [ ] `MatchReady` triggers handicap suggestion
- [ ] `HandicapApplied` can differ from suggested
- [ ] `MatchStarted` rejects further handicap changes
- [ ] `MatchResult` advances bracket and marks next matches ready
- [ ] Invalid transitions return typed errors

**Verification:**
- [ ] `sbt --client "liga/testOnly *TournamentState*"` — full lifecycle + illegal transitions

**Dependencies:** Tasks 7, 9, 11

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/Tournament.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/MatchLifecycle.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/tournament/TournamentSpec.scala`

**Estimated scope:** Medium (3–4 files)

---

### Checkpoint: Tournament Core
- [ ] Replay fixtures produce correct bracket states
- [ ] Match lifecycle rules enforced in tests
- [ ] No HTTP yet — pure JVM verification

---

### Phase 3: HTTP Serve

---

## Task 13: HTTP server skeleton

**Description:** ZIO HTTP app in `liga/serve/` serving static placeholder, configurable port (default 5442), bind `127.0.0.1`. Load HOCON config for poll interval etc.

**Acceptance criteria:**
- [ ] Server starts on `127.0.0.1:5442`
- [ ] Health or root route responds
- [ ] Graceful shutdown on interrupt

**Verification:**
- [ ] `sbt --client "liga/testOnly *Serve*"` — boot test with zio-http test client
- [ ] Manual: server starts without error

**Dependencies:** Task 1

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/serve/Server.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/serve/Config.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/serve/ServerSpec.scala`

**Estimated scope:** Small (3 files)

---

## Task 14: Read API (GET routes)

**Description:** Implement `GET /api/tournament` and `GET /api/leaderboard` returning JSON from replayed state + period snapshot.

**Acceptance criteria:**
- [ ] `/api/tournament` returns bracket, match states, handicaps
- [ ] `/api/leaderboard` returns period-start frozen ratings
- [ ] JSON schema stable for frontend consumption

**Verification:**
- [ ] Integration test with fixture tournament dir
- [ ] `curl localhost:5442/api/tournament` after seeding

**Dependencies:** Tasks 11, 12, 13

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/serve/Routes.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/serve/ApiJson.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/serve/ReadApiSpec.scala`

**Estimated scope:** Medium (3–4 files)

---

## Task 15: Write API (POST routes)

**Description:** Director-only POST routes: seed, ready, handicap, start, result. Each appends an event to the log and returns updated state.

**Acceptance criteria:**
- [ ] `POST /api/tournament/seed` — creates + seeds bracket with frozen ratings
- [ ] `POST /api/matches/:id/ready|handicap|start|result` — append events, enforce lifecycle
- [ ] Write routes return 403 or refuse connection off localhost

**Verification:**
- [ ] Integration test: seed → ready → handicap → start → result flow
- [ ] Test that write routes reject non-localhost (or aren't bound on LAN)

**Dependencies:** Task 14

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/serve/DirectorRoutes.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/serve/WriteApiSpec.scala`

**Estimated scope:** Medium (2–3 files)

---

## Task 16: Dual network bind (`--lan`)

**Description:** Default: all routes on `127.0.0.1`. With `--lan`: audience routes (`/audience`, `/api/tournament`, `/api/leaderboard`) also on `0.0.0.0`; director routes stay localhost-only.

**Acceptance criteria:**
- [ ] `--lan` exposes read API + `/audience` on LAN
- [ ] `POST` routes never reachable off localhost
- [ ] `/` director SPA localhost-only even with `--lan`

**Verification:**
- [ ] Test: read routes accessible on secondary interface; write routes not
- [ ] Manual: phone/TV on LAN can hit `/api/tournament`

**Dependencies:** Task 15

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/serve/Server.scala` (extend)
- `liga/src/main/scala/ph/samson/atbp/liga/serve/BindConfig.scala`

**Estimated scope:** Small (2 files)

---

## Task 17: `liga serve` CLI + resume logic

**Description:** Wire `atbp liga serve [--data] [--port] [--lan] [--new]`. On startup: scan for incomplete `tournament-*` dirs — resume if exactly one, fail if >1, allow `--new` if zero.

**Acceptance criteria:**
- [ ] Auto-resume sole incomplete tournament
- [ ] Multiple incomplete → error listing dirs
- [ ] `--new` creates `tournament-<YYYYMMDD>-<slug>/` (e.g. `tournament-20260315-spring-open/`) from director-provided name + creation timestamp
- [ ] `--data` defaults to CWD

**Verification:**
- [ ] Test resume from fixture tournament dir
- [ ] Manual: kill JVM mid-tournament, restart, state intact

**Dependencies:** Tasks 11, 16

**Files likely touched:**
- `cli/src/main/scala/ph/samson/atbp/cli/Liga.scala` (serve subcommand)
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/Resume.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/tournament/ResumeSpec.scala`

**Estimated scope:** Small (3 files)

---

### Checkpoint: Serve Backend
- [ ] Full tournament runnable via `curl`/API alone
- [ ] Crash recovery verified
- [ ] **Human review** before frontend investment

---

### Phase 4: Frontend (Scala.js + Laminar)

---

## Task 18: Scala.js + Laminar build pipeline

**Description:** Configure `liga-js` with Laminar, produce two entry points (`MainDirector`, `MainAudience`), embed compile output into `liga` resources for static serving.

**Acceptance criteria:**
- [ ] `sbt --client ligaJs/fastLinkJS` produces JS artifacts
- [ ] `liga` serve module serves compiled assets from classpath
- [ ] Two separate HTML/JS entry bundles

**Verification:**
- [ ] `sbt --client compile` succeeds
- [ ] Browser loads placeholder pages at `/` and `/audience`

**Dependencies:** Task 1 (can parallelize with Tasks 2–17)

**Files likely touched:**
- `build.sbt` (Scala.js settings, `scalaJSLinkerConfig`)
- `liga-js/src/main/scala/.../MainDirector.scala`
- `liga-js/src/main/scala/.../MainAudience.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/serve/StaticAssets.scala`

**Estimated scope:** Medium (4–5 files)

---

## Task 19: API fetch client (liga-js)

**Description:** Scala.js HTTP client wrapping `GET /api/tournament`, `GET /api/leaderboard`, and director `POST` endpoints. Shared JSON types (or mirror) for API responses.

**Acceptance criteria:**
- [ ] Fetch client returns typed responses
- [ ] Error handling for network failures
- [ ] Works against running serve instance

**Verification:**
- [ ] Manual: client fetches tournament state in browser console
- [ ] (Automated JS tests deferred per spec)

**Dependencies:** Tasks 14, 18

**Files likely touched:**
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/api/Client.scala`
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/api/Models.scala`

**Estimated scope:** Small (2–3 files)

---

## Task 20: Director UI

**Description:** Laminar SPA at `/` — tournament setup (player list, seed), per-round race-to, bracket tree view, match panel (handicap suggest/override, start, score entry). Instant handicap preview via client-side glicko2.

**Acceptance criteria:**
- [ ] Director can seed bracket from UI
- [ ] Handicap suggestion shown; override editable until start
- [ ] Start locks handicap; result entry advances bracket
- [ ] Client-side preview matches server suggestion (approximately)
- [ ] 8–64 player bracket navigable without getting lost

**Verification:**
- [ ] Manual: run 8-player tournament end-to-end through UI
- [ ] Override flow < few seconds at table

**Dependencies:** Tasks 15, 19

**Files likely touched:**
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/director/*.scala`
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/glicko/HandicapPreview.scala`

**Estimated scope:** Large — **split if needed into 20a (bracket view) + 20b (match panel)**

---

## Task 21: Audience UI

**Description:** Read-only Laminar SPA at `/audience`. Polls `/api/tournament` every 5 seconds. Shows bracket progress, match results, no director controls.

**Acceptance criteria:**
- [ ] Poll interval 5s (configurable via served HOCON)
- [ ] No write controls visible
- [ ] Updates when director records results
- [ ] Works on LAN with `--lan`

**Verification:**
- [ ] Manual: director + audience in two browser windows
- [ ] Manual: audience on second screen / LAN device

**Dependencies:** Tasks 16, 19

**Files likely touched:**
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/audience/*.scala`
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/MainAudience.scala`

**Estimated scope:** Medium (3–4 files)

---

### Phase 5: Integration & Completion

---

## Task 22: Period file emission on tournament complete

**Description:** On `TournamentCompleted`, write a new `*.liga` file to `--data` root with all match results, handicaps, and metadata. Never mutate existing period files.

**Acceptance criteria:**
- [ ] Emitted file validates against period I/O parser
- [ ] `completed` date set appropriately
- [ ] Re-running `liga leaderboard` includes new period
- [ ] File written to data root as `<completed-date>-<slug>.liga` (e.g. `2026-03-15-spring-open.liga`), not inside `tournament-*`

**Verification:**
- [ ] Integration test: complete tournament → parse emitted file → correct ratings
- [ ] Bit-for-bit reproducibility with CLI leaderboard

**Dependencies:** Tasks 4, 12, 17

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/PeriodEmission.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/tournament/PeriodEmissionSpec.scala`

**Estimated scope:** Small (2–3 files)

---

## Task 23: End-to-end verification & dogfooding prep

**Description:** Run full flow with realistic fixture data. Document manual test checklist. Verify all success criteria from spec.

**Acceptance criteria:**
- [ ] All spec success criteria checked off
- [ ] `sbt --client liga/test` green
- [ ] `sbt --client fixup` clean
- [ ] 16-player tournament runnable in one session

**Verification:**
- [ ] Manual E2E checklist (director + audience + CLI leaderboard after)
- [ ] `sbt --client cli/docker:publishLocal` succeeds (if Docker available)

**Dependencies:** All prior tasks

**Files likely touched:**
- `liga/src/test/resources/periods/` (richer fixtures)
- `liga/src/test/resources/tournaments/` (full event sequence)

**Estimated scope:** Small (fixtures only)

---

### Checkpoint: Complete
- [ ] All acceptance criteria met
- [ ] Ready for implementation

---

## Parallelization Opportunities

| Parallel track A | Parallel track B | Coordination |
|------------------|------------------|--------------|
| Tasks 2–8 (JVM core + CLI) | Task 18 (Scala.js scaffold) | API JSON shapes frozen at Task 14 before Task 19 |
| Tasks 9–12 (bracket + replay) | Task 18 (Laminar hello-world) | Event types (Task 10) before frontend models |
| Task 20 (director UI) | Task 21 (audience UI) | After Task 19; independent pages |

**Must be sequential:** Task 1 → everything; Task 10 → 11 → 12 → 14 → 15; Task 22 last.

---

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Scala.js/Laminar toolchain new to repo | High | Task 1 + 18 early; spike `fastLinkJS` before UI tasks |
| Handicap win-probability model wrong | High | Golden tests + dogfooding; compare client preview vs server |
| Bracket advancement bugs at scale | Medium | Parametric tests for 8/16/32/64; fixture replay tests |
| Dual-bind routing complexity | Medium | Integration tests per bind mode; explicit route tables |
| glicko2 lib API differs JVM vs JS | Medium | Shared test vectors in both modules where practical |
| Director UX overwhelming for 64 players | Medium | Collapsible rounds, highlight active matches (Task 20) |
| Period date collisions in real use | Low | Clear error message; document one-period-per-date convention |

---

## Resolved Open Questions (from plan review)

| # | Question | Decision |
|---|----------|----------|
| 1 | Leaderboard CLI output format | Fixed-width table; **sort by rating desc**; rating as **integer**, RD **1 decimal**, W–L as `12-8` |
| 2 | Tournament directory ID | **`tournament-<YYYYMMDD>-<slug>/`** — slug from director-provided name (e.g. `tournament-20260315-spring-open/`) |
| 3 | Emitted period filename | **`<completed-date>-<slug>.liga`** at `--data` root (e.g. `2026-03-15-spring-open.liga`); slug matches tournament |
| 4 | CI for Scala.js | **Defer** `ligaJs` CI until after manual E2E verification (Task 23) |
| 5 | Docker entrypoint | Keep **generic `atbp` entrypoint**; run `atbp liga serve` explicitly |

**Slug rule:** director-provided tournament name → lowercase, trim, replace non-alphanumeric runs with `-`, collapse repeated `-`, strip leading/trailing `-`. Same slug used in tournament dir and emitted period filename.

---

## Suggested Session Groupings

For incremental agent/human sessions (~1–2 hours each):

| Session | Tasks | Deliverable |
|---------|-------|-------------|
| 1 | 1–3 | Modules compile; Glicko2 tested |
| 2 | 4–6 | `atbp liga leaderboard` works |
| 3 | 7–8 | `atbp liga handicap` works |
| 4 | 9–12 | Tournament replay from fixtures |
| 5 | 13–17 | `atbp liga serve` API-complete |
| 6 | 18–19 | Frontend builds; API client works |
| 7 | 20 | Director can run a tournament |
| 8 | 21–23 | Audience + period emission + E2E |
