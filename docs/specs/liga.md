# Spec: Liga

> **Source:** [docs/ideas/liga.md](../ideas/liga.md) · [docs/intent/liga.md](../intent/liga.md)  
> **Status:** Specify — plan approved; ready for implementation

## Objective

Replace manual billiards standings and ad-hoc handicaps with reproducible Glicko2 ratings and a director-run handicapped double-elimination tournament — on one laptop, with a separate URL for the club TV.

### Users

| User | Surface | Goal |
|------|---------|------|
| Club organizer / director | `atbp liga` CLI + director web UI | Maintain ratings, look up handicaps, run tournaments |
| Club members / spectators | Audience web UI (`/audience`) | Read-only bracket progress and results on external display |

### User stories

1. As a director, I run `atbp liga leaderboard` (or `atbp liga leaderboard --data ./club/`) and get a reproducible rating table (rating, RD, career W–L) from period files discovered under `--data`.
2. As a director, I run `atbp liga handicap "Alice" "Bob" --race 7` and get a suggested spot for Bob (weaker player) before a casual match.
3. As a director, I run `atbp liga serve`, seed a double-elim bracket from frozen period-start ratings, set per-round race-to-N, accept or override handicap suggestions per match, enter results, and resume after a crash.
4. As a spectator, I open `/audience` on the club TV and see live bracket progress without director controls.

### Success criteria

- [ ] Leaderboard output is **bit-for-bit reproducible** from the same ordered period files on any machine.
- [ ] Glicko2 updates run **only when a period file is finalized** (tournament complete); ratings are frozen for the duration of an in-progress tournament.
- [ ] Handicap suggestion targets ~50% win probability for the weaker player, capped at **75% of race-to-N**.
- [ ] Director can override a suggested handicap in seconds; override is **locked once play starts**.
- [ ] Tournament state survives JVM restart via append-only JSON replay.
- [ ] Completed tournament emits a period file in CLI input format; **existing period files are never mutated**.
- [ ] Audience view refreshes via polling (no WebSockets); director and audience are **separate URLs**.
- [ ] An 8–64 player double-elim event is runnable without getting lost in the bracket tree.

---

## Tech Stack

| Layer | Choice | Notes |
|-------|--------|-------|
| Language | Scala 3.8.4 | Matches repo |
| CLI | zio-cli 0.8.1 | Existing `atbp` pattern |
| Core effects | ZIO 2.1.26 | Existing |
| HTTP | zio-http 3.11.2 | Existing in `http` module |
| Config | zio-config + Typesafe HOCON | Existing |
| Files | better-files 3.9.2 | Existing |
| JSON (events/API) | zio-json 0.9.2 | Tournament persistence + REST |
| Glicko2 | `com.github.mrdimosthenis` %% `glicko2` 1.0.1 | JVM + Scala.js; default tuning (1500 / 350 / 0.06 / τ 0.5) |
| Frontend | Scala.js + Laminar | **New** — separate `liga-js` module |
| Tests | zio-test | Existing pattern (`*Spec.scala`) |

---

## Commands

```bash
# Format changed Scala (before commit)
sbt --client fixup

# Build all modules including new liga + liga-js
sbt --client compile

# Run unit tests
sbt --client liga/test

# CLI — leaderboard (default subcommand; --data defaults to CWD)
sbt --client "cli/runMain ph.samson.atbp.cli.Main liga leaderboard --data ./club/"

# CLI — handicap lookup
sbt --client "cli/runMain ph.samson.atbp.cli.Main liga handicap Alice Bob --race 7 --data ./club/"

# CLI — tournament server (director laptop)
sbt run   # then: atbp liga serve --data ./club/

# Package
sbt --client cli/docker:publishLocal

# Fast compile loop for frontend only
sbt --client ligaJs/fastLinkJS
```

Installed binary equivalents:

```bash
atbp liga leaderboard [--data <dir>]         # default: CWD; discovers period files recursively
atbp liga handicap <player-a> <player-b> --race <n> [--data <dir>]
atbp liga serve [--data <dir>] [--port 5442] [--lan] [--new]
```

**`--data` layout** (single root directory):

```
./club/                          # --data ./club/
  2025/spring-open.liga          # completed period files (any depth, any name)
  2026/fall-league.liga
  tournament-20260315-spring-open/   # in-progress tournament (prefix tournament-)
    000001-created.json
    000002-seeded.json
```

- Period files: HOCON content in **`*.liga`** files (bare top-level fields, no wrapper key), discovered **recursively** under `--data`, **excluding** `tournament-*` directories.
- Tournaments: subdirectories named `tournament-<YYYYMMDD>-<slug>/` (e.g. `tournament-20260315-spring-open/`) containing append-only JSON event files. `<slug>` is the director-provided tournament name lowercased with non-alphanumeric runs replaced by `-`.
- **Emitted period files:** on tournament complete, write `<completed-date>-<slug>.liga` to the `--data` root (e.g. `2026-03-15-spring-open.liga`).
- **Resume rule:** on `serve` startup, if exactly one incomplete `tournament-*` dir exists, resume it automatically. If zero, allow `--new`. If more than one, fail with a list (director must complete or remove extras).

---

## Project Structure

```
liga/                              → JVM core: Glicko2, period I/O, handicap, bracket, serve
  src/main/scala/ph/samson/atbp/liga/
    glicko/                        → dimos.glicko2 wrapper + tuning
    model/                         → Player, Match, Period, Bracket, TournamentEvent
    io/                            → Period file parser/writer (`*.liga` / HOCON)
    handicap/                      → Suggestion algorithm
    bracket/                       → Double-elim generation, seeding, advancement
    tournament/                    → Append-only event log + replay
    serve/                         → ZIO HTTP routes, static asset serving, JSON API
  src/test/scala/ph/samson/atbp/liga/
    glicko/                        → Golden-vector tests against glicko2 lib + integration
    handicap/                      → Edge cases (large rating gaps, cap at 75%)
    bracket/                       → Seeding, bye handling, advancement rules
    tournament/                    → Crash-recovery replay tests
    io/                            → Period file round-trip tests

liga-js/                           → Scala.js Laminar SPA (director + audience)
  src/main/scala/ph/samson/atbp/liga/js/
    glicko/                        → dimos.glicko2 (Scala.js) for client-side handicap preview
    api/                           → Fetch client for serve JSON API
    director/                      → Director UI components
    audience/                      → Read-only bracket view
    MainDirector.scala             → Entry: /
    MainAudience.scala             → Entry: /audience

cli/src/main/scala/ph/samson/atbp/cli/
  Liga.scala                       → zio-cli command wiring

docs/
  ideas/liga.md                    → Original problem / direction (input)
  intent/liga.md                   → Interview outcome (input)
  specs/liga.md                    → This spec (source of truth for implementation)

liga/src/test/resources/
  periods/                         # Fixture `*.liga` period files for tests
  tournaments/                     → Fixture event logs for replay tests
```

---

## Domain Model

### Period file (`*.liga`, HOCON content)

One file = one Glicko2 rating period (typically one completed tournament). File extension **`.liga`**; body is bare HOCON (top-level fields, no wrapper key).

```hocon
name = "Spring 2026 Open"
completed = "2026-03-15"         # ISO-8601 date (YYYY-MM-DD); ordering key

# Optional presentation metadata — does NOT affect rating math
format = "8-ball"
race-to = 7

matches = [
  {
    player-a = "Alice"
    player-b = "Bob"
    score-a = 7
    score-b = 4
    race-to = 7                    # effective N for this match
    handicap-suggested = 2         # games spotted to weaker player
    handicap-applied = 2           # agreed spot (may differ from suggested)
  }
]
```

**Score → Glicko2 games:** a match with `score-a = 7, score-b = 4` expands to 11 atomic game outcomes (7 wins for A, 4 for B). Game order within the match is irrelevant.

**Processing order:** period files are sorted by the **`completed` date** field (ascending, ISO-8601 `YYYY-MM-DD`). Filename and directory depth are ignored for ordering. Ties on `completed` date are a hard error — each period must have a unique completion date.

**Discovery:** scan `--data` recursively for **`*.liga`** files; skip any path under a `tournament-*` directory.

### Rating lifecycle

```
Prior periods (files) ──► frozen snapshot at tournament start
                              │
                              ▼
                    In-progress tournament
                    (ratings DO NOT update)
                              │
                              ▼
                    Tournament completes
                              │
                              ▼
                    New period file emitted
                    (Glicko2 runs on all matches)
```

### Handicap suggestion

Given players A and B with period-start ratings `(rA, rdA)` and `(rB, rdB)`, effective race-to-N:

1. Determine weaker player (lower rating; tie → lower RD; further tie → alphabetical).
2. Binary-search handicap `h` in `[0, floor(0.75 × N)]` such that `P(weaker wins race-to-N | h) ≈ 0.5`.
3. Win probability uses Glicko2 expected score per game, adjusted for handicap (weaker player starts at `h` games).

CLI `liga handicap` and director UI use the **same function**. Server response is authoritative at match lock; `liga-js` may use `glicko2` locally for instant preview while the director adjusts.

### Bracket constraints (v1)

- **8–64 players**, bracket size rounded up to next power of two; byes go to lowest seeds.
- Seeding by period-start rating (1 vs N, 2 vs N−1, …).
- Double-elimination only.

### Player identity

Display-name strings; **case-sensitive** match. Typos and renames via find-and-replace on period files — no ID layer in v1.

### Tournament event log (append-only JSON)

Directory: `<data>/tournament-<YYYYMMDD>-<slug>/` (e.g. `./club/tournament-20260315-spring-open/`)

Each state change appends one file: `000001-created.json`, `000002-seeded.json`, …

```json
{
  "seq": 1,
  "type": "TournamentCreated",
  "at": "2026-03-15T18:00:00Z",
  "payload": { "name": "Spring 2026 Open", "playerCount": 16 }
}
```

Event types (v1):

| Event | Purpose |
|-------|---------|
| `TournamentCreated` | Name, player list |
| `RoundRaceToSet` | Per bracket round default race-to-N |
| `BracketSeeded` | Frozen ratings snapshot + initial bracket |
| `MatchReady` | Handicap suggested; director may still edit |
| `HandicapApplied` | Final agreed handicap before play |
| `MatchStarted` | Locks handicap |
| `MatchResult` | Final scores; advances bracket |
| `TournamentCompleted` | Writes finalized period file to **`--data` root** (not inside `tournament-*` dir) |

Replay: sort files by `seq`, fold into current `TournamentState`. Never mutate or delete event files. A tournament is **incomplete** until a `TournamentCompleted` event exists in its replay.

### Serve networking

| Setting | Default | Notes |
|---------|---------|-------|
| Bind address | `127.0.0.1` | Director UI + write API localhost-only |
| Port | `5442` | Configurable via `--port` or HOCON |
| `--lan` | off | When set, `/audience` + read-only API bind `0.0.0.0` for club TV on LAN |
| Director over LAN | **No** | Write routes and `/` remain localhost-only even with `--lan` |

Audience poll interval: **5 seconds** (configurable in HOCON).

### HTTP API (serve mode)

| Method | Path | Audience | Purpose |
|--------|------|----------|---------|
| GET | `/api/tournament` | ✓ | Current bracket + match states |
| GET | `/api/leaderboard` | ✓ | Period-start snapshot ratings |
| POST | `/api/tournament/seed` | ✗ | Director: seed bracket |
| POST | `/api/matches/:id/ready` | ✗ | Compute handicap suggestion |
| POST | `/api/matches/:id/handicap` | ✗ | Set applied handicap |
| POST | `/api/matches/:id/start` | ✗ | Lock handicap |
| POST | `/api/matches/:id/result` | ✗ | Record score |
| GET | `/` | ✗ | Director SPA |
| GET | `/audience` | ✓ | Audience SPA |

Static assets served from `liga-js` compile output.

---

## Code Style

Follow existing repo conventions:

- Package: `ph.samson.atbp.liga.*`
- Scala 3 with `-no-indent -old-syntax` (brace style)
- `object Foo` for pure modules; `case class` for data
- Effects at edges (`ZIO`), pure logic in the center
- `ZIOSpecDefault` + `assertTrue` for tests

```scala
// Example: pure handicap at the core, ZIO at the CLI edge
object Handicap {
  def suggest(
      a: PlayerRating,
      b: PlayerRating,
      raceTo: Int
  ): HandicapSuggestion = {
    // pure — no ZIO, no files
  }
}

case class LigaLeaderboard(periods: File) extends ToolCommand {
  override def run(conf: Conf): ZIO[Any, Throwable, Unit] =
    ZIO.logSpan("Liga.leaderboard") {
      for {
        snapshot <- PeriodLoader.loadAll(periods)
        board    <- ZIO.succeed(Glicko2.leaderboard(snapshot))
        _        <- ZIO.logInfo(LeaderboardRenderer.render(board))
      } yield ()
    }
}
```

---

## Testing Strategy

| Concern | Level | Location | Approach |
|---------|-------|----------|----------|
| Glicko2 math | Unit | `liga/.../glicko/*Spec.scala` | Golden vectors from published examples + property tests (ratings bounded, RD decreases with play) |
| Score expansion | Unit | `liga/.../model/*Spec.scala` | `7-4` → 11 games |
| Handicap cap | Unit | `liga/.../handicap/*Spec.scala` | Extreme rating gaps never exceed `0.75 × N` |
| Period I/O | Unit | `liga/.../io/*Spec.scala` | Parse fixture HOCON → domain → write → re-parse |
| Bracket logic | Unit | `liga/.../bracket/*Spec.scala` | 8/16/32/64 seeding, bye, winners/losers advancement |
| Event replay | Unit | `liga/.../tournament/*Spec.scala` | Fixture event dirs → expected state; crash mid-sequence |
| CLI | Unit | `liga/.../cli/*Spec.scala` | Leaderboard + handicap output formatting |
| HTTP API | Integration | `liga/.../serve/*Spec.scala` | zio-http test client against routes |
| Frontend | Manual (v1) | — | Director flow + audience on second window; automated JS tests deferred |

**Coverage expectation:** 100% of Glicko2, handicap, bracket, and replay logic via unit tests. Serve and UI verified manually for v1.

---

## Boundaries

### Always do

- Run `sbt --client liga/test` before commits touching liga code
- Run `sbt --client fixup` on changed Scala files before commit
- Keep Glicko2 / handicap / bracket logic pure (testable without ZIO)
- Append-only tournament events — never update event files in place
- Never mutate completed period files

### Ask first

- Adding dependencies beyond `glicko2`, zio-json, and Scala.js/Laminar toolchain
- Changing default Glicko2 parameters (affects all historical recomputation)
- Changing period file format once fixtures exist
- CI changes for Scala.js build
- Docker image changes for serve deployment

### Never do

- Commit secrets or local tournament data
- In-period rating updates (mid-tournament drift)
- Authentication / multi-user editing in v1
- WebSockets for audience view
- Auto-apply handicaps without director confirmation
- Allow handicap edits after match start
- SQLite or other DB for tournament state

---

## Out of Scope (v1)

Per [docs/ideas/liga.md](../ideas/liga.md): single-elim, round-robin, auth, multi-tournament, cloud deploy, mobile, in-app period file editing, format-aware rating logic, per-rack game sequence, stable player IDs, in-place state mutation.

---

## Resolved Open Questions

| # | Question | Decision |
|---|----------|----------|
| 1 | Period file format | **HOCON in `*.liga` files**; bare top-level fields (no `liga.period` wrapper) |
| 2 | Glicko2 implementation | **`com.github.mrdimosthenis` `glicko2` 1.0.1** — JVM (`%%`) + Scala.js (`%%%`); handicap algorithm shared where practical |
| 3 | Data layout | Single **`--data` flag** (default CWD). Period files anywhere recursively under `--data`. In-progress tournaments in **`tournament-<id>/`** subdirs. Auto-resume if exactly one incomplete tournament |
| 4 | Serve bind | **`localhost:5442`** default. **`--lan`** exposes audience + read API on `0.0.0.0`; director stays localhost-only |
| 5 | Audience poll interval | **5 seconds** |
| 6 | Bracket size (v1) | **8–64** players (power of two; byes as needed) |
| 7 | Period ordering | By **`completed` date** (`YYYY-MM-DD`) ascending; duplicate dates = error |
| 8 | SBT modules | **Separate `liga` + `liga-js`** modules |
| 9 | Emitted period file location | **`--data` root** on tournament complete |
| 10 | Leaderboard CLI output | Fixed-width table; sort by **rating desc**; rating **integer**, RD **1 decimal**, W–L `12-8` |
| 11 | Tournament directory ID | **`tournament-<YYYYMMDD>-<slug>/`** from director name + creation date (e.g. `tournament-20260315-spring-open/`) |
| 12 | Emitted period filename | **`<completed-date>-<slug>.liga`** at data root (e.g. `2026-03-15-spring-open.liga`) |
| 13 | CI for Scala.js | Defer `ligaJs` to CI until after manual E2E (Task 23) |
| 14 | Docker entrypoint | Generic `atbp` image; explicit `atbp liga serve` |

### Assumptions to validate (from idea doc)

These are **operational hypotheses** to test during implementation / dogfooding, not blockers:

- [ ] Handicap suggestions are usually close enough — directors override rarely
- [ ] 75% cap handles extreme rating gaps without absurd spots
- [ ] Override UX is fast (seconds at the table)
- [ ] External display workflow (`/audience` on second screen) is frictionless
- [ ] 8–64 bracket UX keeps the director oriented
- [ ] Display-name strings suffice with find-and-replace corrections

---

## Resolved Decisions

Carried forward from [docs/ideas/liga.md](../ideas/liga.md):

| Topic | Decision |
|-------|----------|
| MVP scope | Full stack: CLI + director + audience |
| Period granularity | One tournament = one rating period |
| Result storage | Final score only (e.g. `7–4`) |
| Rating updates | Period boundaries only |
| Handicaps | Suggested at setup; director/players accept or override |
| Audience view | Separate URL; poll-based |
| Format metadata | Presentation only |
| Player identity | Display-name strings |
| Serve runtime | Single JVM on director laptop |
| Race-to-N | Per bracket round + per-match override |
| Tournament persistence | Append-only JSON in `tournament-<YYYYMMDD>-<slug>/` under `--data` |
| Data root | Single `--data` directory (default CWD) |
| Period discovery | Recursive under `--data`; exclude `tournament-*` dirs |
| Period ordering | `completed` date (`YYYY-MM-DD`) ascending |
| Period file extension | `.liga` |
| Emitted period file | Written to `--data` root on tournament complete |
| Glicko2 library | `mrdimosthenis/glicko2` (JVM + Scala.js) |
| Serve port | 5442 on localhost; `--lan` for audience on LAN |
| Bracket size | 8–64 players |
| Resume | Auto-resume sole incomplete `tournament-*` dir |
| Leaderboard output | Fixed-width table; rating desc; integer rating, 1-decimal RD, `12-8` W–L |
| Tournament dir ID | `tournament-<YYYYMMDD>-<slug>/` |
| Emitted period filename | `<completed-date>-<slug>.liga` at data root |
| Scala.js CI | Deferred until post-E2E |
| Docker | Generic `atbp` entrypoint; `atbp liga serve` explicit |

---

## Next Steps

1. **Implement** — follow [docs/plans/liga.md](../plans/liga.md) task list starting at Task 1
