# Spec: Liga thermo-review remediation

> **Source:** Thermo-nuclear branch audit + code quality audit of `liga` branch vs `main` (2026-07-12)  
> **Prior work:** First remediation pass (validation, localhost guard, static paths, generic 500s) is merged on branch  
> **Status:** Specify — awaiting human approval before implementation

## Assumptions

1. Scope is **remediation and hardening** from thermo findings — no unrelated new features.
2. The v1 security model is unchanged: director writes are localhost-only; `--lan` is read-only for remote clients.
3. Billiards rules unchanged: winner score equals race-to exactly; loser score is strictly less than race-to; handicap cap is `floor(0.75 × race-to)`.
4. A venue may run **multiple tournaments per serve session** (same day, after complete) without restarting `liga serve`.
5. Event log is the source of truth; replay must enforce the same invariants as command handlers.
6. `SPEC.md` is the working spec for this pass; it does not replace `docs/specs/liga.md`.
7. `liga-common` crossProject is **in scope** as Phase 2 (thermo P0 maintainability debt).
8. API type codegen / full consolidation is **deferred** unless time permits in Phase 3.
9. Performance work (in-memory projection cache, decoupling `liga` from `ligaJs`) is **out of scope** except where noted as optional.

---

## Objective

Close the gaps identified by thermo review so that Liga is **venue-correct** after tournament completion and **integrity-safe** under event-sourced replay — without compounding handicap/client drift.

### Users

| User | Impact |
|------|--------|
| Tournament director | Can start a second tournament same session; leaderboard reflects emitted period ratings after complete; clearer API errors |
| Club organizer | Period files and completion events stay consistent; corrupt event logs cannot silently produce invalid state |
| Spectator | Audience leaderboard shows updated ratings after tournament complete |

### User stories

1. As a director who resumed an incomplete tournament via CLI, when I complete it and start another tournament, all wizard and match writes target the **new** tournament — not the completed one.
2. As a director, after I complete a tournament, `/api/leaderboard` shows ratings from period files (including the just-emitted period), not frozen period-start snapshots.
3. As a director viewing the completed state, I can start a **new tournament** from the UI without restarting serve.
4. As an operator, if tournament completion fails mid-flight, the system is never left with a period file but no `TournamentCompleted` event (or vice versa).
5. As a maintainer, replay rejects the same invalid wizard/match events that command handlers reject (scores, handicaps, duplicate players, race-to bounds).
6. As a director, the handicap preview in the browser matches server handicap math (single shared implementation).

---

## Tech Stack

Unchanged unless Phase 2 adds `liga-common`:

| Layer | Choice | Notes |
|-------|--------|-------|
| Language | Scala 3.8.4 | |
| Build | sbt + Scala.js crossProject | **New:** `liga-common` JVM+JS |
| Effects | ZIO 2.1.26 | |
| HTTP | zio-http 3.11.2 | |
| JSON | zio-json 0.9.2 | |
| Frontend | Scala.js + Laminar | Deletes duplicated handicap copies |
| Tests | zio-test | JVM + minimal `liga-js` tests |

---

## Commands

```bash
# Format + compile + test (required before every commit)
sbt --client fixup

# Full module test suite
sbt --client "liga/test"
sbt --client "liga-js/test"

# Targeted verification
sbt --client "liga/testOnly *ServeContext*"
sbt --client "liga/testOnly *WriteApiSpec"
sbt --client "liga/testOnly *ReadApiSpec"
sbt --client "liga/testOnly *ReplaySpec"
sbt --client "liga/testOnly *TournamentSpec"
sbt --client "liga/testOnly *SeedSpec"
sbt --client "liga/testOnly *EndToEndSpec"
sbt --client "liga-js/testOnly *Handicap*"

# After build.sbt / project/* edits only
sbt --batch compile
sbt --batch fixup

# Verify clean tree after fixup loop
sbt --client fixup && git status
```

---

## Project Structure

```
liga-common/                          # NEW (Phase 2) — shared JVM+JS
  src/main/scala/ph/samson/atbp/liga/
    handicap/Handicap.scala
    handicap/WinProbability.scala
    glicko/Tuning.scala
    model/Types.scala                   # types needed by both tiers (subset)

liga/src/main/scala/ph/samson/atbp/liga/
  serve/
    ServeContext.scala                  # dir resolution, leaderboard, complete atomicity
    DirectorRoutes.scala                # InvalidSeq → 409
  tournament/
    Replay.scala                        # unified validation
    Seed.scala                          # raceToComplete guard, race-to bounds
    Tournament.scala                    # race-to bounds on setRoundRaceTo
    TournamentValidation.scala          # NEW — shared command+replay validators

liga-js/src/main/scala/ph/samson/atbp/liga/js/
  director/DirectorApp.scala            # "New tournament" UX
  glicko/                               # DELETE duplicated HandicapPreview, WinProbability, Tuning

liga-js/src/test/scala/                 # NEW — parity tests for shared math

liga/src/test/scala/ph/samson/atbp/liga/
  serve/ServeCheckpointSpec.scala
  serve/WriteApiSpec.scala
  serve/ReadApiSpec.scala
  tournament/ReplaySpec.scala
  EndToEndSpec.scala
```

---

## Code Style

Follow existing Liga patterns. Extract shared validation into pure `Either` functions callable from both `Tournament`/`Seed` (commands) and `Replay` (fold).

Example — replay delegates to shared validator:

```scala
// TournamentValidation.scala
object TournamentValidation {
  def validateMatchResult(
      state: TournamentState,
      matchDef: BracketMatch,
      scoreA: Int,
      scoreB: Int
  ): Either[String, Unit] =
    Tournament.validateScores(state, matchDef, scoreA, scoreB).left.map(_.message)
}

// Replay.scala
case TournamentEvent.MatchResult(_, _, payload) =>
  for {
  _ <- TournamentValidation.validateMatchResult(state, matchDef, payload.scoreA, payload.scoreB)
  winner <- winnerFromScores(matchDef, payload.scoreA, payload.scoreB)
  ...
```

`ServeContext` dir resolution — illustrative:

```scala
private def activeDirOption: Task[Option[File]] =
  tournamentDir match {
    case Some(dir) =>
      Replay.replayDir(dir).flatMap { state =>
        if (state.completed) Resume.resolve(dataDir)
        else ZIO.succeed(Some(dir))
      }
    case None => Resume.resolve(dataDir)
  }
```

Conventions:

- `createTournament` returns `(ServeContext, TournamentState)` or updates context via `withTournamentDir` at the HTTP layer.
- `loadLeaderboard`: when `state.completed`, always load from `PeriodLoader.loadAll(dataDir)`.
- Reuse existing error types; add `WizardError` variants for invalid race-to rather than raw strings in replay.
- No new runtime dependencies.

---

## Testing Strategy

| Concern | Level | Location |
|---------|-------|----------|
| Pinned dir re-resolves after complete | Integration | `ServeCheckpointSpec` or new `ServeContextSpec` |
| Create second tournament after complete (pinned context) | Integration | `WriteApiSpec` |
| Leaderboard after HTTP complete | Integration | `ReadApiSpec` |
| Complete: period write then append failure | Integration | `WriteApiSpec` / `ServeCheckpointSpec` |
| Replay rejects invalid scores, handicap, duplicates | Unit | `ReplaySpec` |
| Race-to bounds + seed without race-to | Unit | `TournamentSpec`, `SeedSpec` |
| InvalidSeq → 409 not 500 | Integration | `WriteApiSpec` |
| Handicap JVM/JS parity | Unit | `liga-js` `HandicapPreviewParitySpec` (extend) |
| Full venue flow | E2E | `EndToEndSpec` |

Requirements:

- Every **Required** success criterion has at least one test.
- Replay tests write corrupt events **directly to disk** (bypass HTTP) to prove fold integrity.
- Existing eight- and sixteen-player E2E flows must pass unchanged.

---

## Boundaries

### Always

- Run `sbt --client fixup` and complete the fixup/`git status` loop before commit.
- Preserve append-only event log semantics.
- Preserve period file immutability (`PeriodEmission.write` must not overwrite).
- Keep director writes localhost-only in `--lan` mode.
- Command and replay paths must call the **same** validation functions.

### Ask first

- Adding `liga-common` crossProject (touches `build.sbt`, `project/Dependencies.scala`).
- Changing `TournamentPhase` API wire labels.
- Serving only `audience.js` on non-loopback (thermo low item).
- Caching `fastLinkJS` or decoupling `liga` from `ligaJs` dependency.
- Full API type consolidation / codegen.

### Never

- Weaken `--lan` write protection.
- Overwrite existing `.liga` period files.
- Remove or skip failing tests to green CI.
- Ship replay validation that is weaker than command validation.

---

## Success Criteria

### Phase 1 — Venue correctness (required)

- [ ] **Pinned dir re-resolution:** When `tournamentDir` is pinned and that tournament replays as `completed`, `activeDirOption` falls through to `Resume.resolve`. Verified with pinned-context integration test.
- [ ] **Create updates pin:** `createTournament` (or its HTTP handler) calls `withTournamentDir(newDir)` so subsequent writes use the new tournament.
- [ ] **Leaderboard after complete:** When `state.completed`, `loadLeaderboard` uses `PeriodLoader.loadAll(dataDir)`, not `frozenRatings`. `ReadApiSpec` proves HTTP response includes post-tournament ratings.
- [ ] **Second-tournament UX:** Director UI shows a "New tournament" (or equivalent) action when `phase == completed` that calls create and transitions to wizard flow.
- [ ] **Complete atomicity (inverse case):** If `EventLog.append` fails after `PeriodEmission.write`, the period file is rolled back (or write is deferred until after append). Retry does not leave orphan period file. Test simulates append failure.
- [ ] **Misleading create error:** When `Resume.resolve` finds an incomplete tournament, error reads `"an incomplete tournament already exists; resume or remove it first"` (not generic "directory already exists").

### Phase 2 — Event-log integrity + shared math (required)

- [ ] **Replay score validation:** Replay rejects `MatchResult` events where winner score ≠ race-to or loser score ≥ race-to (same rules as `Tournament.validateScores`).
- [ ] **Replay handicap validation:** Replay rejects `HandicapApplied` outside `0 … floor(0.75 × race-to)`.
- [ ] **Replay wizard validation:** Replay rejects `PlayersSet` with duplicate names; rejects seed-related preconditions matching `Seed.validateState`.
- [ ] **`liga-common` crossProject:** `Handicap`, `WinProbability`, `Tuning` live in one module; JVM and JS depend on it; duplicated `liga-js/glicko/*` copies removed.
- [ ] **Handicap cap constant:** `0.75` race-to factor defined once in shared code (not four copies).
- [ ] **Minimal JS tests:** At least one `liga-js` test proves handicap preview matches JVM for a representative fixture.

### Phase 3 — API hardening (required)

- [ ] **Race-to bounds:** `setRoundRaceTo` and `Seed.buildEvents` reject `raceTo < 1` (or document and enforce minimum of 1).
- [ ] **Seed guard:** `Seed.validateState` requires `TournamentPhase.raceToComplete(state)` before seeding.
- [ ] **InvalidSeq → 409:** `EventLog.InvalidSeq` maps to HTTP 409 with retryable message, not generic 500.

### Optional (implement if time permits)

- [ ] Extract inline CSS from `DirectorApp.scala` to static asset.
- [ ] Fix `BracketLayout.roundOf("gf-1")` to match server round semantics for display.
- [ ] Cache `fastLinkJS` in `resourceGenerators` (devex).
- [ ] Block `director.js` on non-loopback in `--lan` mode.

### Verification gate

- [ ] `sbt --client "liga/test"` passes.
- [ ] `sbt --client "liga-js/test"` passes (after Phase 2).
- [ ] `sbt --client fixup && git status` clean.
- [ ] `EndToEndSpec` eight- and sixteen-player flows pass.

---

## Implementation order

```
Phase 1 (venue)          Phase 2 (integrity)       Phase 3 (hardening)
─────────────────        ───────────────────       ───────────────────
1. activeDirOption       1. TournamentValidation   1. race-to bounds
2. createTournament pin  2. Replay wiring + tests  2. Seed raceToComplete
3. loadLeaderboard       3. liga-common extract    3. InvalidSeq → 409
4. complete atomicity    4. Delete JS duplicates
5. DirectorApp UX        5. JS parity tests
6. create error message
```

Phases 1 and 2 should land before merge; Phase 3 can follow in the same PR or a fast-follow.

---

## Open Questions

1. **Complete ordering:** Thermo found period-before-event ordering risk. Prior remediation fixed event-before-period. Confirm final order: **append `TournamentCompleted` first, then write period file** (with rollback on write failure)? Or temp-file rename after both succeed?
2. **Second tournament same name/date:** `Resume.tournamentDirName` may collide. Should create auto-suffix (`-2`) or reject with 409? Default proposal: **reject with clear message** if dir exists.
3. **`liga-common` scope:** Move only handicap/glicko/tuning, or also shared `Player`/`BracketMatch` types? Default proposal: **minimal move** — only duplicated math + types it needs; keep full `Types.scala` on JVM for now.
4. **Race-to minimum:** Reject `< 1` or `< 2`? Default proposal: **reject `< 1`** (allow race-to 1 for edge cases).
5. **Phase 3 in same PR?** Default proposal: **yes** if small; otherwise fast-follow within same merge window.

---

*Approve this spec (or note corrections to Open Questions) before implementation begins.*
