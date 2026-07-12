# Spec: Liga code-review remediation

> **Source:** Five-axis review of `liga` branch vs `main` (2026-07-12)  
> **Status:** Approved — open questions resolved (2026-07-12)

## Assumptions

1. Scope is **remediation only** — no new features, no refactors unrelated to review findings.
2. **Important** findings are in scope; **Suggestion** items are in scope only where listed under Success Criteria (optional items marked).
3. The existing security model stands: director writes are localhost-only; `--lan` exposes read-only audience API.
4. Billiards scoring rules: winner's board total equals race-to; loser's board total is strictly less than race-to; scores are non-negative integers; ties are invalid.
5. Handicap override remains director-controlled, but must be bounded server-side to `0 … floor(0.75 × race-to)` (matching `Handicap.suggest` and `DirectorGuidance.handicapCap`).
6. `SPEC.md` lives at repo root for this remediation pass; it does not replace `docs/specs/liga.md`.
7. Complete failure returns **409** when the period file already exists, **500** for other I/O errors.
8. Winner's score must **equal** race-to exactly; loser score must be **strictly less** than race-to.
9. Duplicate roster check is **case-sensitive** (`"Alice"` and `"alice"` are distinct).
10. All former optional items (static path sanitization, generic 500s, phase naming comment) are **in scope**.

---

## Objective

Harden the Liga tournament server and domain validation identified in the pre-merge code review so that:

- Tournament completion cannot leave the system in an inconsistent state (completed event without period file).
- Match results and handicaps accepted by the API are valid under billiards rules.
- Rosters cannot contain duplicate player names.
- Director write protection cannot be bypassed when `remoteAddress` is absent.

### Users

| User | Impact |
|------|--------|
| Tournament director | Clearer API errors; invalid scores/handicaps rejected before corrupting state |
| Club organizer | Period files and leaderboard remain trustworthy after tournament complete |
| Spectator | No behaviour change (read-only) |

### User stories

1. As a director, when I complete a tournament, either a valid period file is written **or** the tournament remains incomplete (no orphaned `TournamentCompleted` event).
2. As a director, when I record a result, the server rejects scores that do not match the configured race-to for that bracket round.
3. As a director, when I set the roster, the server rejects duplicate display names before lock/seed.
4. As a director, when I apply a handicap, the server rejects values outside `0 … floor(0.75 × race-to)`.
5. As an operator running `--lan`, write routes remain blocked unless the request originates from loopback, even if `remoteAddress` is missing.

---

## Tech Stack

Unchanged from existing Liga stack:

| Layer | Choice |
|-------|--------|
| Language | Scala 3.8.4 |
| Effects | ZIO 2.1.26 |
| HTTP | zio-http 3.11.2 |
| JSON | zio-json 0.9.2 |
| Tests | zio-test |
| Frontend guidance | `liga-js` (`DirectorGuidance`) — error strings must stay compatible |

---

## Commands

```bash
# Format + compile + test (required before commit)
sbt --client fixup

# Module tests
sbt --client "liga/test"
sbt --client "liga/testOnly *TournamentSpec"
sbt --client "liga/testOnly *ServeContext*"
sbt --client "liga/testOnly *WriteApiSpec"
sbt --client "liga/testOnly *BindConfigSpec"
sbt --client "liga/testOnly *EndToEndSpec"

# Verify clean tree after fixup loop
sbt --client fixup && git status
```

---

## Project Structure

```
liga/src/main/scala/ph/samson/atbp/liga/
  serve/
    BindConfig.scala          # isLocalDirector fix
    ServeContext.scala        # completeTournament ordering
  tournament/
    Tournament.scala          # validateScores, setPlayers, applyHandicap
    MatchLifecycle.scala      # (read-only reuse for race-to resolution)

liga/src/test/scala/ph/samson/atbp/liga/
  tournament/TournamentSpec.scala
  serve/WriteApiSpec.scala
  serve/BindConfigSpec.scala
  serve/ServeCheckpointSpec.scala   # if complete-flow covered here
  EndToEndSpec.scala

liga-js/src/main/scala/ph/samson/atbp/liga/js/director/
  DirectorGuidance.scala    # optional: map new error messages
```

---

## Code Style

Follow existing Liga patterns: pure `Either` validation in `Tournament` / `MatchLifecycle`, `ServeContext.CommandError` at HTTP boundary, descriptive sealed error types with `message` strings.

Example — race-to score validation (illustrative):

```scala
private def validateScores(
    state: TournamentState,
    matchDef: BracketMatch,
    scoreA: Int,
    scoreB: Int
): Either[Error, Unit] =
  for {
    raceTo <- MatchLifecycle.resolveRaceTo(state, matchDef.id)
    _ <-
      if (scoreA == scoreB) {
        Left(invalidResult(matchDef.id, "scores cannot tie"))
      } else if (scoreA < 0 || scoreB < 0) {
        Left(invalidResult(matchDef.id, "scores must be non-negative"))
      } else {
        val winnerScore = math.max(scoreA, scoreB)
        val loserScore = math.min(scoreA, scoreB)
        if (winnerScore != raceTo) {
          Left(invalidResult(matchDef.id, s"winner score must be $raceTo"))
        } else if (loserScore >= raceTo) {
          Left(invalidResult(matchDef.id, s"loser score must be less than $raceTo"))
        } else {
          Right(())
        }
      }
  } yield ()
```

Conventions:

- Reuse `MatchLifecycle.InvalidTransitionError` for match-scoped validation failures.
- Add `WizardError` variants for roster-level issues (e.g. duplicate names).
- Do not add new dependencies.
- Keep HTTP handlers thin; no validation logic in `DirectorRoutes` beyond JSON parsing.

---

## Testing Strategy

| Concern | Level | Location |
|---------|-------|----------|
| Score / handicap / duplicate-player rules | Unit | `TournamentSpec` |
| `isLocalDirector` with `None` remote | Unit | `BindConfigSpec` |
| Complete ordering + period write failure | Integration | `ServeCheckpointSpec` or new `ServeContextSpec` |
| HTTP 400 on invalid result/handicap | Integration | `WriteApiSpec` |
| Full tournament still completes | E2E | `EndToEndSpec` (existing tests must pass) |

Requirements:

- Every **Important** fix has at least one failing-then-passing test (TDD where practical).
- Regression tests for previously accepted invalid input (e.g. `scoreA: 999`).
- No test-only shortcuts that bypass `Tournament` / `ServeContext` production paths.

---

## Boundaries

### Always

- Run `sbt --client fixup` and complete the fixup/`git status` loop before commit.
- Preserve append-only event log semantics (no in-place event edits).
- Preserve existing period file immutability (`PeriodEmission.write` must not overwrite).
- Keep director writes localhost-only in `--lan` mode.
- Match `DirectorGuidance.friendlyApiError` for any new user-facing error substrings where practical.

### Ask first

- Changing score semantics (e.g. allowing winner > race-to for special formats).
- Adding file locking or concurrent-write support beyond documenting single-director assumption.
- Renaming `TournamentPhase` API labels (`locked` / `raceTo`).
- Refactoring `nextSeq` / `EventLog.append` duplication.

### Never

- Weaken `--lan` write protection.
- Write period files that overwrite existing `.liga` files.
- Remove or skip failing tests to green CI.
- Bundle unrelated feature work into this remediation.

---

## Success Criteria

### Required (Important)

- [ ] **`completeTournament` atomicity:** If `PeriodEmission.write` fails, no `TournamentCompleted` event remains on disk. Verified by test simulating write failure (e.g. pre-create target file or inject failing layer). HTTP: **409** when target `.liga` exists, **500** for other I/O errors.
- [ ] **Race-to score validation:** `Tournament.recordResult` rejects when `max(scoreA, scoreB) != raceTo` or `min(scoreA, scoreB) >= raceTo`. HTTP POST returns 400 with clear message.
- [ ] **Duplicate roster names:** `Tournament.setPlayers` rejects when `players.distinct.size != players.size` (case-sensitive `Player` equality). HTTP POST `/api/tournament/players` returns 400.
- [ ] **`isLocalDirector` fix:** `BindConfig.isLocalDirector` returns `false` when `remoteAddress` is `None`. Test added; existing `BindConfigSpec` LAN write-block tests still pass.

### Required (handicap — promoted from Suggestion)

- [ ] **Handicap bounds:** `Tournament.applyHandicap` rejects `handicap < 0` or `handicap > floor(0.75 × raceTo)` for the match's round. HTTP returns 400.

### Required (formerly optional)

- [ ] Static asset handler rejects `fileName` containing `..` or `/`.
- [ ] Generic 500 message for unexpected errors (no raw exception text to client).
- [ ] Comment on `TournamentPhase.derive` explaining `locked` vs `raceTo` naming.

### Verification gate

- [ ] `sbt --client "liga/test"` passes.
- [ ] `sbt --client fixup && git status` clean.
- [ ] Existing `EndToEndSpec` eight- and sixteen-player flows still pass unchanged.

---

## Implementation order

1. `BindConfig.isLocalDirector` + test (smallest, security-first).
2. `Tournament.setPlayers` duplicate check + test.
3. `Tournament.applyHandicap` bounds + test.
4. `Tournament.validateScores` race-to rules + `recordResult` wiring + tests.
5. `ServeContext.completeTournament` reorder / rollback + integration test.
6. `DirectorGuidance` error mapping for new messages (if needed).
7. Static asset path sanitization + test.
8. Generic 500 responses in `DirectorRoutes` / read API.
9. `TournamentPhase.derive` naming comment.

---

## Decisions (resolved)

| # | Question | Decision |
|---|----------|----------|
| 1 | Complete failure HTTP status | **409** when period file exists; **500** for other I/O errors |
| 2 | Score vs race-to | Winner score must **equal** race-to exactly |
| 3 | Duplicate name comparison | **Case-sensitive** (match `Player` equality) |
| 4 | Optional items scope | **All in scope** for this pass |

---

*Spec approved. Proceed to plan/tasks, then implementation.*
