# Spec: Liga batch period ratings

Source idea: [`docs/ideas/liga-batch-period-ratings.md`](docs/ideas/liga-batch-period-ratings.md)

## Objective

**What we're building.** Replace match-by-match Glicko2 folding within a `.liga` period file with a single batch update per file. Each period file is exactly one Glicko2 rating period: every known player is updated once from period-start ratings, all match results are batched together, match order is irrelevant, and inactive players' RD/volatility reflect that the period passed.

**Why.** The current `Leaderboard.compute` inner fold (`period.matches.foldLeft(state)(Glicko2.updateAfterMatch)`) violates Glicko2 rating-period semantics:

- Opponent ratings change between matches in the same file (order-dependent output).
- Players who sit out a period never get `afterPeriod(Nil)`, so their RD stays falsely tight.

**Who benefits.** Club directors and players using `atbp liga` leaderboard and handicap suggestions — ratings, RD, and seeding must reflect correct Glicko2 uncertainty for players who didn't play.

**Success looks like.** `Glicko2.updateAfterPeriod` is the only rating-update entry point; `Leaderboard.compute` calls it once per period file; golden fixtures and property tests prove order-independence and inactive-player RD inflation.

### User stories

1. As a director, when I add a period file with multiple matches, leaderboard ratings must not depend on the order matches appear in the file.
2. As a director, when a player sat out a period, their RD should increase to reflect elapsed time — even if they don't appear in that file's matches.
3. As a developer, I can verify batch semantics with focused unit tests and a golden fixture that documents expected values.

### Acceptance criteria

- [ ] `Glicko2.updateAfterPeriod(priorSnapshot, period): Snapshot` exists with scaladoc stating the order-independence invariant.
- [ ] `Leaderboard.compute` uses only `updateAfterPeriod` (no inner match fold).
- [ ] `updateAfterMatch` and `updateAfterGame` are removed from `Glicko2`.
- [ ] All tests pass, including new cases listed under Testing Strategy.
- [ ] Golden fixture expected values are updated deliberately with commented expected ratings in `LeaderboardSpec` (not silent drift).
- [ ] Period files with zero matches are rejected at load time.

## Tech Stack

| Layer | Choice |
|-------|--------|
| Language | Scala 3.8.4 (`-no-indent`, `-old-syntax`) |
| Effects / tests | ZIO, ZIO Test |
| Glicko2 library | `com.github.mrdimosthenis` `glicko2` `1.0.1` (`dimos.glicko2.*`) |
| Module | `liga` (JVM); shared types in `liga-common` |
| Formatting | Scalafmt + Scalafix via `sbt fixup` |

No new dependencies. No changes to `.liga` file format, tournament serve mode, or `frozenRatings` behavior.

## Commands

```bash
# Compile
sbt --client compile

# All liga tests
sbt --client "liga/test"

# Focused test runs during development
sbt --client "liga/testOnly *Glicko2*"
sbt --client "liga/testOnly *PeriodLoader*"
sbt --client "liga/testOnly *Leaderboard*"

# Required before any Scala commit
git add -A   # include new/untracked sources
sbt --client fixup && git status
# repeat until fixup succeeds AND working tree is clean for this commit
```

## Project Structure

```
liga/src/main/scala/ph/samson/atbp/liga/
  glicko/
    Glicko2.scala       ← add updateAfterPeriod; remove updateAfterMatch/Game
    Leaderboard.scala   ← single updateAfterPeriod call per period
  io/
    PeriodLoader.scala  ← reject period files with zero matches
  model/
    ScoreExpansion.scala  ← reuse for per-game outcome expansion
    Types.scala           ← Period, PeriodMatch (unchanged)

liga/src/test/scala/ph/samson/atbp/liga/
  glicko/
    Glicko2Spec.scala       ← rewrite tests for updateAfterPeriod
    LeaderboardSpec.scala   ← new: batch semantics + golden values
  io/
    PeriodLoaderSpec.scala  ← add empty-period rejection test; keep self-consistency golden test

liga/src/test/resources/
  period-loader/golden/     ← 2026-01-10.liga, 2026-03-15.liga (unchanged files)
  glicko/golden-vectors.txt ← library reference vectors (unchanged)
```

## Code Style

Follow existing `liga` conventions: `final case class` models, `object` companions for logic, ZIO Test suites, `approx` helper for floating-point assertions (±0.001).

**Target API shape:**

```scala
/** Apply one Glicko2 rating period from a period-start snapshot.
  *
  * Within a period, the order of `period.matches` is presentation-only.
  * Shuffling match rows must produce identical ratings, RD, volatility,
  * and W–L for every player.
  *
  * @param priorSnapshot cumulative state after all earlier period files
  * @param period one `.liga` file's matches and metadata
  */
def updateAfterPeriod(priorSnapshot: Snapshot, period: Period): Snapshot
```

**Algorithm (implementation guide):**

1. Freeze `priorSnapshot` — all opponent references for this period come from here.
2. For each match, expand scores via `ScoreExpansion.expandGames`; build `Seq[Result]` per player using **period-start** opponent `GlickoPlayer` values.
3. Update every player in `priorSnapshot`:
   - Played → `afterPeriod(results, tuning)`; W–L incremented from match scores.
   - Did not play → `afterPeriod(Nil, tuning)` — rating unchanged, RD/volatility advance; W–L unchanged.
4. Debut players (in matches but not in `priorSnapshot`) → new entry, then one `afterPeriod` with their results.
5. Return map of all players (prior + debuts).

**Leaderboard.compute after change:**

```scala
def compute(periods: List[Period]): List[PlayerRating] = {
  val snapshot =
    periods.foldLeft(Glicko2.empty) { (state, period) =>
      Glicko2.updateAfterPeriod(state, period)
    }
  Glicko2.leaderboard(snapshot.view.mapValues(_.toPlayerRating).toMap)
}
```

## Testing Strategy

**Framework:** ZIO Test (`ZIOSpecDefault`, `suite`, `test`, `assertTrue`).

**Test locations:** `liga/src/test/scala/ph/samson/atbp/liga/glicko/` (primary), `liga/src/test/scala/ph/samson/atbp/liga/io/` (integration).

### Required test cases

| Case | What to assert |
|------|----------------|
| `afterPeriod(Nil)` library semantics | Direct `dimos.glicko2.Player.afterPeriod(Nil)` — RD rises, rating stable, volatility updated |
| Single-match period | One match in a `Period` produces correct W–L and ratings |
| Match-order independence | Shuffle `period.matches` → identical snapshot (property test) |
| Multi-match, multi-opponent | Alice beats Bob 7–4 and loses to Carol 4–7 in one period → one batch update |
| Rematch within period | Two matches vs same opponent both use period-start opponent μ/φ |
| Inactive player RD increase | Bob in period 1, absent period 2 → Bob's RD increases in period 2; W–L unchanged |
| Empty period rejection | `.liga` file with zero matches fails at load time with a clear error |
| Golden fixture | Hard-coded expected ratings in `LeaderboardSpec` for `period-loader/golden` after batch recompute |
| Existing golden vectors | Library `afterPeriod` tests in `Glicko2Spec` remain (direct library calls) |

### Coverage expectations

- No coverage tooling gate; behavioral completeness is enforced by the cases above.
- Migrate tests that call `updateAfterMatch` / `updateAfterGame` to construct `Period` values and call `updateAfterPeriod`.
- Property tests use `approx` (±0.001) for doubles, matching existing `Glicko2Spec`.

### Verification checkpoint

```bash
sbt --client "liga/testOnly *Glicko2*"
sbt --client "liga/testOnly *PeriodLoader*"
sbt --client "liga/test"
```

## Boundaries

### Always

- Run `sbt --client fixup` and confirm a clean `git status` before committing Scala changes.
- Use period-start opponent ratings for all games within a period.
- Update inactive players (`priorSnapshot` keys who didn't play) with `afterPeriod(Nil)`.
- Document the order-independence invariant on `updateAfterPeriod` scaladoc and in `LeaderboardSpec` comments.
- Update golden expected values deliberately with comments explaining batch semantics in `LeaderboardSpec`.
- Reject period files with zero matches at load time.

### Ask first

- Changing `Tuning.Default` parameters (init rating, max RD, τ).
- Adding dependencies or changing `build.sbt` / `project/`.
- Changing tournament `frozenRatings` or in-progress serve behavior.
- Embedding frozen ratings in `.liga` file format.
- Batching across multiple period files (cross-period batching is out of scope).

### Never

- Reintroduce `updateAfterMatch` or `updateAfterGame` as public API.
- Fold matches sequentially within a period (the bug being fixed).
- Update only match participants — inactive club members must advance too.
- Treat match order in a file as a Glicko2 input.
- Silently drift golden fixture expected values without documented rationale.
- Change `liga-js`, serve routes, or tournament replay in this work.

## Success Criteria

Specific, testable conditions for "done":

1. `Glicko2.updateAfterPeriod` is the sole rating-update entry point; `updateAfterMatch` and `updateAfterGame` are deleted.
2. `Leaderboard.compute` has no inner `matches.foldLeft`.
3. Property test: shuffling `period.matches` yields bit-for-bit identical `Snapshot` (within `approx` tolerance for doubles).
4. `LeaderboardSpec` asserts Bob's RD increases in period 2 despite not playing (commented numeric expected values).
5. Period files with zero matches are rejected at load time.
6. `sbt --client "liga/test"` passes.
7. `sbt --client fixup` passes with clean working tree.

## Resolved Decisions

| Question | Decision |
|----------|----------|
| Golden fixture assertions | New `LeaderboardSpec` with commented numeric expected values; `PeriodLoaderSpec` keeps self-consistency only |
| Empty period files | Reject at load time — a `.liga` file must have at least one match |
| W–L for inactive players | W–L unchanged; only RD and volatility advance via `afterPeriod(Nil)` |
| Match-order documentation | Scaladoc on `updateAfterPeriod` plus comments in `LeaderboardSpec` |

## Assumptions

1. **One `.liga` file = one Glicko2 rating period** — aligns with `docs/ideas/liga.md`.
2. **`dimos.glicko2.Player.afterPeriod(Nil)`** correctly models inactive periods (RD rises, rating stable).
3. **Debut players** enter as new Glicko2 entries and receive one `afterPeriod` with their results; no prior inactive pass.
4. **Cross-period chaining** stays sequential file-by-file; only within-file batching changes.
5. **Tournament serve / `frozenRatings`** is unaffected — already freezes at bracket seed.
6. **Golden fixture values will change** — Bob's RD in period 2 is the canonical regression case.

## Out of Scope

- Embedding frozen ratings in `.liga` files.
- Changing tournament in-progress / `frozenRatings` behavior.
- Cross-period batching.
- Persisting period-start snapshots in files.
- CLI output format changes (same fields: rating, RD, W–L).
