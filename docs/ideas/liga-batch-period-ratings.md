# Liga batch period rating update

## Problem Statement

**How might we apply Glicko2 so each `.liga` period file is exactly one rating period — every known player updated once from period-start ratings, all match results batched together, match order irrelevant, and inactive players' RD/volatility reflecting that the period passed?**

## Recommended Direction

**One rating update per period file. No match-level update API.**

Replace the current `Leaderboard.compute` inner fold (`period.matches.foldLeft(state)(Glicko2.updateAfterMatch)`) with a single call per file:

```scala
periods.foldLeft(Glicko2.empty) { (state, period) =>
  Glicko2.updateAfterPeriod(state, period)
}
```

`Glicko2.updateAfterPeriod(priorSnapshot, period)` is the only rating-update entry point. A period with one match is still a period — there is no `updateAfterMatch` or `updateAfterGame`.

### Algorithm

1. **Freeze period-start snapshot.** `priorSnapshot` is the cumulative state after all earlier files (rating, RD, volatility, W–L). All opponent references for this period's games come from here — never from interim post-match state.

2. **Collect results per player.** For each match, expand final scores to per-game outcomes (`ScoreExpansion`). Build `Seq[Result]` per player, pairing each game with the **period-start** opponent `GlickoPlayer`. Rematches in the same file still reference the same period-start opponent rating.

3. **Update every known player.** The player universe at period start is everyone in `priorSnapshot`:
   - **Played this period** → `player.afterPeriod(results, tuning)`; W–L incremented from match scores.
   - **Did not play** → `player.afterPeriod(Nil, tuning)` — inactive period; rating unchanged, RD increases (uncertainty from time passing), volatility recomputed per Glicko2.

4. **Debut players.** Anyone appearing in `period.matches` but absent from `priorSnapshot` enters as a new Glicko2 entry, then receives one `afterPeriod` with their results. They are not treated as "inactive" in the period they debut.

5. **Emit next snapshot.** Map of all players (prior + debuts), sorted for leaderboard output.

### Order independence (documented invariant)

> **Within a period, the order of `matches` in a `.liga` file is presentation-only.** Shuffling match rows must produce identical ratings, RD, volatility, and W–L for every player.

This extends the existing within-match guarantee to the full period. Document on `updateAfterPeriod` scaladoc and in this idea doc. Enforce with a property test that shuffles `period.matches` and asserts identical output.

### What gets removed

- `Glicko2.updateAfterMatch` — no single-match update path; one-match periods go through `updateAfterPeriod`.
- `Glicko2.updateAfterGame` — test-only today; tests should construct a `Period` instead.

### Relationship to existing docs

`docs/ideas/liga.md` already says Glicko2 runs at period boundaries and ratings are frozen during tournaments. This idea closes the **implementation gap**: the CLI ledger currently folds match-by-match within a file, which violates Glicko2 rating-period semantics and makes output depend on match order. Tournament serve mode is unaffected — it already uses `frozenRatings` at bracket seed.

## Key Assumptions to Validate

- [ ] **`afterPeriod(Nil)` is correct inactive semantics** — RD rises, rating stable, volatility updated; verify against `dimos.glicko2` with a direct library test before wiring into `updateAfterPeriod`.
- [ ] **Inactive player universe = `priorSnapshot` keys** — Bob played in period 1, sits out period 2 → Bob's RD increases after period 2. Carol debuts in period 2 → no prior inactive treatment.
- [ ] **Multi-opponent batching** — Alice beats Bob 7–4 and loses to Carol 4–7 in one file → one `afterPeriod` with both result sets, opponents at period-start ratings.
- [ ] **Rematch in same period** — two matches vs the same opponent both reference period-start opponent μ/φ.
- [ ] **Golden fixture values change** — `period-loader/golden` expected leaderboard shifts to correct batch semantics; update deliberately with commented expected values, not silent drift.

## MVP Scope

**In:**

- `Glicko2.updateAfterPeriod(priorSnapshot, period): Snapshot` with scaladoc stating order-independence invariant.
- `Leaderboard.compute` uses only `updateAfterPeriod`.
- Remove `updateAfterMatch` and `updateAfterGame`.
- Tests:
  - single-match period (one match in file still works)
  - match-order independence (`shuffle(matches)` → identical snapshot)
  - multi-match, multi-opponent period
  - rematch within period
  - inactive player RD increase (e.g. Bob in period 1, absent period 2)
  - empty period (no matches → all known players get inactive update)
  - golden fixture recomputation

**Out:**

- Embedding frozen ratings in `.liga` file format.
- Changing tournament in-progress / `frozenRatings` behavior.
- Cross-period batching (periods still chain sequentially file-by-file).

## Not Doing (and Why)

- **`updateAfterMatch` / `updateAfterGame`** — period is the only Glicko2 unit; a single-match file is just a period with one match.
- **Sequential match folding within a period** — the bug; violates Glicko2 and causes order-dependent output.
- **Updating only match participants** — inactive club members must have RD/volatility reflect the elapsed period or the ledger misstates uncertainty for players who sat out.
- **Match order as signal** — bracket progression order is not a Glicko2 input.
- **Batching across period files** — RD inflation and volatility need period boundaries between files.
- **Persisting period-start snapshot in the file** — reproducibility comes from ordered files + deterministic recompute; audit trail can come later.

## Open Questions

- **Empty period file in production** — can a `.liga` file with zero matches occur (cancelled tournament)? If yes, inactive-update-all is the right behavior; if no, still worth testing.
- **Golden fixture documentation** — add a comment in the test or fixture README that ratings are match-order independent within a file?

## Resolved Decisions

| Question | Decision |
|----------|----------|
| Rating update unit | One `.liga` file = one Glicko2 rating period |
| API surface | `updateAfterPeriod` only; no match-level update |
| Opponent ratings in period | Always period-start snapshot |
| Match order in file | Presentation only; tested for independence |
| Inactive players | All `priorSnapshot` players who didn't play get `afterPeriod(Nil)` |
| Cross-period chaining | Sequential file-by-file; only within-file batching changes |
| Debut players | New entry + one `afterPeriod` with their results; no prior inactive pass |

## Stress-test notes

The golden fixture is a concrete case: period 1 is Alice–Bob; period 2 is Alice–Carol. Today Bob's rating/RD are frozen at his post-period-1 values because he never appears in period 2's matches. Under correct Glicko2, Bob's RD should increase in period 2 even though he didn't play — his uncertainty grows with time. Without this, handicaps and seeding for Bob in period 3 would use a falsely tight RD.
