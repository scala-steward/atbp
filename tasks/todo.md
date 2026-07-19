# Liga Batch Period Ratings — Task Checklist

Source: [`tasks/plan.md`](plan.md) · [`SPEC.md`](../SPEC.md)

## Phase 1: Foundation

### Task 1: Validate `afterPeriod(Nil)` library semantics
- [x] Add direct `GlickoPlayer.afterPeriod(Nil, Tuning.Default)` test in `Glicko2Spec`
- [x] Assert rating unchanged (within `approx`)
- [x] Assert RD increased
- [x] Assert volatility finite
- [x] Verify: `sbt --client "liga/testOnly *Glicko2*"`

### Task 2: Implement `Glicko2.updateAfterPeriod`
- [x] Add `updateAfterPeriod(priorSnapshot, period)` with order-independence scaladoc
- [x] Freeze period-start snapshot for opponent references
- [x] Collect `Seq[Result]` per player via `ScoreExpansion`
- [x] Update played players with `afterPeriod(results)` and correct W-L
- [x] Update inactive prior players with `afterPeriod(Nil)` and unchanged W-L
- [x] Handle debut players (new entry + one `afterPeriod`)
- [x] Migrate single-match 7-4 test to `updateAfterPeriod`
- [x] Verify: `sbt --client "liga/testOnly *Glicko2*"` and `sbt --client compile`

### Checkpoint: Foundation
- [x] `updateAfterPeriod` compiles and single-match tests pass
- [x] `afterPeriod(Nil)` library test passes
- [x] Review algorithm against SPEC steps 1-5

## Phase 2: Integration

### Task 3: Wire `Leaderboard.compute`
- [x] Replace inner `matches.foldLeft` with `updateAfterPeriod` per period
- [x] Verify: `sbt --client "liga/testOnly *PeriodLoader*"`

### Task 4: Batch semantics and order-independence tests
- [x] Multi-opponent period test (Alice beats Bob 7-4, loses to Carol 4-7)
- [x] Rematch within period uses period-start opponent ratings
- [x] Property test: shuffled `period.matches` → identical snapshot
- [x] Verify: `sbt --client "liga/testOnly *Glicko2*"`

### Checkpoint: Core batch semantics
- [x] Order-independence property test passes
- [x] Multi-match scenarios pass
- [x] `Leaderboard.compute` integration works end-to-end

## Phase 3: Edge Cases and Golden Fixtures

### Task 5: Reject empty period files at load time
- [ ] Fail `PeriodCodec.toPeriod` when `matches.isEmpty`
- [ ] Add `period-loader/empty/empty.liga` fixture
- [ ] Add `PeriodLoaderSpec` test with clear error message
- [ ] Verify: `sbt --client "liga/testOnly *PeriodLoader*"`

### Task 6: `LeaderboardSpec` golden values and inactive RD regression
- [ ] Create `LeaderboardSpec.scala`
- [ ] Load golden fixture via `PeriodLoader`
- [ ] Assert commented numeric expected rating/RD/W-L values
- [ ] Assert Bob's RD increases in period 2 (did not play)
- [ ] Assert Bob's W-L unchanged in period 2
- [ ] Reduce `PeriodLoaderSpec` golden test to self-consistency only
- [ ] Verify: `sbt --client "liga/testOnly *Leaderboard*"` and `sbt --client "liga/testOnly *PeriodLoader*"`

### Checkpoint: Fixtures and edge cases
- [ ] Golden expected values documented and passing
- [ ] Empty period rejection works
- [ ] Bob inactive RD inflation verified

## Phase 4: Cleanup

### Task 7: Remove deprecated APIs and finalize tests
- [ ] Delete `updateAfterMatch` from `Glicko2`
- [ ] Delete `updateAfterGame` from `Glicko2`
- [ ] Migrate remaining `Glicko2Spec` tests to `Period` + `updateAfterPeriod`
- [ ] Preserve direct library golden vector tests
- [ ] Verify: `sbt --client "liga/test"`
- [ ] Verify: `git add -A && sbt --client fixup && git status` clean

### Checkpoint: Complete
- [ ] All SPEC acceptance criteria met
- [ ] Full test suite green
- [ ] Working tree clean after fixup
- [ ] Ready for human review / commit
