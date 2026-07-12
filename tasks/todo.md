# Thermo Remediation — Task Checklist

Source: [SPEC.md](../SPEC.md) · Plan: [plan.md](plan.md)

## Phase 1: Venue Correctness

- [x] **Task 1:** Fix `loadLeaderboard` for completed state + `ReadApiSpec`
- [x] **Task 2:** Complete idempotent retry (verify/skip/append) + tests
- [x] **Task 3:** Create error messages + dir collision 409 + `WriteApiSpec`

### Checkpoint: Phase 1
- [x] `sbt --client "liga/test"` passes for serve specs
- [x] Director can complete tournament; audience leaderboard shows updated ratings
- [x] Complete retry and create-collision paths verified in tests

## Phase 2: Event-Log Integrity + Shared Math

- [x] **Task 4:** Extract `TournamentValidation`; refactor command handlers
- [x] **Task 5:** Wire `Replay` through `TournamentValidation` + `ReplaySpec` negatives
- [x] **Task 6:** Add `liga-common` crossProject (`build.sbt` + `Dependencies`)
- [x] **Task 7:** Move shared math/types; delete JS dupes; `HandicapCap` constant
- [x] **Task 8:** `liga-js` parity tests

### Checkpoint: Phase 2
- [x] `ReplaySpec` negative cases pass
- [x] `liga-js/test` passes
- [x] No `0.75` literals outside shared `HandicapCap` (except CSS)

## Phase 3: API Hardening

- [x] **Task 9:** Race-to bounds (`< 2`) in `Tournament`, `Seed`, `Replay`
- [x] **Task 10:** Seed `raceToComplete` guard + `SeedSpec`/`WriteApiSpec` updates
- [x] **Task 11:** `InvalidSeq` → 409 + `WriteApiSpec`

### Checkpoint: Complete
- [x] `sbt --client "liga/test"` passes
- [x] `sbt --client "liga-js/test"` passes
- [ ] `sbt --client fixup && git status` clean
- [x] `EndToEndSpec` eight- and sixteen-player flows pass
- [ ] All SPEC required success criteria checked off

## Optional (not blocking PR)

- [ ] Extract inline CSS from `DirectorApp.scala`
- [ ] Fix `BracketLayout.roundOf("gf-1")` display semantics
- [ ] Cache `fastLinkJS` in `resourceGenerators`
- [ ] Block `director.js` on non-loopback in `--lan` mode
