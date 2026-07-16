# Section-Aware Race-To — Task Checklist

Source: [`SPEC.md`](../SPEC.md) · Plan: [`tasks/plan.md`](plan.md)

---

## Phase 1: Shared Foundation

- [x] **Task 1:** `RaceToScopes` in `liga-common` (keyForMatch, requiredKeys, scopeLabel)
- [x] **Task 2:** `RaceToWizard` in `liga-common` (initialState, loadState, applyEdit, GF pin)

### Checkpoint: Foundation
- [x] `sbt --client "ligaCommonJVM/test"` passes
- [x] Review plan before server migration

---

## Phase 2: Server Domain

- [x] **Task 3:** State, events, replay (`raceToByScope`, `RaceToSet`, EventLog `race-to`)
- [x] **Task 4:** Resolve, phase gating, seed (MatchLifecycle, TournamentPhase, Seed)
- [x] **Task 5:** Delete `BracketRounds`; update domain test helpers

### Checkpoint: Server Domain
- [x] No `roundRaceTo` / `RoundRaceToSet` in `liga/` main sources
- [x] `sbt --client "liga/testOnly *Seed*"` and `*EventCodec*` pass
- [x] Review before API layer

---

## Phase 3: API Contract

- [x] **Task 6:** HTTP routes + JSON (`DirectorRoutes`, `ApiJson`, serve tests)
- [x] **Task 7:** JS models + client (`Models`, `Client`, `DirectorApp`, `MatchPanel`)

### Checkpoint: API Contract
- [x] API contract tests pass
- [x] `liga-js/compile` succeeds
- [x] Review before fixture rewrite

---

## Phase 4: Fixtures and Integration

- [x] **Task 8:** Rewrite `*-round-race-to.json` → `*-race-to.json` (all 3 fixture dirs)
- [x] **Task 9:** `EndToEndSpec` + integration sweep; differentiated 8-player scenario

### Checkpoint: Integration
- [x] `sbt --client "liga/test"` passes
- [x] Review before UI work

---

## Phase 5: Director Wizard UI

- [x] **Task 10:** Section-grouped `WizardView` with cascade; delete `WizardRounds`

### Checkpoint: Complete
- [x] `sbt --client "ligaCommonJVM/test"`, `liga/test`, `liga-js/test` pass
- [x] `sbt --client fixup && git status` clean
- [x] SPEC success criteria verified
- [x] Ready for PR

---

## Quick reference — verification commands

```bash
sbt --client "ligaCommonJVM/testOnly *RaceTo*"
sbt --client "liga/testOnly *Replay*"
sbt --client "liga/testOnly *Seed*"
sbt --client "liga/testOnly *EventCodec*"
sbt --client "liga/testOnly *Api*"
sbt --client "liga/testOnly *EndToEnd*"
sbt --client "ligaCommonJVM/test"
sbt --client "liga/test"
sbt --client "liga-js/test"
sbt --client fixup
```
