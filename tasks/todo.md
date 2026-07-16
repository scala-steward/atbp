# Section-Aware Race-To — Task Checklist

Source: [`SPEC.md`](../SPEC.md) · Plan: [`tasks/plan.md`](plan.md)

---

## Phase 1: Shared Foundation

- [ ] **Task 1:** `RaceToScopes` in `liga-common` (keyForMatch, requiredKeys, scopeLabel)
- [ ] **Task 2:** `RaceToWizard` in `liga-common` (initialState, loadState, applyEdit, GF pin)

### Checkpoint: Foundation
- [ ] `sbt --client "ligaCommonJVM/test"` passes
- [ ] Review plan before server migration

---

## Phase 2: Server Domain

- [ ] **Task 3:** State, events, replay (`raceToByScope`, `RaceToSet`, EventLog `race-to`)
- [ ] **Task 4:** Resolve, phase gating, seed (MatchLifecycle, TournamentPhase, Seed)
- [ ] **Task 5:** Delete `BracketRounds`; update domain test helpers

### Checkpoint: Server Domain
- [ ] No `roundRaceTo` / `RoundRaceToSet` in `liga/` main sources
- [ ] `sbt --client "liga/testOnly *Seed*"` and `*EventCodec*` pass
- [ ] Review before API layer

---

## Phase 3: API Contract

- [ ] **Task 6:** HTTP routes + JSON (`DirectorRoutes`, `ApiJson`, serve tests)
- [ ] **Task 7:** JS models + client (`Models`, `Client`, `DirectorApp`, `MatchPanel`)

### Checkpoint: API Contract
- [ ] API contract tests pass
- [ ] `ligaJs/compile` succeeds
- [ ] Review before fixture rewrite

---

## Phase 4: Fixtures and Integration

- [ ] **Task 8:** Rewrite `*-round-race-to.json` → `*-race-to.json` (all 3 fixture dirs)
- [ ] **Task 9:** `EndToEndSpec` + integration sweep; differentiated 8-player scenario

### Checkpoint: Integration
- [ ] `sbt --client "liga/test"` passes
- [ ] Review before UI work

---

## Phase 5: Director Wizard UI

- [ ] **Task 10:** Section-grouped `WizardView` with cascade; delete `WizardRounds`

### Checkpoint: Complete
- [ ] `sbt --client "ligaCommonJVM/test"`, `liga/test`, `ligaJs/test` pass
- [ ] `sbt --client fixup && git status` clean
- [ ] SPEC success criteria verified
- [ ] Ready for PR

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
sbt --client "ligaJs/test"
sbt --client fixup
```
