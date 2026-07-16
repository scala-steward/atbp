# Implementation Plan: Section-Aware Race-To

## Overview

Replace the flat `roundRaceTo: Map[Int, Int]` model with section-scoped keys (`wb-1`, `lb-4`, `gf`) so tournament directors can set winners, losers, and Grand Final race-to independently. Shared scope and cascade logic lives in `liga-common`; the server stores a full map with no cascade semantics; the director wizard applies cascade rules client-side and sends the complete map on save/seed.

**Spec:** [`SPEC.md`](../SPEC.md) (approved, no open questions).

**Current state:** Nothing from this spec is implemented yet. The codebase still uses `RoundRaceToSet`, `roundRaceTo`, `BracketRounds` (JVM), and `WizardRounds` (JS). `liga-common` has roster/handicap modules but no `bracket/` package.

## Architecture Decisions

| Decision | Rationale |
|----------|-----------|
| Scope keys are flat strings end-to-end | Mirrors match ID structure (`wb-3-1` → `wb-3`); avoids nested API shapes |
| `RaceToScopes` + `RaceToWizard` in `liga-common` | Single cross-project source for JVM + JS; replaces duplicated `BracketRounds` / `WizardRounds` |
| Cascade is client-only | Server stores one value per scope key; simpler replay and API contract |
| Breaking change, no migration | Rewrite fixtures/tests in place; production tournament dirs out of scope |
| `RoundRaceToSet` → `RaceToSet`; log file `round-race-to` → `race-to` | Aligns event type and filename with scope semantics |
| Bracket round counts hardcoded in `RaceToScopes` | Same approach as today's `WizardRounds`; avoids pulling `BracketTopology` into `liga-common` |

## Dependency Graph

```
RaceToScopes (liga-common)
    │
    ├── RaceToWizard (liga-common)
    │       └── WizardView UI (liga-js)
    │
    ├── TournamentState.raceToByScope + RaceToSet event
    │       ├── Replay / EventLog
    │       ├── MatchLifecycle.resolveRaceTo
    │       ├── TournamentPhase.raceToComplete
    │       └── Seed validation + emission
    │               ├── DirectorRoutes / ApiJson
    │               │       └── liga-js Models + Client
    │               └── Replay fixtures + integration tests
    │
    └── Delete BracketRounds / WizardRounds
```

Implementation order: foundation (`liga-common`) → server domain → API → fixtures → UI → cleanup.

## Task List

### Phase 1: Shared Foundation (`liga-common`)

#### Task 1: `RaceToScopes` — scope keys and labels

**Description:** Add pure scope-key helpers in `liga-common`: derive scope from match ID, list required keys per player count, and produce section-aware labels for the wizard.

**Acceptance criteria:**
- [ ] `keyForMatch("wb-3-1")` → `Some("wb-3")`, `keyForMatch("lb-4-2")` → `Some("lb-4")`, `keyForMatch("gf-1")` → `Some("gf")`
- [ ] `requiredKeys(8)` returns 8 keys: `wb-1..wb-3`, `lb-1..lb-4`, `gf`
- [ ] `requiredKeys(16)` and `requiredKeys(64)` match spec key counts
- [ ] `scopeLabel` produces section-grouped labels (e.g. `wb-3` → usable "Round 3" under Winners)

**Verification:**
- [ ] `sbt --client "ligaCommonJVM/testOnly *RaceToScopes*"` passes

**Dependencies:** None

**Files likely touched:**
- `liga-common/src/main/scala/ph/samson/atbp/liga/bracket/RaceToScopes.scala` (new)
- `liga-common/src/test/scala/ph/samson/atbp/liga/bracket/RaceToScopesSpec.scala` (new)

**Estimated scope:** S (2 files)

---

#### Task 2: `RaceToWizard` — cascade and GF pin

**Description:** Add client-side cascade logic in `liga-common`: initial state, load from server map (infer `gfPinned`), and `applyEdit` implementing the spec cascade tables including `wb-1` → losers sync and GF pin behavior.

**Acceptance criteria:**
- [ ] `initialState(8)` pre-fills all scope keys to 7 with `lb-*` synced from `wb-1` and `gf` = `wb-N`
- [ ] Editing `wb-1` cascades winners, sets `lb-1` (which cascades losers), updates unpinned `gf`
- [ ] Editing `lb-k` cascades only losers section
- [ ] Editing `gf` pins GF; subsequent `wb-*` edits do not change `gf`
- [ ] `loadState` infers pinned when `gf ≠ wb-N`

**Verification:**
- [ ] `sbt --client "ligaCommonJVM/testOnly *RaceToWizard*"` passes

**Dependencies:** Task 1

**Files likely touched:**
- `liga-common/src/main/scala/ph/samson/atbp/liga/bracket/RaceToWizard.scala` (new)
- `liga-common/src/test/scala/ph/samson/atbp/liga/bracket/RaceToWizardSpec.scala` (new)

**Estimated scope:** S (2 files)

---

### Checkpoint: Foundation

- [ ] `sbt --client "ligaCommonJVM/test"` passes
- [ ] No existing modules broken (new code only so far)
- [ ] Human review before server migration

---

### Phase 2: Server Domain

#### Task 3: State, events, and replay

**Description:** Migrate the tournament domain from integer rounds to scope keys: rename `roundRaceTo` → `raceToByScope`, replace `RoundRaceToSet` with `RaceToSet(scope, raceTo)`, update replay and event log filename, and wire `EventCodec`.

**Acceptance criteria:**
- [ ] `TournamentState` uses `raceToByScope: Map[String, Int]`
- [ ] `RaceToSetPayload(scope: String, raceTo: Int)` replaces `RoundRaceToSetPayload`
- [ ] `TournamentEvent.RaceToSet` with `@jsonHint("RaceToSet")` round-trips JSON
- [ ] `EventLog` writes `race-to` filename (not `round-race-to`)
- [ ] `Replay` applies `RaceToSet` events to `raceToByScope`
- [ ] `MissingRaceToError` carries `scope: String` instead of `round: Int`

**Verification:**
- [ ] `sbt --client "liga/testOnly *EventCodec*"` passes
- [ ] Domain compiles: `sbt --client compile`

**Dependencies:** Task 1

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/model/Types.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/events/TournamentEvent.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/EventCodec.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/EventLog.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/Replay.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/MatchLifecycle.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/tournament/EventCodecSpec.scala`

**Estimated scope:** M (7 files)

---

#### Task 4: Resolve, phase gating, and seed

**Description:** Wire scope-based resolution and validation through the tournament lifecycle: `resolveRaceTo` via `RaceToScopes.keyForMatch`, `raceToComplete` via `RaceToScopes.requiredKeys`, and seed/race-to save emitting one `RaceToSet` per scope.

**Acceptance criteria:**
- [ ] `resolveRaceTo("lb-2-1", …)` and `resolveRaceTo("wb-2-1", …)` can return different values
- [ ] `resolveRaceTo("gf-1", …)` uses `gf` scope, independent of `wb-N`
- [ ] `TournamentPhase.raceToComplete` requires every `requiredKeys(playerCount)` scope
- [ ] `Seed` validates all required scopes and emits sorted `RaceToSet` events
- [ ] `Tournament.setRoundRaceTo` (or renamed equivalent) validates scope keys

**Verification:**
- [ ] `sbt --client "liga/testOnly *Seed*"` passes
- [ ] Add/update focused tests for independent wb/lb/gf resolution

**Dependencies:** Task 3

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/MatchLifecycle.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/TournamentPhase.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/Seed.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/tournament/Tournament.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/tournament/SeedSpec.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/tournament/TournamentSpec.scala`

**Estimated scope:** M (6 files)

---

#### Task 5: Remove `BracketRounds`, update domain tests

**Description:** Delete JVM `BracketRounds` and migrate callers to `RaceToScopes` from `liga-common`. Update or remove `BracketRoundsSpec`; fix remaining domain test helpers (`ReplaySpec` inline events, `ReplayFixtureWriter`, `PeriodEmissionSpec`).

**Acceptance criteria:**
- [ ] `BracketRounds.scala` deleted; no imports remain
- [ ] `BracketRoundsSpec` replaced by or merged into `RaceToScopesSpec` coverage
- [ ] In-test event builders use `RaceToSet` with scope payloads

**Verification:**
- [ ] `sbt --client "liga/testOnly *Replay*"` passes (may still fail on fixtures until Task 6)

**Dependencies:** Task 4

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/bracket/BracketRounds.scala` (delete)
- `liga/src/test/scala/ph/samson/atbp/liga/bracket/BracketRoundsSpec.scala` (delete)
- `liga/src/test/scala/ph/samson/atbp/liga/tournament/ReplaySpec.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/tournament/ReplayFixtureWriter.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/tournament/PeriodEmissionSpec.scala`

**Estimated scope:** M (5 files)

---

### Checkpoint: Server Domain

- [ ] `sbt --client "liga/testOnly *Seed*"`, `*EventCodec*`, `*Tournament*"` pass
- [ ] `roundRaceTo` / `RoundRaceToSet` gone from `liga/` main sources
- [ ] Human review before API layer

---

### Phase 3: API Contract

#### Task 6: HTTP routes and JSON responses

**Description:** Rename API fields and request bodies from `roundRaceTo` to `raceToByScope`. Update `DirectorRoutes`, `ApiJson`, `ServeContext`, and all serve-layer tests.

**Acceptance criteria:**
- [ ] `RaceToRequest` and `SeedRequest` accept `raceToByScope: Map[String, Int]`
- [ ] Tournament API responses expose `raceToByScope` (no `roundRaceTo`)
- [ ] POST `/api/tournament/race-to` and seed endpoint accept scope-keyed JSON
- [ ] `ApiClientContractSpec`, `ReadApiSpec`, `WriteApiSpec`, `BindConfigSpec`, `ServeCheckpointSpec` updated

**Verification:**
- [ ] `sbt --client "liga/testOnly *Api*"` passes
- [ ] `sbt --client "liga/testOnly *ReadApi*"` passes
- [ ] `sbt --client "liga/testOnly *WriteApi*"` passes

**Dependencies:** Task 4

**Files likely touched:**
- `liga/src/main/scala/ph/samson/atbp/liga/serve/DirectorRoutes.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/serve/ApiJson.scala`
- `liga/src/main/scala/ph/samson/atbp/liga/serve/ServeContext.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/js/ApiClientContractSpec.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/serve/ReadApiSpec.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/serve/WriteApiSpec.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/serve/BindConfigSpec.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/serve/ServeCheckpointSpec.scala`

**Estimated scope:** M (8 files)

---

#### Task 7: JS API models and client

**Description:** Mirror the server API shape in `liga-js`: `TournamentResponse.raceToByScope`, `SeedRequest` / `RaceToRequest` payloads, and `Client.setRaceTo` / seed signatures using `Map[String, Int]`.

**Acceptance criteria:**
- [ ] `Models.scala` uses `raceToByScope: Map[String, Int]`
- [ ] `Client.setRaceTo` and seed methods send scope-keyed JSON
- [ ] `DirectorApp` wires `Observer[Map[String, Int]]`
- [ ] `DirectorDefaults` provides scope-based defaults (or delegates to `RaceToWizard.initialState`)
- [ ] `MatchPanel` reads race-to from scope, not integer round

**Verification:**
- [ ] `sbt --client "ligaJs/compile"` succeeds

**Dependencies:** Task 6

**Files likely touched:**
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/api/Models.scala`
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/api/Client.scala`
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/director/DirectorApp.scala`
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/director/DirectorDefaults.scala`
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/director/MatchPanel.scala`

**Estimated scope:** M (5 files)

---

### Checkpoint: API Contract

- [ ] Server and JS compile cleanly
- [ ] API contract tests pass
- [ ] Human review before fixture rewrite

---

### Phase 4: Fixtures and Integration

#### Task 8: Rewrite replay fixture JSON

**Description:** Rename and rewrite all tournament fixture event files: `*-round-race-to.json` → `*-race-to.json` with `RaceToSet` scope payloads. Adjust seq numbers for additional scope events (8 events for 8-player, not 4).

**Acceptance criteria:**
- [ ] All 12 fixture files under `liga/src/test/resources/tournaments/` rewritten
- [ ] Each 8-player fixture has 8 `RaceToSet` events covering all scope keys
- [ ] Downstream event seq numbers in each fixture directory remain consistent

**Verification:**
- [ ] `sbt --client "liga/testOnly *Replay*"` passes

**Dependencies:** Task 5

**Files likely touched:**
- `liga/src/test/resources/tournaments/eight-player-complete/*.json` (4 race-to files + seq shifts)
- `liga/src/test/resources/tournaments/eight-player-partial/*.json`
- `liga/src/test/resources/tournaments/eight-player-seeded/*.json`

**Estimated scope:** M (12+ JSON files, seq renumbering in sibling files)

---

#### Task 9: End-to-end and integration test sweep

**Description:** Update `EndToEndSpec` and any remaining tests to use `raceToByScope` and `RaceToScopes.requiredKeys`. Add the spec's differentiated 8-player scenario as a focused test if not covered elsewhere.

**Acceptance criteria:**
- [ ] `EndToEndSpec.raceToBody` builds scope-keyed JSON
- [ ] Eight- and sixteen-player E2E flows complete via HTTP
- [ ] Scenario test: `lb-1` → 5, `wb-2` → 5, `gf` → 9 produces expected independent values

**Verification:**
- [ ] `sbt --client "liga/testOnly *EndToEnd*"` passes
- [ ] `sbt --client "liga/test"` passes

**Dependencies:** Task 8, Task 6

**Files likely touched:**
- `liga/src/test/scala/ph/samson/atbp/liga/EndToEndSpec.scala`
- Any remaining test files referencing `roundRaceTo`

**Estimated scope:** S–M (2–4 files)

---

### Checkpoint: Integration

- [ ] Full `liga/test` green
- [ ] Replay fixtures replay to correct differentiated state
- [ ] Human review before UI work

---

### Phase 5: Director Wizard UI

#### Task 10: Section-grouped wizard with cascade

**Description:** Rewrite `WizardView.raceToStep` to show Winners / Losers / Grand Final sections. Delegate input changes to `RaceToWizard.applyEdit`; load server state via `RaceToWizard.loadState`. Remove `WizardRounds` usage.

**Acceptance criteria:**
- [ ] UI shows three sections at all bracket sizes (8–64)
- [ ] Labels from `RaceToScopes.scopeLabel` (not `raceToRoundLabel`)
- [ ] Cascade behavior matches spec tables on every edit
- [ ] GF helper text: "usually longer than finals — set explicitly."
- [ ] Save sends full `raceToByScope` map

**Verification:**
- [ ] Manual: load wizard for 8-player tournament, verify section layout and cascade
- [ ] `sbt --client "ligaJs/test"` passes (add wizard tests if feasible)

**Dependencies:** Task 2, Task 7

**Files likely touched:**
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/director/WizardView.scala`
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/director/BracketLayout.scala` (drop `raceToRoundLabel`, scope-based `defaultRaceTo`)
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/director/WizardRounds.scala` (delete)

**Estimated scope:** M (3 files)

---

### Checkpoint: Complete

- [ ] `sbt --client "ligaCommonJVM/test"`, `liga/test`, `ligaJs/test` all pass
- [ ] `sbt --client fixup && git status` clean before commit
- [ ] All SPEC.md success criteria checked off
- [ ] Ready for human review / PR

---

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Fixture seq renumbering breaks replay | High | Rewrite one fixture dir at a time; run `ReplaySpec` after each |
| Cascade edge cases (GF pin + wb-1 sync) | Med | Comprehensive `RaceToWizardSpec` before UI wiring |
| Missed `roundRaceTo` reference | Med | Grep after each phase; full test suite at checkpoints |
| JS/JVM map key ordering in JSON | Low | Use same zio-json codecs; verify in `ApiClientContractSpec` |
| `liga-common` bracket size math diverges from topology | Med | Cross-check `requiredKeys` counts against `BracketRoundsSpec` values expanded to scopes |

## Open Questions

None — all resolved in SPEC.md.

## Parallelization Opportunities

| Can run in parallel | Must be sequential |
|---------------------|-------------------|
| Task 1 specs while reviewing Task 2 design | Task 3 → 4 → 6 (domain before API) |
| Task 8 fixture rewrite after Task 5 domain tests compile | Task 10 after Task 2 + 7 |
| Documentation updates (optional) | Event rename touches replay + API together |

## Success Criteria Mapping

| SPEC criterion | Task(s) |
|----------------|---------|
| `raceToByScope` in state, API, requests | 3, 6, 7 |
| `RaceToScopes` + `RaceToWizard` in `liga-common` | 1, 2 |
| Independent wb/lb/gf resolution | 4 |
| `raceToComplete` on scope keys | 4 |
| `RaceToSet` replay + fixtures | 3, 8 |
| Section-grouped wizard + cascade | 10 |
| Full test suite green | All checkpoints |
