# Implementation Plan: Liga code-review remediation

## Overview

Harden Liga's tournament director path per [SPEC.md](../SPEC.md): fix localhost write guard, tighten domain validation (roster, handicap, scores), make tournament completion atomic with correct HTTP status codes, and add minor HTTP/UX hardening. Work is remediation-only — no new features.

## Architecture Decisions

| Decision | Rationale |
|----------|-----------|
| **Write period before `TournamentCompleted` event** | Avoids orphaned completed state when `PeriodEmission.write` fails; simpler than append-then-compensate-delete |
| **Map `PeriodEmission.EmissionError` at HTTP boundary** | Message containing `already exists` → 409; other emission/I/O failures → 500 |
| **Keep validation in `Tournament` / `MatchLifecycle`** | Reuse `MatchLifecycle.resolveRaceTo` for handicap and score checks; HTTP stays thin |
| **`DuplicatePlayersError` as `WizardError`** | Consistent with roster validation errors (`InvalidPlayerCountError`, etc.) |
| **Case-sensitive duplicate check via `players.distinct`** | Matches resolved spec decision and existing `Player` equality |
| **Generic 500 body: fixed string** | e.g. `"internal server error"` — log full exception server-side if logging exists; do not expose `getMessage` |
| **Static asset reject list: `..` and `/` in segment** | Single path segment from zio-http; no classpath traversal |

## Dependency Graph

```
BindConfig.isLocalDirector          StaticAssets sanitization
         │                                    │
         └──────────────┬─────────────────────┘
                        ▼
              Tournament domain validation
         ┌──────────────┼──────────────┐
         ▼              ▼              ▼
   setPlayers      applyHandicap   recordResult
   (duplicates)    (bounds)        (race-to scores)
         │              │              │
         └──────────────┼──────────────┘
                        ▼
           ServeContext.completeTournament
           (write period → append event)
                        │
         ┌──────────────┼──────────────┐
         ▼              ▼              ▼
   HTTP 409/500    DirectorGuidance   TournamentPhase comment
   + generic 500s   error mapping
```

**Order rationale:** Security fixes first (fail fast). Domain rules before completion reorder (completion tests assume valid match data). HTTP polish last so new error strings are stable before mapping.

## Task List

### Phase 1: Security (Tasks 1–2)

- [ ] **Task 1:** Fix `isLocalDirector` for missing `remoteAddress`
- [ ] **Task 2:** Sanitize static JS asset paths

**Checkpoint A:** `BindConfigSpec` + `StaticAssetsSpec` pass; LAN write block unchanged.

### Phase 2: Domain validation (Tasks 3–5)

- [ ] **Task 3:** Reject duplicate roster players
- [ ] **Task 4:** Enforce handicap bounds server-side
- [ ] **Task 5:** Enforce race-to score rules on result

**Checkpoint B:** `TournamentSpec` + `WriteApiSpec` pass for new rejection cases; `EndToEndSpec` still passes.

### Phase 3: Completion atomicity (Task 6)

- [ ] **Task 6:** Reorder `completeTournament`; map 409/500 at HTTP

**Checkpoint C:** Integration test proves no `TournamentCompleted` event when period write fails; happy-path E2E unchanged.

### Phase 4: UX polish (Tasks 7–8)

- [ ] **Task 7:** Generic 500 responses (director + read API)
- [ ] **Task 8:** `DirectorGuidance` mappings + `TournamentPhase` comment

**Checkpoint D (complete):** Full `liga/test`, `fixup`, clean `git status`.

## Risks and Mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| Reordering complete breaks E2E | High | Run `EndToEndSpec` at Checkpoint C |
| Score validation rejects bye/auto-advance paths | Med | Byes use `Advancement` internally with 1–0; only `recordResult` path validated — confirm no director POST for bye matches |
| `EmissionError` message parsing for 409 | Low | Prefer typed error (`PeriodFileExists`) over string match; or match stable message from `PeriodEmission.write` |
| New validation breaks existing fixture tournaments | Med | Run full `liga/test` at each checkpoint |

## Parallelization

| Safe in parallel | Must be sequential |
|------------------|-------------------|
| Tasks 1 and 2 | Tasks 3→5 before Task 6 (recommended) |
| Tasks 7 and 8 after Task 6 | Task 6 after domain validation stable |

## Verification Commands

```bash
sbt --client "liga/testOnly *BindConfigSpec"
sbt --client "liga/testOnly *StaticAssetsSpec"
sbt --client "liga/testOnly *TournamentSpec"
sbt --client "liga/testOnly *WriteApiSpec"
sbt --client "liga/testOnly *EndToEndSpec"
sbt --client "liga/test"
sbt --client fixup && git status
```

## Reference Files

| Area | Primary files |
|------|---------------|
| Localhost guard | `liga/.../serve/BindConfig.scala`, `BindConfigSpec.scala` |
| Domain | `liga/.../tournament/Tournament.scala`, `TournamentSpec.scala` |
| Complete flow | `liga/.../serve/ServeContext.scala`, `DirectorRoutes.scala` |
| Period write | `liga/.../tournament/PeriodEmission.scala` |
| HTTP tests | `liga/.../serve/WriteApiSpec.scala`, `ServeCheckpointSpec.scala` |
| Assets | `liga/.../serve/StaticAssets.scala`, `StaticAssetsSpec.scala` |
| UI errors | `liga-js/.../director/DirectorGuidance.scala` |
| Phase comment | `liga/.../tournament/TournamentPhase.scala` |

---

*Plan derived from SPEC.md (approved 2026-07-12). Review before `/build`.*
