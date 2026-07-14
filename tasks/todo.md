# Paste roster review remediation — TODO

See [tasks/plan.md](plan.md) for acceptance criteria and verification steps.

## Phase 1: Paste safety

- [x] **Task 1:** Guard empty apply + prefill textarea on resume (`WizardView.scala`)

## Phase 2: RosterPaste hardening

- [x] **Task 2:** `Tuning.Default.initRating`, `parsePaste("")` test, "players in roster" copy

## Checkpoint (after Tasks 1–2)

- [x] `sbt --client "liga/testOnly *RosterPaste*"`
- [x] `sbt --client "liga-js/compile"`
- [ ] Manual smoke: paste → apply → save

## Phase 3: Seed-order parity

- [x] **Task 3:** `RatingOrder` helper + RD-aware `resolveRoster` + WizardView map + tests

## Phase 4: Director guardrails

- [x] **Task 4:** Unsaved-roster hint + oversize paste warning

## Checkpoint (complete)

- [x] `sbt --client "liga/test"`
- [x] `sbt --client fixup` until `git status` clean
- [ ] Manual define-step flow (spec success criteria 1–5)
- [ ] Re-review / merge
