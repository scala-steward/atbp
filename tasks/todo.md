# Paste roster review remediation — TODO

See [tasks/plan.md](plan.md) for acceptance criteria and verification steps.

## Phase 1: Paste safety

- [x] **Task 1:** Guard empty apply + prefill textarea on resume (`WizardView.scala`)

## Phase 2: RosterPaste hardening

- [ ] **Task 2:** `Tuning.Default.initRating`, `parsePaste("")` test, "players in roster" copy

## Checkpoint (after Tasks 1–2)

- [ ] `sbt --client "liga/testOnly *RosterPaste*"`
- [ ] `sbt --client "ligaJS/compile"`
- [ ] Manual smoke: paste → apply → save

## Phase 3: Seed-order parity

- [ ] **Task 3:** `RatingOrder` helper + RD-aware `resolveRoster` + WizardView map + tests

## Phase 4: Director guardrails

- [ ] **Task 4:** Unsaved-roster hint + oversize paste warning

## Checkpoint (complete)

- [ ] `sbt --client "liga/test"`
- [ ] `sbt --client fixup` until `git status` clean
- [ ] Manual define-step flow (spec success criteria 1–5)
- [ ] Re-review / merge
