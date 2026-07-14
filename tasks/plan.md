# Implementation Plan: Paste roster review remediation

## Overview

Address all findings from the five-axis review of the paste-first roster feature
(`roster-ux` branch). The original feature is spec-aligned; this pass fixes the
empty-apply footgun, aligns preview sort with bracket seeding, removes magic
constants, and polishes director copy and guardrails. No backend or API changes.

**Spec reference:** [docs/specs/paste-roster.md](../docs/specs/paste-roster.md)

## Review findings mapped to work

| Finding | Severity | Task |
|---------|----------|------|
| Empty Apply paste wipes resumed roster | Important | 1 |
| `GuestDisplayRating` hardcoded vs `Tuning` | Consider | 2 |
| Missing `parsePaste("")` test | Nit | 2 |
| "players selected" copy | Nit | 2 |
| Preview sort ignores RD tie-break | Consider | 3 |
| Lock uses saved server state, not local paste | Consider | 4 |
| No client cap/warning on huge paste | Consider | 4 |
| `tasks/` scratch files in branch | Consider | 5 (this plan replaces them) |

## Architecture decisions

1. **Empty apply guard + resume prefill (Task 1)** — Fix in `WizardView` first.
   Disable Apply when `pasteText.trim.isEmpty`. On defining-step mount, initialize
   `pasteText` from `tournament.players.map(_.name).mkString("\n")` so resume
   matches the visible roster and re-apply is idempotent.

2. **Tuning reference (Task 2)** — Replace `GuestDisplayRating = 1500.0` with
   `Tuning.Default.initRating` in `liga-common`. Keep the `GuestDisplayRating`
   val as an alias if tests/UI import it, or migrate call sites to the tuning
   constant. Guests still display-only; seed behavior unchanged.

3. **Seed-order parity (Task 3)** — Preview sort should match
   `Seeding.compareRatings` (rating desc → lower RD wins → name asc). Add a small
   `RatingOrder` helper in `liga-common` (same compare logic as
   `liga/bracket/Seeding.scala`). Change `resolveRoster` to accept
   `Map[String, PlayerRating]` (liga-common model), build a `PlayerRating` per
   name (period hit or guest with `initRating` + `maxDeviation`), sort via
   `RatingOrder`, then map to `RosterEntry`. Update `WizardView` to pass full
   period ratings from `leaderboard.ratings`. **Out of scope:** refactoring
   `Seeding` to call `RatingOrder` (optional follow-up).

4. **Unsaved + oversize hints (Task 4)** — Reactive hint when local `names` differ
   from `tournament.players` names: remind director to Save before Lock. Reuse
   `DirectorGuidance.lockRosterHint` for count >64; add a one-line hint when
   parsed count >64 before apply (or on roster summary).

5. **Task files** — Keep `tasks/plan.md` / `tasks/todo.md` as the working plan
   for this remediation. Do not commit further ephemeral notes; delete or empty
   `tasks/todo.md` checkboxes when done.

## Dependency graph

```
Task 2 (Tuning + tests + copy)     Task 1 (paste safety UI)
         │                                    │
         └──────────────┬─────────────────────┘
                        ▼
              Task 3 (RD-aware sort)
                        │
                        ▼
              Task 4 (dirty + oversize hints)
                        │
                        ▼
              Checkpoint + fixup
```

Task 1 is independent and should land first (highest user impact). Task 3 depends
on Task 2 only if `GuestDisplayRating` / guest RD values change together; safe
order is 1 → 2 → 3 → 4.

---

## Task list

### Phase 1: Paste safety (Important)

#### Task 1: Guard empty apply and prefill textarea on resume

**Description:** Prevent accidental roster wipe when Apply is clicked with an empty
textarea (especially on resume). Pre-populate the paste box from saved player
names so the UI reflects server state.

**Acceptance criteria:**
- [ ] Apply paste button is disabled when `pasteText.trim.isEmpty`
- [ ] On defining-step mount with saved players, textarea contains one name per line
- [ ] Applying pre-filled text reproduces the same `names` list (idempotent)
- [ ] Empty textarea cannot clear `names` via Apply

**Verification:**
- [ ] Manual: resume defining tournament → textarea shows saved names → roster unchanged
- [ ] Manual: clear textarea → Apply disabled → roster still shows saved names
- [ ] `sbt --client compile` (ligaJS)

**Dependencies:** None

**Files likely touched:**
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/director/WizardView.scala`

**Estimated scope:** Small (1 file)

---

### Phase 2: RosterPaste hardening

#### Task 2: Tuning constant, empty-parse test, summary copy

**Description:** Remove magic `1500.0` drift risk, document empty-paste behavior in
tests, fix leftover checkbox copy.

**Acceptance criteria:**
- [ ] `GuestDisplayRating` (or equivalent) derives from `Tuning.Default.initRating`
- [ ] `RosterPasteSpec` includes `parsePaste("")` → `Nil`
- [ ] Roster summary reads `"N players in roster"` (or equivalent), not "selected"
- [ ] Existing `RosterPasteSpec` tests still pass

**Verification:**
- [ ] `sbt --client "liga/testOnly *RosterPaste*"`

**Dependencies:** None (parallel with Task 1)

**Files likely touched:**
- `liga-common/src/main/scala/ph/samson/atbp/liga/roster/RosterPaste.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/roster/RosterPasteSpec.scala`
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/director/WizardView.scala` (copy only)

**Estimated scope:** Small (2–3 files)

---

### Checkpoint: Safety + constants

- [ ] Tasks 1–2 complete
- [ ] `sbt --client "liga/testOnly *RosterPaste*"`
- [ ] `sbt --client "ligaJS/compile"`
- [ ] Manual smoke: create tournament → paste → apply → save

---

### Phase 3: Preview ↔ seed order parity

#### Task 3: RD-aware roster sort via shared `RatingOrder`

**Description:** Align define-step roster preview order with bracket seeding when
ratings tie (RD, then name).

**Acceptance criteria:**
- [ ] `RatingOrder.sortBestFirst` in `liga-common` matches `Seeding.compareRatings` ordering
- [ ] `resolveRoster` takes `Map[String, PlayerRating]`; guests use `initRating` + `maxDeviation`
- [ ] `WizardView` passes `leaderboard.ratings` map (not rating-only)
- [ ] New spec: two players same rating, different RD → lower RD ranks higher in preview
- [ ] New spec: guest at 1500 vs period player at 1500 sorts like seed (RD breaks tie)

**Verification:**
- [ ] `sbt --client "liga/testOnly *RosterPaste*"`
- [ ] `sbt --client "ligaJS/compile"`

**Dependencies:** Task 2 (guest rating/RD constants)

**Files likely touched:**
- `liga-common/src/main/scala/ph/samson/atbp/liga/roster/RatingOrder.scala` (new)
- `liga-common/src/main/scala/ph/samson/atbp/liga/roster/RosterPaste.scala`
- `liga/src/test/scala/ph/samson/atbp/liga/roster/RosterPasteSpec.scala`
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/director/WizardView.scala`

**Estimated scope:** Medium (4 files)

---

### Phase 4: Director guardrails

#### Task 4: Unsaved roster hint and oversize paste warning

**Description:** Reduce lock-without-save and oversized-paste confusion.

**Acceptance criteria:**
- [ ] When local `names` ≠ saved `tournament.players` names, show hint: save before lock
- [ ] When roster count >64, existing `lockRosterHint` still shown (no regression)
- [ ] Optional: when applied/parsed count >64, summary shows clear oversize message

**Verification:**
- [ ] Manual: paste 12 names, do not save → hint visible → Lock still operates on server state
- [ ] Manual: paste 70 names, apply → oversize hint visible, Lock disabled

**Dependencies:** Task 1 (stable names/paste flow)

**Files likely touched:**
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/director/WizardView.scala`
- `liga-js/src/main/scala/ph/samson/atbp/liga/js/director/DirectorGuidance.scala` (if new copy helpers)

**Estimated scope:** Small (1–2 files)

---

### Checkpoint: Complete

- [ ] All tasks 1–4 done
- [ ] `sbt --client "liga/testOnly *RosterPaste*"`
- [ ] `sbt --client "liga/test"`
- [ ] `sbt --client fixup` loop until `git status` clean
- [ ] Manual define-step flow per spec success criteria 1–5
- [ ] Ready for re-review / merge

---

## Risks and mitigations

| Risk | Impact | Mitigation |
|------|--------|------------|
| `RatingOrder` drifts from `Seeding` | Med | Copy compare logic once; add parity test with fixed fixtures |
| Prefill textarea confuses "must re-apply" | Low | Apply still required to replace; prefill is display-only sync |
| Dirty hint false positives on order-only diff | Low | Compare as sets or normalized lists; document if order ignored |

## Open questions

None blocking — review assumptions stand. Optional follow-up: refactor `Seeding`
to delegate to `RatingOrder` (not required for this remediation).
