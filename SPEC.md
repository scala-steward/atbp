# Spec: Roster UX remediation (Thermo Nuclear findings)

Follow-up on the `roster-ux` branch after a Thermo Nuclear Review audit.
Parent intent: [docs/intent/paste-roster.md](docs/intent/paste-roster.md).

## Assumptions

1. **Scope is remediation only** — close M1/M2 and L1–L3 from the audit; no new
   paste-roster features beyond what intent already describes.
2. **M2 fix: Lock auto-saves** — clicking Lock persists the roster (same as
   Save) then locks, in one action. No separate Save required before Lock.
   If the textarea has unapplied paste, Lock applies it first so the locked
   roster matches what the director typed.
3. **M1 fix: single comparator** — `Seeding` delegates to `RatingOrder` in
   `liga-common`; no third shared module.
4. **L2 fix: soft dirty-paste hint** — when textarea content differs from the
   applied roster, show a non-blocking hint in the paste area (preview not
   updated yet). Suppress conflicting lock/count hints in the roster summary.
   Lock still auto-applies paste on click; the hint is guidance only.
5. **Still UI-only** — no changes to `DirectorRoutes`, `Tournament`, or lock/save
   API contracts (per paste-roster intent).

→ Correct these assumptions before implementation if any are wrong.

## Objective

Tournament directors on the define step must trust that (a) the roster preview
order matches real seeding order, and (b) Lock always persists then locks the
roster they intend — including unapplied textarea content — without a separate
Save step.

### User

Same as paste-roster intent: director during the defining wizard step.

### Findings addressed

| ID | Priority | Issue | Remediation |
|----|----------|-------|-------------|
| M1 | Medium | `RatingOrder.compareBestFirst` duplicates `Seeding.compareRatings` | `Seeding.seedOrder` calls `RatingOrder.sortBestFirst`; delete private duplicate |
| M2 | Medium | Lock enabled while local roster ≠ saved server roster | Lock runs apply-paste (if needed) → `setPlayers` → `lockPlayers` sequentially |
| L1 | Low | `parsePaste` splits on `\n` only; bare `\r` collapses | Split on `\r\n`, `\r`, or `\n` |
| L2 | Low | Oversize hint (textarea) vs lock hint (applied names) can contradict | Soft apply-paste hint in paste area when dirty; suppress summary lock hints |
| L3 | Low | Guest flag recomputed via second `periodByName.contains` | Track `guest` during initial name→rating resolution |

### Success criteria

- [ ] **M1** — `Seeding.seedOrder` is implemented as `RatingOrder.sortBestFirst`
  (or equivalent delegation). No duplicate comparator logic remains in
  `Seeding.scala`. Existing `BracketSpec` seed-order tests still pass.
- [ ] **M1** — `RatingOrderSpec` in `liga-common` covers comparator tie-breaks
  (rating desc, RD asc, name asc) on a non-trivial fixture.
- [ ] **M2** — Lock click: if `parsePaste(pasteText)` ≠ `names`, apply paste to
  `names` first; then if roster ≠ server-saved, call `setPlayers`; on success,
  call `lockPlayers`. Single busy state for the whole chain.
- [ ] **M2** — Lock button stays enabled when count is 8–64 (not gated on
  saved state). `saveBeforeLockHint` removed or replaced with neutral copy
  (e.g. “Lock saves your roster”).
- [ ] **M2** — If `setPlayers` fails, Lock does not proceed; error surfaces
  via existing `DirectorApp` status handling.
- [ ] **L1** — `parsePaste("A\rB\r")` → `List("A", "B")`; existing tests pass.
- [ ] **L2** — When `parsePaste(pasteText) != names` (list equality), paste area
  shows `DirectorGuidance.applyPasteHint` (soft copy). Roster summary omits
  lock/count hints that reference applied `names` only.
- [ ] **L2** — Oversize warning (`> 64` in textarea) may show alongside the
  dirty hint; both live in the paste area, not the summary.
- [ ] **L3** — `resolveRoster` sets guest in one pass; behavior unchanged per
  `RosterPasteSpec`.
- [ ] Full `liga-common`, `liga`, and `liga-js` compile and test; `sbt --client
  fixup` clean per `AGENTS.md`.

## Tech Stack

- Scala 3, sbt multi-module (`liga-common`, `liga`, `liga-js`)
- ZIO Test (`liga-common` JVM + `liga` modules)
- Laminar frontend (`liga-js`)
- No new dependencies

## Commands

```bash
# Compile affected modules
sbt --client compile

# Unit tests — rating order (liga-common) + roster + bracket
sbt --client "liga-commonJVM/testOnly *RatingOrder*"
sbt --client "liga/testOnly *RosterPaste*"
sbt --client "liga/testOnly *Bracket*"

# Full suites
sbt --client "liga-commonJVM/test"
sbt --client "liga/test"

# Frontend compile
sbt --client "liga-js/compile"

# Pre-commit (required)
git add -A   # include new/changed sources
sbt --client fixup && git status
```

## Project Structure

```
liga-common/src/main/scala/ph/samson/atbp/liga/roster/
  RatingOrder.scala          # canonical best-first comparator (unchanged API)
  RosterPaste.scala          # parsePaste + resolveRoster fixes (L1, L3)

liga-common/src/test/scala/ph/samson/atbp/liga/roster/
  RatingOrderSpec.scala      # M1 comparator unit tests (new — first liga-common tests)

liga/src/main/scala/ph/samson/atbp/liga/bracket/
  Seeding.scala              # delegate to RatingOrder (M1)

liga-js/src/main/scala/ph/samson/atbp/liga/js/director/
  WizardView.scala           # Lock passes roster; hint coherence (M2, L2)
  DirectorApp.scala          # save-then-lock sequence (M2)
  DirectorGuidance.scala     # applyPasteHint (L2); reword saveBeforeLockHint (M2)

liga/src/test/scala/ph/samson/atbp/liga/roster/
  RosterPasteSpec.scala      # L1 bare-\r test; L3 unchanged expectations

liga/src/test/scala/ph/samson/atbp/liga/bracket/
  BracketSpec.scala          # unchanged expectations; verifies M1 delegation

docs/intent/paste-roster.md  # reference only; no edit unless intent shifts
```

## Code Style

Match existing `liga-common` / `liga-js` patterns: small objects, explicit names,
minimal comments (business rules only).

**M1 — delegation (target shape):**

```scala
// liga/.../bracket/Seeding.scala
import ph.samson.atbp.liga.roster.RatingOrder

def seedOrder(players: List[PlayerRating]): List[PlayerRating] =
  RatingOrder.sortBestFirst(players)
```

**L3 — single-pass guest (target shape):**

```scala
names.map { name =>
  periodByName.get(name) match {
    case Some(rating) => (rating, guest = false)
    case None         => (syntheticGuestRating(name, tuning), guest = true)
  }
}
```

**M2 — save-then-lock (target shape in `DirectorApp`):**

```scala
def saveAndLock(players: List[Player]): Future[TournamentResponse] =
  client.setPlayers(players).flatMap { saved =>
    if (saved.players.map(_.name).toSet == players.map(_.name).toSet)
      client.lockPlayers()
  else
      Future.failed(new IllegalStateException("save did not stick"))
  }
```

`WizardView` Lock click: resolve roster from textarea + `names`, then invoke
`onSaveAndLock` with `List[Player]` (replaces bare `onLock: Observer[Unit]`).
Lock `disabled` only on `busy` and count 8–64 (same as today minus saved check).

**L2 — dirty-paste hint (`DirectorGuidance`):**

```scala
val applyPasteHint: String =
  "Apply paste to update the preview — or Lock will use your pasted list."
```

Shown in `.roster-paste` when `parsePaste(pasteText) != names`. Roster summary
lock hints (`lockRosterHint`) render only when paste is clean (lists match).

## Testing Strategy

| Concern | Level | Location |
|---------|-------|----------|
| Parse edge cases (L1) | Unit | `RosterPasteSpec` |
| Guest resolution (L3) | Unit | `RosterPasteSpec` (existing cases) |
| Comparator tie-breaks (M1) | Unit | `liga-common/.../RatingOrderSpec` |
| Seeding delegation (M1) | Unit | `BracketSpec` (regression via `Seeding.seedOrder`) |
| Save-then-lock (M2) | Manual | Paste roster, skip Save, Lock → server has pasted list |
| Save-then-lock failure (M2) | Manual | Invalid roster → Lock shows error, phase stays Defining |
| Hint coherence (L2) | Manual | Dirty textarea: soft hint in paste area; no conflicting summary hints |

Framework: ZIO Test. `RatingOrderSpec` lives beside `RatingOrder` in
`liga-common` (JVM only — comparator has no JS-specific behavior). First test
source in that module: enable `ZTestFramework` on `ligaCommon.jvm` in
`build.sbt` if not already present. `BracketSpec` in `liga` confirms
`Seeding.seedOrder` still matches expected bracket ordering after delegation.

## Boundaries

### Always

- Run `sbt --client fixup` and leave working tree clean before commit
  (`AGENTS.md`).
- Keep paste-roster intent constraints: exact name match, same REST endpoints
  (`setPlayers` then `lockPlayers` — no new server routes).
- Preserve `RatingOrder` as the single source of truth for best-first ordering.

### Ask first

- Changing lock/save API or server-side validation.
- Confirmation dialogs before Lock.
- Moving `RatingOrder` out of `liga-common` or adding a new module.

### Never

- Reintroduce checkbox picker or one-by-one guest entry.
- Fuzzy or case-insensitive name matching.
- Duplicate comparator logic after M1 lands.

## Implementation order (suggested)

1. **M1** — `RatingOrderSpec` + `Seeding` → `RatingOrder` delegation (low risk).
2. **L1, L3** — `RosterPaste` parse/resolve (pure, test-driven).
3. **M2, L2** — `WizardView` + `DirectorApp` save-then-lock and hint logic
   (UI, manual verify).

---

**Status:** Approved — all open questions resolved. Ready for PLAN/TASKS/IMPLEMENT.
