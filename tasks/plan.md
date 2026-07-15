# Plan: Roster UX remediation

From [SPEC.md](../SPEC.md). Branch: `roster-ux`.

## Components

| Layer | Change | Depends on |
|-------|--------|------------|
| `liga-common` | `RatingOrderSpec`, `RosterPaste` L1/L3 | — |
| `liga` | `Seeding` → `RatingOrder` | `liga-common` |
| `liga-js` | save-then-lock, hints, `WizardView` | `liga-common` |
| `build.sbt` | `ZTestFramework` on `ligaCommon.jvm` | — |

## Order

1. **M1** — Enable JVM tests on `liga-common`; add `RatingOrderSpec`; delegate `Seeding`.
2. **L1/L3** — Fix `parsePaste` split; single-pass guest in `resolveRoster`; extend `RosterPasteSpec`.
3. **M2/L2** — `DirectorGuidance` hints; `DirectorApp.saveAndLock`; `WizardView` lock flow + hint coherence.

## Risks

- **Stale tournament in lock handler** — always `setPlayers` then `lockPlayers` to avoid saved-state races.
- **Lock disabled count** — derive from `parsePaste(pasteText)` since Lock auto-applies textarea.

## Verification

```bash
sbt --client "ligaCommonJVM/testOnly *RatingOrder*"
sbt --client "liga/testOnly *RosterPaste*"
sbt --client "liga/testOnly *Bracket*"
sbt --client "liga-js/compile"
sbt --client fixup && git status
```
