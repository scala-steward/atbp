# Liga match forfeit (admin walkover)

## Problem Statement

How might we let a Liga TD resolve a match that won’t be played so the waiting
opponent advances and the bracket stays honest — without inventing scores that
pollute period ratings?

## Recommended Direction

Ship an admin-only **forfeit / walkover** on the director MatchPanel. The TD
names who forfeits, types a **reason** (required free text — the confirm
friction), and the match completes with **no result scores**.

Bracket placement uses normal loss advancement (winners → losers), **not**
structural-bye (`isBye`) semantics. Period emission skips forfeited matches;
prior finished matches still count. The player stays eligible for later matches
unless those are forfeited too. Allowed from Ready or Started (score entry is
ephemeral UI until Record result — there is no persisted partial score).

Director and audience bracket views show **forfeit status and the reason**
(extend bye-style `BracketLayout.resultLabel` labeling — not a fake score).

No in-app undo. Recovery: delete the last event file and restart so replay
rebuilds state.

Related intent: `docs/intent/liga-match-forfeit.md`.

## Key Assumptions to Validate

- [ ] Required free-text reason is enough mis-click friction on a live night
      (and not so annoying TDs invent nonsense)
- [ ] Event-file delete + restart is acceptable recovery when a forfeit was wrong
- [ ] Skipping period emission for forfeits leaves post-event period/Glicko
      review looking correct
- [ ] Showing the reason on the audience bracket is desirable (transparency),
      not awkward (ops notes that should stay director-only)
- [ ] TD always knows which side forfeits (no “both absent” case in MVP)

## MVP Scope

- MatchPanel: Forfeit → choose forfeiting player → type reason → submit
- Complete path: `Completed`, no result scores, normal `Advancement` (loser
  drops when applicable)
- Persist reason on the match / event so replay and UIs can show it
- `PeriodEmission` excludes forfeited / no-result matches
- Works on Ready and Started matches
- Director and audience brackets: forfeit label + reason (not a score)
- Append-only event; no undo UI

## Not Doing (and Why)

- **Reuse `isBye`** — bye path skips loser placement; wrong for double-elim
  forfeit
- **Synthetic scores** — pollutes period / Glicko
- **In-app undo** — event-file delete + restart is enough
- **Click-only confirm without reason** — typed reason is the deliberate
  friction + public record
- **Player self-forfeit** — TD-operated only
- **Full-night withdrawal / cascade** — keep per-match focus
- **Automatic timeout forfeits** — ops judgment, not a clock
- **Reason presets** — free text only for MVP
- **Pre-lock no-shows** — roll-call roster remove

## Open Questions

None blocking. Reason field, audience visibility, and Started-match semantics
decided above.
