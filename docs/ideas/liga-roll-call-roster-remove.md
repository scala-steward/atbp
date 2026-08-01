# Roll-call roster remove (Define step)

## Problem statement

How might we let a Liga director drop no-shows from the pre-lock roster during
roll call, with easy undo, without rewriting the signup paste and without a
separate roll-call screen?

## Recommended direction

Add soft-remove on the existing Define roster list. Each rated row can be marked
removed and restored. Removed players stay in place (strike + opacity,
rating/guest still visible) so the director can see who was dropped and unmark
mistakes. Removals never rewrite the signup paste.

Paste is a restart, not a parallel source of truth. Edit paste, then Apply:
replace the roster from the new paste and clear all remove marks. Save and Lock
always persist the current roster list (non-removed names only). They ignore a
dirty paste box until Apply.

This matches the real job: trim a mostly-correct signup at the door. It avoids
opt-in check-in friction and avoids teaching a second "which list wins?" rule.
It does change today's Lock behavior, which re-applies paste before locking.
That change is intentional.

Related intent: `docs/intent/liga-roll-call-roster-remove.md`.

## Key assumptions to validate

- [ ] Typical roll call is a few absences, so per-row remove beats
      default-unchecked check-in. Confirm on a real tournament night.
- [ ] Soft-removed rows (visible, strike + opacity) are clearer for undo than
      hard-delete from the list. Watch one wrong remove.
- [ ] Directors accept that Apply wipes remove marks (paste = restart). Nobody
      expects removals to survive a re-paste.
- [ ] Locking from the roster list (not paste) matches mental model once Lock
      no longer auto-applies paste. Dry-run Save vs Lock with a dirty paste box.

## MVP scope

- On Define roster rows: mark removed / restore
- Removed rows stay visible (strike + opacity), excluded from player count and
  from Save/Lock payload
- Apply paste: parse, replace roster names, clear all remove marks; paste
  formatting cleanup as today
- Save roster / Lock roster: operate only on non-removed roster list names
  (same APIs as today)
- Count hints / Lock enablement use non-removed count
- Below-min after removals: existing Lock disable + `lockRosterHint` only; do
  not block individual removes
- No separate roll-call screen; no paste rewriting; no add-player from this UI;
  no post-lock edits

Save commits the trimmed list the same way Lock does for players. Soft-remove
is UI-only until then. After Save, restore means re-paste the signup and Apply.
That is fine: Save remains useful as a Defining checkpoint (step away, refresh)
without inventing absent-mark persistence the event log does not have.

## Not doing (and why)

- Opt-in check-in (default unchecked). Taxes the common "almost everyone showed"
  case; wrong default for no-show trimming.
- Dirty-paste Lock semantics / dual source of truth. Apply restarts; Save/Lock
  ignore the textarea.
- Rewriting or syncing the paste when removing. Paste stays the signup artifact.
- Hard-delete rows with only undo-stack restore. Too easy to lose track of who
  was dropped during verbal roll call.
- Persisting remove marks across reload without Save. Keep the overlay local
  until commit.
- Bulk absent tools / keyboard roll-call mode. Nice later; not needed to test
  the core assumption.
- Post-lock roster changes. Already out of product scope.
- Blocking removes that would go below minimum. Rely on Lock disable + hints.

## Open questions

None blocking. Visual, Save semantics, and below-min behavior decided above.
