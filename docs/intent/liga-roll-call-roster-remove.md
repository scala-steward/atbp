# Intent: roll-call roster remove before save/lock

Confirmed statement of intent (output of an interview-me session). This captures
*what* we want and *why*, not the implementation. The spec is downstream.

## Outcome

During roll call, the director can drop players from the roster list (with undo)
right before Save / Lock Roster — no hand-editing the signup paste.

## User

Liga tournament director running pre-lock roll call.

## Why now

Last chance for no-shows/withdrawals; editing the paste is the slow, error-prone
part.

## Success

- They can mark someone removed, unmark if wrong, then Save/Lock the remaining
  list without touching the paste.

## Constraint

Paste stays unchanged; removals only affect the roster list for that Save/Lock
action.

## Out of scope

- Separate roll-call screen.
- Rewriting the signup paste.
- Adding players from this UI.
- Changing anything after the roster is already locked.
