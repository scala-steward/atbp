# Intent: paste roster on define

Confirmed statement of intent (output of an interview-me session). This captures
*what* we want and *why*, not the implementation. The spec is downstream.

## Outcome

Director pastes a newline-separated signup list and gets a rated roster they can
verify at a glance.

## User

Tournament director during the defining step of the director wizard.

## Why now

Signup already produces one mixed list of returning players and guests. Splitting
that list into period checkboxes vs one-by-one guest entry is busywork.

## Success

- Paste replaces the current local selection with the pasted names.
- Exact string match against the period leaderboard preserves period ratings.
- Non-matching names are guests (default rating at seed, as today).
- Resulting roster displays ordered by descending rating, with guests highlighted.
- Wrong paste → paste a corrected list again.

## Constraint

UI-only on the defining step. Same save/lock API. No fuzzy or case-insensitive
matching.

## Out of scope

- Period checkboxes and one-by-one guest field (removed).
- CSV / file upload.
- Merge-into-selection paste behavior.
- Fuzzy / case-insensitive name matching.
