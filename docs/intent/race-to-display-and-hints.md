# Intent: race-to display and hints

Confirmed statement of intent (output of an interview-me session). This captures
*what* we want and *why*, not the implementation. The spec is downstream.

## Outcome

After the bracket is seeded, race-to is trustworthy and easy to see — round
headers and MatchPanel show `"Race to N"` from resolved scope data. Bracket
match cards stay quiet (no race-to chrome). Validation and error hints that
depend on race-to cite actual numbers (for example maximum handicap and
expected winner/loser score).

## User

- Tournament director on the laptop (seeded / active bracket, and when entering
  handicap or results).
- Club TV / spectators on `/audience`.

## Why now

Seeded brackets hide or mis-state race-to. Match-card hints previously lied
(always showing 7). Related validation messages did not cite the real limits for
that match's scope, so directors guessed instead of reading the number.

## Success

- Glance at a round header or MatchPanel and trust the race-to without opening
  configuration or guessing.
- When handicap or result entry is rejected, the hint names the real cap or
  expected score for that match.

## Constraint

Same display idea on director and audience (shared `BracketLayout` labeling).
Round headers and MatchPanel show race-to; match cards do not. Unresolved
race-to fails loudly with bug-filing copy instead of silent defaults.

## Out of scope

- Race-to wizard cascade / UX during setup
- Changing how race-to is stored or resolved server-side, unless a wrong lookup
  is what causes the bad display or error numbers
