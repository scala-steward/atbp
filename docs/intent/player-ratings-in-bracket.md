# Intent: player ratings in bracket views

Show each player's **frozen tournament rating** as a small, muted label centered
below their name in director and audience bracket views. Ratings are informational
— available for those who want them, without competing with names, scores, or
match state.

## Outcome

- Period players: whole number from the frozen snapshot (e.g. `1543`).
- New/guest players (not on the period roster at seed): `unrated`, not the init
  rating (`1500`).
- TBD slots and pre-seed (empty `frozenRatings`): no rating subline.
- Director and audience use the same render path (`AppliedHandicapView`).

## Unrated detection

Frozen ratings carry no guest flag. A player is **unrated** when their frozen
profile matches the synthetic guest profile at seed: `rd == Tuning.Default.maxDeviation`
and `wins == 0` and `losses == 0`. Period players at the same numeric rating but
lower RD still show the number.

## Styling

`.player-cell` stacks name + rating vertically; `.player-rating` is smaller,
muted, tabular-nums. Rules live in `director.css` and are mirrored in
`AudienceApp` inline styles (audience does not load director CSS).

## Boundaries

- Frozen ratings only — not live ledger values.
- No backend or API changes for v1.
- No RD, delta, or W-L in bracket rows unless revisited deliberately.
