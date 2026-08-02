# Liga bracket results after complete

## Problem Statement

How might we make a completed liga double-elim bracket a permanent results view —
elimination depth at a glance, with tournament record and rating movement on the
same slots — for anyone reopening the tournament (director or audience), without
a separate standings page?

## Recommended Direction

When the tournament is completed (period file emitted), keep the existing bracket
UI and restyle each player’s last participating round slot:

- Name as `Name (W-L)` — tournament match wins–losses only; byes excluded.
- Rated veterans: `frozen +delta` (green if `>0`, red if `<0`, neutral if `0`).
- Previously unrated / guests: replace `unrated` with post-period
  `rating (new)` in guest-badge styling (amber/gold family of roster
  `.guest-badge` / guest row accent) — e.g. `1342 (new)`. Not green, so
  “entered the rating system” does not read as a positive delta.

Director and audience share this behavior (same player-cell rendering). Live
chrome stays: handicaps, race-to headers, scores, winner styling. Elimination
depth is conveyed by annotation placement in the tree, not by a new depth label
or standings list.

Related intent: `docs/intent/liga-bracket-results.md`.

## Key Assumptions to Validate

- [ ] Last-round placement alone communicates elimination depth (spot-check one
      finished double-elim with a non-director club member)
- [ ] Post-period deltas and first ratings for guests are available when reopening
      a completed tournament (not only frozen seed ratings)
- [ ] Crowded last-round cells remain readable on audience (narrow) and director
- [ ] `1342 (new)` in guest-badge color reads as “new to ratings,” distinct from
      a positive delta

## MVP Scope

- Gate on `tournament.completed`.
- Per player: last participated match slot; tournament W–L excluding byes;
  rating display as above.
- Apply via shared bracket player-cell path so director + audience both get it.
- Data sourced from completed tournament / period emission outputs already in
  play — no new page.

## Not Doing (and Why)

- Separate standings/results page — constraint; bracket is the surface.
- Counting byes as wins — distorts tournament record.
- Demoting handicap / race-to / scores on completed mode — keep live density.
- Explicit “eliminated in WB R2” labels — depth via slot position for MVP.
- One-shot Complete confirmation UI — results must survive reopen.
- Changing Complete / period emission mechanics beyond feeding this display.
- Green styling for new ratings — reserved for positive deltas; new uses guest
  vocabulary.

## Open Questions

- Exact rule for “last participating round” with forfeits / no-shows (same as
  last match with a recorded result?).
- Rounding / formatting for delta — reuse `LatestRatingsView.formatDelta`?
- Players who never play a real match (bye-only then out) — W–L `0-0` on bye
  slot, or no annotation?
- Reuse `.guest-badge` class vs a dedicated `.rating-new` that shares the same
  tokens (audience CSS may not load director roster styles).
