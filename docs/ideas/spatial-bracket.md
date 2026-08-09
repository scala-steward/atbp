# Spatial audience bracket (chalkboard columns)

## Problem Statement

How might we let liga spectators on a laptop or big screen grasp a live
tournament as a classic bracket at a glance — who’s playing, who advanced,
and how we got here — without replacing the list view or drawing a full
double-elim graph?

## Recommended Direction

Ship an opt-in **second route** (not a toggle on `/audience`) that renders the
same audience bracket data as **round columns**: matches stacked top→bottom
within a round; **most current rounds on the left**; scroll right to review
earlier completed rounds; omit rounds that still have no assigned players.

Stay on **story-first layout (B)**: winners / losers / GF / SE as labeled
bands or column groups with light within-section advance cues — **not**
faithful WB→LB drop topology.

Visual language is **chalkboard / wall bracket**: sparse cells (names, score,
winner emphasis), minimal chrome. Reuse existing audience data and labeling
helpers; do not invent new bracket facts. Keep today’s list as the default
audience experience.

Related intent: `docs/intent/spatial-bracket.md`.

## Key Assumptions to Validate

- [ ] Club spectators will open the second route during a live night
      (link from director / QR / bookmark on the house display)
- [ ] Scroll-right through completed columns answers “how did we get here”
      without drop arrows (spot-check one mid-tournament DE)
- [ ] Current-left + hide-empty-future is obvious on first open (no legend)
- [ ] Chalkboard sparseness still feels complete enough next to the list view

## MVP Scope

- New audience route `/audience/bracket`; list `/audience` unchanged and default
- Director footer: clickable links to `/audience` and `/audience/bracket`
  (replace plain-text `DirectorGuidance.localhostNote`)
- Column layout: rounds with ≥1 assigned player; current/unfinished bias left;
  completed earlier rounds to the right; matches top→bottom in a column
- Sectioned bands for WB / LB / GF / SE: **stacked row-bands** on one shared
  left=current axis (labels, not woven drop edges)
- Sparse cells (**Club** density): names, scores/winners where present, applied
  handicap `(+N)` when present; no ratings; thin advance affordance within a
  section; state via subtle live/done chrome
- Same poll/data surface as today’s audience bracket
- Wide viewport first; horizontal scroll acceptable

## Not Doing (and Why)

- Replacing the list audience view — constraint; list stays default
- Faithful DE drop-path diagram — fights glanceability at 16–32
- Phone-first polish — constraint; wide-first
- In-page toggle instead of second route — shareable/TV URL matters more
- Path highlighter / click-to-trace (var 6) — validate scroll-history first
- New bracket data features — same facts as audience already sees
- Director spatial twin — audience-only outcome
- Dense parity with list chrome (ratings / big status stamps) — Club density
  keeps spot + score; ratings stay on the list view

## Resolved decisions

- **URL:** `/audience/bracket` (second route; list stays at `/audience`).
- **Discovery:** Director footer note (today: plain “Open /audience on the
  club TV” in `DirectorGuidance.localhostNote`) becomes clickable links to
  **both** `/audience` and `/audience/bracket` so the TD can open either in a
  window/tab directly for the house display.
- **Section geometry:** Stacked row-bands (option X) — Winners (+ GF / SE as
  labeled) above Losers on one shared horizontal timeline (current left,
  earlier right). Not separate scroll strips; not a tabbed primary-band.
- **Cell density (Club):** Names, score/winner when present, applied handicap
  `(+N)` when present. No ratings on spatial cells (list keeps them). Match
  state via subtle live/done chrome, not a large status stamp.

## Open Questions

- None — ready for spec / implementation slicing.
