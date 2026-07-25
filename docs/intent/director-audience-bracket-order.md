# Intent: director + audience bracket order

Confirmed statement of intent (output of an interview-me session). This captures
*what* we want and *why*, not the implementation. The spec is downstream.

## Outcome

Director and audience bracket lists only show matches that matter now, with later
/ still-open work above and finished work sunk without leaving its round.

## User

- Tournament director on the laptop (Active / Completed phase).
- Club TV / spectators on `/audience`.

## Why now

On large brackets, early and not-yet-relevant slots bury the live work. The
director also scrolls the side match-controls panel out of view; controls stay
where they are — only the list changes.

## Success

Shared visibility and order semantics for director and audience:

**Visibility** — show a match when it is Ready, Started, or Completed, or when
it is Pending with at least one player set (waiting on an opponent). Hide
Pending matches with neither player set.

**Round stacking**

1. Rounds that still have any unfinished match sit above rounds where every
   match is done.
2. Within each of those two bands, section order is **Grand Final → Losers →
   Winners**, with **later rounds above earlier** inside a section.

**Within a round**

1. Ready
2. Pending
3. Live (Started), in current bracket-seeding / match-id order
4. Done (Completed), in that same seeding order

## Constraint

Match controls panel placement unchanged. List behavior is shared for director
and audience (same visibility/order rules).

## Out of scope

- Sticky or floating match controls
- Changing match lifecycle APIs (ready / handicap / start / result)
- Reordering or filtering outside the Active/Completed bracket views
