# Intent: liga bracket results after complete

Confirmed statement of intent (output of an interview-me session). This captures
*what* we want and *why*, not the implementation. The spec is downstream.

## Outcome

After a liga tournament is completed (period file emitted), the bracket becomes a
permanent results view: each player’s name/rating on their last played round
shows match W–L and rating delta.

## User

Anyone reopening that completed tournament (not only the moment you hit
Complete).

## Why now

You want to read the finished draw as results — how far people got — with rating
movement as a secondary glance on the same slots.

## Success

On each player’s last participating round:

- Name as `Name (W-L)` with match wins–losses this tournament only (byes
  excluded from the record).
- Rating as `frozen +delta` (green if `>0`, red if `<0`, default/neutral if
  `0`).

## Constraint

Reuse the existing bracket UI; data comes from the completed tournament / period
emission, not a new surface.

## Out of scope

- Separate standings/results page.
- Counting byes as wins.
- One-shot confirmation-only UI after Complete.
- Changing how Complete / period emission itself works (beyond feeding this
  display).
