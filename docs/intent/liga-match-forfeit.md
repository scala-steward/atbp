# Intent: liga match forfeit

Confirmed statement of intent (output of an interview-me session). This captures
*what* we want and *why*, not the implementation. The spec is downstream.

## Outcome

Admin/TD can mark a match as forfeit; opponent advances as if by bye; bracket
placement follows a normal loss (e.g. winners → losers).

## User

Tournament admin/TD running liga events (players don’t self-declare).

## Why now

Real ops cases like late to a winners-round start — need to advance the waiting
opponent without inventing a played result.

## Success

- Forfeited match adds no scores to period records.
- Matches that player already finished still count in period records.
- Player stays eligible for later matches (e.g. losers) unless those are
  forfeited too.

## Constraint

Forfeit is per-match only; admin can apply it even if the match had scores / was
in progress.

## Out of scope

- Player/opponent self-forfeit.
- One-click full tournament withdrawal.
- Automatic timeout forfeits.
