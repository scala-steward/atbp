# Intent: liga elimination cut (DE → SE / true DE)

Confirmed statement of intent (output of an interview-me session). This captures
*what* we want and *why*, not the implementation. The spec is downstream.

## Outcome

On the race-to screen, the director sets **SE-from last \(N\)** (\(N = 1\) or a
power of two) so a club night can finish sooner than full classic double-elim,
or run a true double-elim finish — without introducing a separate tournament
format.

## User

Tournament director running liga on a weeknight (audience still watches the same
event).

## Why now

Full double-elim runs too long for club nights; plain single-elim feels too
abrupt. Directors also want the option of a true double-elim endgame (LB winner
must beat WB winner twice).

## Success

- Config lives on the **race-to** screen (same wizard spine as today).
- Director chooses **last \(N\)** where \(N\) is **1** or any **power of two**.
- Screen shows how many brackets and rounds that choice produces from the
  locked roster size.
- For each round, a director hint shows how many **players**, **matches**, and
  **byes**.
- **Last 1** — full / true double-elim: losers-bracket winner must beat the
  winners-bracket winner **twice** (reset grand final).
- **Last 2** — today’s behavior: winners bracket, losers bracket, grand final
  (one decisive GF path as now).
- **Last 4+** (powers of two) — play normal double-elim **until only \(N\)
  players remain alive**, then those \(N\) play a **single-elim mini-bracket**
  (e.g. last 4 → semi-finals + grand final). The losers bracket stops creating
  new matches once the cut is reached.
- Larger cuts shrink and eventually remove the losers bracket until the choice
  covers the field = **full single elimination**.

## Constraint

Same tournament spine and race-to screen — not a separate named format.
Double-elim while more than \(N\) are alive (for \(N \ge 2\)); then single-elim
among the survivors. Last 1 and last 2 are the two double-elim endgames.

## Out of scope

- Separate formats (standalone round robin / pool play as their own tournament
  types).
- Changing rating / period emission rules.
- Redesigning the audience beyond reflecting the new rounds and true-GF
  behavior.
