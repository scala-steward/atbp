# Bye bracket display fix

## Problem Statement

**How might we show bye-advanced players only in their immediate next match, matching the UX of a normal round-1 win, on both director and audience views?**

## What shipped

Root cause: the old `isByeMatch` treated any half-filled incomplete match as a bye. After a round-1 bye, the winner lands in `wb-2-X` alone — still half-filled — so bye propagation recursively auto-completed through every later winners round.

**Domain (`BracketByes`):**

- Winners R1 byes (`wb-1-*` with one seed slot): complete as 1–0, place winner in `wb-2-*` pending, stop cascade.
- Ghost losers matches (empty, all feeders permanently dead): mark completed with `isBye`.
- Cascading losers byes (sole player when the empty slot can never fill): auto-advance via `advanceCore(..., isBye = true)`.

**UI:**

- `BracketMatch.isBye` on domain and JS models.
- Director and audience surfaces show italic **bye** label (not a score) via `BracketLayout.resultLabel`.

## Validated assumptions

- [x] R1 winners byes are not the only structural empty slots — losers ghost/cascade cases also need handling.
- [x] `wb-2-*` with one player stays Pending until the feeder completes.
- [x] R1 bye tests pass with stricter downstream assertions (wb-2 pending, no GF pre-fill).

## Test coverage

- 3-in-4 and 12-in-16: player appears in exactly one wb-2 slot, no cascade.
- Losers structural bye (3-player) and ghost bye (5-player).
- `partial fills 3..64`: play-out reaches ready grand final.
- HTTP seed API: 3-player bracket leaves `wb-1-2` as sole ready match.
