# Liga elimination cut

## Problem Statement

How might we let a weeknight tournament director shorten (or deepen) a
double-elim night by choosing how late the bracket stays double-elim — using a
familiar Top‑\(N\) control and a match-count preview on the race-to screen —
without inventing a separate tournament format?

## Recommended Direction

**One Top‑\(N\) control on the race-to screen**, labeled like other bracket sites
(“double elimination until Top \(N\)”).

\(N\) is **1** or a **power of two** that **does not exceed the locked roster
size**. No byes to pad up to \(N\); do not offer undersized values. Default for a
new tournament: **Top 8** when legal; otherwise the largest power of two \(\le\)
roster size (Top 1 remains available whenever roster size \(\ge 1\)).

Scrubbing \(N\) updates a preview of brackets, rounds, players, matches, and
byes; that preview guides race-to choices on the same screen. Long club nights
are the primary pain; Top 1 is for flexibility and bracket-site parity, not the
default path. No separate reset-GF toggle — Top \(N\) is the only endgame
control.

| Top \(N\) | Meaning |
|---|---|
| **1** | True DE endgame — reset grand final (LB must beat WB twice) |
| **2** | Today’s behavior — WB, LB, one decisive GF path |
| **\(4+\)** | DE while more than \(N\) are alive; then the \(N\) survivors play an SE mini-bracket |
| **= field** (field is \(2^k\)) | Full single elimination |

**At the SE cut (Top \(4+\)):** when exactly \(N\) players remain alive, reseed
them into a fresh single-elim mini-bracket by **earned racks won–lost this
tournament** (handicap spots do not count). Goal: the night’s strongest
performers get the best SE seeds and are most likely to meet in the final.
Standard SE placement (1 vs \(N\), snake halves). Losers bracket stops creating
new matches once the cut is reached.

**Reseed ranking** (earned racks only; handicap spots excluded)

1. Rack differential this tournament (earned won − earned lost)
2. Period-start rating seed (stronger first)
3. Draw order

## Key Assumptions to Validate

- [ ] **Preview drives \(N\)** — directors change Top \(N\) because
      match/player/bye counts change the plan
- [ ] **Top 8 default shortens real nights** — for typical rosters, Top 8 (or
      clamped largest legal \(2^k\)) cuts enough without feeling illegitimate
- [ ] **Earned-racks reseed feels fair** — players accept “form tonight” over
      original rating seed at the cut
- [ ] **Race-to scopes track topology** — changing \(N\) drops/adds scopes
      coherently on the wizard
- [ ] **Earned vs spotted racks are unambiguous** from stored match scores +
      `handicap_applied` (spot-check one handicapped match)

## MVP Scope

**In**

- Top‑\(N\) control on the race-to screen (legal \(N\) only: 1 or \(2^k \le\)
  roster; no bye-padded / undersized options)
- Default Top 8 when legal; else largest legal power of two \(\le\) roster
- Live preview: brackets/rounds + per-round players, matches, byes
- Topology + advancement for Top 1 (reset GF), Top 2 (current), Top \(4+\)
  (DE→SE cut), and full SE when field is \(2^k\) and \(N =\) field
- At cut: reseed by earned rack differential (spots excluded), then
  period-start rating seed, then draw order
- Race-to scopes regenerate with \(N\); director still sets race-to per scope
- Audience reflects new rounds / reset GF behavior as needed

**Out**

- Mid-tournament cut after seed
- Match-budget / “finish by X” as the primary control
- Standalone named formats (round robin, pool play, format picker)
- Rating / period emission rule changes
- Audience redesign beyond reflecting the bracket
- Offering \(N\) that requires byes or exceeds roster size

## Not Doing (and Why)

- **Mid-event cut** — planning belongs on race-to; keeps append-only / pre-seed
  config simple
- **Fixed-path cut (no reseed)** — night’s strongest performers should be steered
  toward the final via earned racks
- **Reseed by period-start rating alone** — rating is only a late tie-break
- **Counting handicap spots as racks** — reseed reflects balls pocketed / games
  earned, not the spot on the wire
- **Separate reset-GF toggle** — one control only
- **Match-count as the hero knob** — directors scrub Top \(N\); preview is the aid
- **Preset-only UI** — full legal power-of-two flexibility required
- **start.gg-style Top 8 still-DE phase** — cut means SE, not continued DE
- **Bye-padded \(N\) or \(N >\) field** — illegal; no undersize
- **Full SE when roster is not a power of two** — would need byes; not offered
- **Rating/period rule changes** — cut is bracket shape + local reseed only

## Open Questions

_(None — ready for spec.)_
