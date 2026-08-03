# TD match wait indicators

## Problem Statement

How might we make floor urgency glanceable on the director match list so the TD
always calls the longest-waiting Ready match next — and can eyeball Done dwell
before seating someone again — without policies, queues, or changing the
audience view?

## Recommended Direction

**Director Ready strip + shared elapsed clocks.**

On the director list only: all Ready matches float to the very top, sorted by
longest wait first. Everything else keeps today’s bracket-order rules. Audience
ordering and visibility stay unchanged via a **director-only layout entry
point** (wrapper/helper); audience keeps calling `groupMatches` as today.

**Wait clock** — side arrival = Instant that side was filled (feeder
`MatchResult`/`MatchForfeit.at`, `BracketSeeded.at` for seeds, bye-propagation
moment for bye advances). Wait start = earlier of the filled sides’ arrival
times. R1 both-seeded: wait starts at Ready/seed (same Instant). Never use
`MatchReady.at`.

**Done clock** — elapsed since this match’s `MatchResult`/`MatchForfeit.at`. No
clock on `isBye` rows.

Pending-with-one-player stays below the Ready strip; may show cooling elapsed
in place. Same elapsed chip for Ready wait and Done dwell. Display at **minute
resolution**, refresh about **every 15s** (and on tournament poll).

Related intent: `docs/intent/td-match-wait-indicators.md`.

## Key Assumptions to Validate

- [ ] First-available and Done ages derived from event-log Instants match floor
      memory on a real tournament log (incl. forfeit feeders and bye advances).
- [ ] Floating all Ready above Started/Pending doesn’t hide in-progress work
      the TD still needs mid-event.
- [ ] A single elapsed chip is enough for rest judgment (no per-side Done ages
      in MVP).

## MVP Scope

**In**

- Director-only Ready strip (longest wait first); lower list = existing order
- Elapsed on Ready and Completed; optional cooling on Pending-with-one
- Derive/expose timestamps from the event log per the rule table above
- Minute-resolution chip, ~15s refresh

**Out**

- Audience list changes
- Pending in the Ready strip
- Thresholds, colors-as-rules, auto-call, notifications
- Per-player Done breakdown, rest policies, new screens/queues
- State-machine changes beyond using existing timing

## Not Doing (and Why)

- **Audience parity** — floor urgency is a TD job; spectator order intent stays
  as shipped.
- **Pending in the Ready strip** — Ready means callable; half-ready cooling
  stays in place and is baked into Ready wait later.
- **Rest thresholds / highlights** — director eyeballs “enough.”
- **Parameterizing shared `groupMatches` for the strip** — director wrapper
  keeps the audience path untouched.
- **Player-centric cooling queue** — existing match list only.
- **1s stopwatch ticks** — glanceable urgency, not a chronometer.

## Open Questions

- None product-blocking; remaining detail (exact API field shapes, bye Instant
  wiring in replay) belongs in the spec.
