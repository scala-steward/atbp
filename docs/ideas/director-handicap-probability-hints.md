# Director handicap probability hints

## Problem Statement

How might we make the Ready-stage handicap suggestion legible enough that
director and players can agree on the applied spot, using the same neighborhood
the CLI already shows, in a compact vertical MatchPanel?

## Recommended Direction

Ship a **huddle-ready CLI neighborhood** in director MatchPanel: same decision
data as `HandicapRenderer` (weaker player, race-to, win % for +0 / suggested−1 /
suggested / suggested+1), laid out for three people reading a laptop
shoulder-to-shoulder.

Suggested row is **bold**. Neighborhood stays anchored on the suggestion. If
the typed spot is outside that set, show a separate hint with that spot’s win %.
Percents are **whole numbers** (weaker player only; see
`docs/specs/director-handicap-probability-hints.md` — not the complementary
stronger-% pairs sketched in early notes). Neighborhood membership matches CLI (`List(0, max(0, suggested-1), suggested, suggested+1)`).

Shared `liga-common` `WinProbability` / handicap math remains the source of
truth; no API change if computed client-side from frozen ratings.

Upstream intent: `docs/intent/director-handicap-probability-hints.md`.

## Key Assumptions to Validate

- [ ] Players can agree from the neighborhood table without a verbal “closest to
      ~50%” explanation — try on the next real Ready-stage huddle
- [ ] Compact vertical layout is readable at laptop distance for 2–3 people —
      check once in MatchPanel with a real match
- [ ] Whole weaker percents feel trustworthy next to the CLI’s one-decimal
      column — spot-check a few known pairs against `liga handicap`

## MVP Scope

**In**

- Ready-stage only: preview before Ready, and adjust/apply after Ready
- Weaker name, race-to, four neighborhood spots with whole-number weaker win %
- Suggested row bold
- Separate typed-spot % when typed value ∉ neighborhood (same rounding rule)
- Client-side math via existing shared handicap / `WinProbability`
- Parity tests: same neighborhood set and underlying probabilities as CLI;
  display formatting is whole % with complementary rounding

**Out** — see Not Doing

## Not Doing (and Why)

- **Audience view / post-Start probabilities** — agreement happens at Ready;
  after Start the spot is locked
- **Changing CLI output or handicap math** — director is the consumer; CLI stays
  the reference
- **Recentering the neighborhood on typed input** — suggestion stays the
  anchor; typed gets its own hint
- **Prose “why ~50%” / negotiation-band framing** — validate whether numbers
  alone suffice first
- **One-line summary instead of the full neighborhood** — full information is
  required for agreement
- **CLI one-decimal / weaker-only display** — director uses denser whole-%
  pairs for the huddle; math parity, not string parity

## Open Questions

_(none blocking — format, emphasis, and CLI neighborhood behavior decided)_
