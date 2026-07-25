# Race-to display and hints

## Problem Statement

How might we make race-to glanceable and trustworthy on the seeded bracket —
for the director and for club-TV spectators — so nobody opens config or guesses
when entering handicap or results?

## Recommended Direction

**Header + MatchPanel truth with scoreboard language.**

After seed, **round headers** on director and `/audience` show the real race-to
for that scope as **“Race to N”** (e.g. `"Race to 9"`). **Bracket match cards
do not** repeat race-to. The director’s **match control panel** does show it
during Ready / Apply / Result.

Silent defaults like `getOrElse(7)` go away: resolve from `matchDef.raceTo` or
`BracketLayout.scopeRaceTo` / `raceToByScope`. If resolve returns `None` for a
shown round or match that claims a race-to, **fail loudly** (tests and UI — no
blank chip, no “—”, no new default).

When handicap or result entry fails, hints cite the real cap or expected winner
score via **context-aware** copy from the known match race-to (`HandicapCap` /
race-to), not stripped generics.

Shared header labeling keeps director and audience aligned. Cards stay quiet
because every match in a round shares a scope.

## Key Assumptions to Validate

- [ ] Every seeded match/round that appears can resolve a race-to — if not, loud
      failure is correct; spot-check across player counts / scopes
- [ ] “Race to N” on round headers is enough for spectators
- [ ] MatchPanel race-to is visible enough during handicap/result entry that
      directors don’t still guess

## MVP Scope

**In**

- Round headers: `"Race to N"` from resolved scope (shared director + audience
  via `BracketLayout`)
- MatchPanel: same `"Race to N"`
- Bracket match cards: no race-to chrome
- No silent `7` fallbacks; unresolved race-to fails loudly
- Context-aware error/hint strings for handicap cap and winner/loser score vs
  race-to
- Tests for header labels + guidance with mixed scopes (e.g. WB 7 / LB 5 /
  GF 9), plus unresolved → loud failure

**Out**

- Race-to on bracket match cards
- Race-to wizard cascade / setup UX
- Server storage or resolve changes (unless a wrong lookup is the root cause)
- Structured API error codes

## Not Doing (and Why)

- **Race-to on every match card** — redundant within a round; clutters TV;
  header + MatchPanel cover spectator vs director jobs
- **Audience omit** — spectators still get race-to on round headers
- **Non-default-only chrome** — “7” still needs confirming when scopes are mixed
- **Wizard / cascade work** — separate intent; storage already section-aware
- **Parsing API errors as the main path** — brittle; client has race-to when
  translating match-flow errors
- **Soft fallbacks for missing race-to** — hides data bugs; fail loudly instead

## Open Questions

- None — copy is `"Race to N"`; unresolved race-to fails loudly.
