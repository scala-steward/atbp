# Intent: director handicap probability hints

Confirmed statement of intent (output of an interview-me session). This captures
*what* we want and *why*, not the implementation. The spec is downstream.

## Outcome

Director MatchPanel handicap hints show the same win-probability neighborhood as
`liga handicap` (weaker player, race-to, win % for +0 / suggested−1 / suggested /
suggested+1), so directors can judge the spot without leaving the UI.

## User

Tournament director on the laptop, during Ready-stage match setup (preview
before Ready, and adjust/apply after Ready).

## Why now

Director only shows a bare suggested spot; the CLI already has the decision data
they need.

## Success

- For the same two ratings + race-to, director shows the same probabilities the
  CLI would.
- The probability neighborhood stays anchored on the suggestion (not recentered
  on typed input).
- If the director types a spot that is not in that neighborhood, a separate hint
  shows the win probability for that typed value.

## Constraint

Content parity with CLI math; web presentation is free to choose. Shared
`liga-common` handicap / `WinProbability` stays the source of truth.

## Out of scope

- Audience view
- Changing CLI output or handicap math
- Recentering the probability neighborhood on typed values
- Probabilities after Start / Completed
