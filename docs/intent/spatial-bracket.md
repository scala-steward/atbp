# Intent: spatial audience bracket

Confirmed statement of intent (output of an interview-me session). This captures
*what* we want and *why*, not the implementation. The spec is downstream.

## Outcome

An opt-in alternate liga audience view that renders the tournament as a spatial,
poster-like bracket (rounds + connecting winner paths).

## User

Spectators / audience looking at an existing liga tournament (not admins running
it).

## Why now

The current audience view has the info, but it doesn’t read as a bracket at a
glance.

## Success

On a laptop/big screen, you can tell who’s playing whom, who advanced, and
(where scores already exist) the score — without hunting through a dense list.

## Constraint

- Design for wide viewports first.
- Keep the current audience view as the default.
- Switch in via a clear opt-in (link / toggle / second route).
- Same data the audience already sees (names, structure, winners, scores where
  present); reuse existing match drill-down where applicable.

## Out of scope

- Replacing the current audience view
- Phone-first layout polish
- New bracket *data* features beyond what the audience already sees
