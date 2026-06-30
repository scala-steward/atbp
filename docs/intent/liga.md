# Intent: `liga`

Confirmed statement of intent (output of an interview-me session). This captures
*what* we want and *why*, not the implementation. The spec is downstream.

## Outcome

A new `atbp liga` command for a billiards club: maintain Glicko2 ratings from
match history, derive handicaps for matchups, and run handicapped tournaments
via a web app.

## User

- You / club organizers — CLI for ratings + handicaps; tournament director runs
  events.
- Club members + spectators — audience view of tournament progress and results.

## Why now

Match results and standings are tracked and recomputed by hand today.

## Success

- `liga` reads one-or-many period files (or a directory), each file = one Glicko2
  rating period, processed in order; ratings are always reproducible from the
  inputs (files are the source of truth).
- **Leaderboard mode** (default): rating, RD, and career win-loss record per
  player.
- **Handicap mode:** given two players + a race-to-N, spot the weaker player
  enough games so the expected match outcome is as close to 50% as possible,
  without spotting more than 75% of the games required to win.
- **Serve mode:** ZIO backend + Laminar (Scala.js) SPA with:
  - **Director view** — run a double-elimination, rating-seeded bracket; each
    match is a handicapped race-to-N (handicaps auto-derived from current
    ratings).
  - **Audience view** — explore live tournament progress and results.
- Tournament in-progress state is persisted so the web app can resume after a
  crash or restart. A finalized period file (in the CLI input format) is emitted
  only when the tournament completes. Existing input files are never mutated.

## Constraint

Stays within the repo's stack (Scala / ZIO, HOCON config, `better-files`); adds
Scala.js / Laminar to the build for the frontend.

## Out of scope (v1)

- Single-elimination and round-robin formats.
- Authentication / user accounts.
- Running multiple tournaments concurrently.
- Cloud / hosted deployment.
- Mobile apps.
- Editing past period files through the web app.
