# Intent: serve without tournament

Confirmed statement of intent (output of an interview-me session). This captures
*what* we want and *why*, not the implementation. The spec is downstream.

## Outcome

`liga serve` always starts, even when no tournament is in progress. With no active
tournament, both the director (`/`) and audience (`/audience`) show the period
leaderboard. Tournaments are created and run entirely from the Director UI via a
staged wizard. The `--new` CLI flag is removed.

## User

- Tournament director — localhost `/`; defines and runs today's event from the
  browser.
- Audience — `/audience` (including LAN); display for standings and live bracket.

## Why now

`liga serve` currently refuses to start without `--new <name>` or an existing
incomplete tournament. At the venue you want the server and audience display up
immediately; tournament creation belongs in the Director UI, not the CLI.

## Success

- `liga serve` starts with no tournament: period leaderboard on both surfaces;
  director sees leaderboard plus controls to begin defining a tournament.
- Director wizard (staged):
  1. **Define** — tournament name; select players from period data or add new
     names.
  2. **Lock players** — audience switches from period leaderboard to a
     tournament-only leaderboard (entrants and their ratings).
  3. **Race-to** — director sets race-to per round (round count derived from
     locked player count).
  4. **Seed** — audience switches to live tournament view; director gets existing
     bracket and match controls.
- Restarting `liga serve` mid-setup auto-resumes the single incomplete tournament
  from the event log (director lands at the correct wizard step).
- Guest players not in period data receive a default Glicko rating (1500, RD 350)
  at seed time; period players keep their computed ratings.

## Constraint

Event log on disk is the source of truth. Exactly one incomplete tournament
auto-resumes on serve start (same rule as today). Stays within the existing
serve stack (ZIO HTTP, Laminar, event-sourced tournament state).

## Out of scope

- CLI tournament creation (`--new`).
- Redesigning bracket and match controls once a tournament is seeded (keep
  current director behavior).
- Running multiple incomplete tournaments concurrently (unchanged: error or
  explicit resolution if multiple exist).

## Audience states

| Phase | `/audience` shows |
| --- | --- |
| No tournament | Period leaderboard |
| Defined, players locked, not seeded | Tournament-only leaderboard |
| Seeded | Live tournament view |
