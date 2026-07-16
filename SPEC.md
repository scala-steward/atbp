# Spec: Section-Aware Race-To

Source idea: [`docs/ideas/section-aware-race-to.md`](docs/ideas/section-aware-race-to.md)

## Assumptions

Proceeding with these unless you correct them:

1. **Breaking change is acceptable** — no migration of existing tournament event directories; fixtures and tests are rewritten in place.
2. **Scope keys are flat strings** (`wb-3`, `lb-4`, `gf`) end-to-end — not a nested `{ winners, losers, grandFinal }` API shape.
3. **Cascade is wizard-only** — the server stores a full `raceToByScope` map; no anchor/sparse semantics on the backend.
4. **No post-seed race-to edits** — misconfiguration requires a new tournament (same as today).
5. **No per-match override** — effective race-to always resolves from scope at `MatchReady` / resolve time.
6. **`RaceToScopes` lives on JVM** (`liga`) with a **JS mirror** (`liga-js`), matching today's `BracketRounds` / `WizardRounds` split — not moved into `liga-common` unless implementation proves shared topology access is needed.
7. **Event rename** — `RoundRaceToSet` → `RaceToSet` with payload field `scope: String` (replacing `round: Int`). Event log filename segment may change from `round-race-to` to `race-to`.

## Objective

Tournament directors need to set race-to **independently per bracket section** (Winners round N, Losers round N, Grand Final). Today `roundRaceTo: Map[Int, Int]` collapses all sections that share a round number into one value — so losers cannot be shorter than winners and Grand Final cannot be longer than the final winners round.

**Target users:** tournament directors using the Liga director wizard (8–64 players).

**User stories:**

- As a director, I can set losers bracket race-to shorter than winners (e.g. winners 7, losers 5).
- As a director, I can set Grand Final race-to longer than the final winners round (e.g. finals 7, GF 9).
- As a director, I can configure a typical 8-player event in 2–3 edits thanks to cascade rules, not ~8 individual fields.
- As a director, I see section-grouped inputs (Winners / Losers / Grand Final) at all bracket sizes.

**Success looks like:** a director saves race-to once before seed; every match resolves the correct race-to from its scope key; replay, API, and UI stay in sync.

## Tech Stack

| Layer | Choice |
|-------|--------|
| Language | Scala 3.8.4 |
| Server / domain | `liga` (ZIO, zio-json) |
| Director UI | `liga-js` (Scala.js, Laminar) |
| Shared models | `liga-common` (handicap, roster — scope logic stays in liga + mirror for now) |
| Tests | ZIO Test (`ZIOSpecDefault`) |
| Build | sbt cross-project |

## Commands

```bash
# Compile server + JS
sbt --client compile

# Run all liga tests (domain, API, replay fixtures)
sbt --client "liga/test"

# Run JS/director tests
sbt --client "ligaJs/test"

# Focused tests while developing
sbt --client "liga/testOnly *RaceToScopes*"
sbt --client "liga/testOnly *Replay*"
sbt --client "ligaJs/testOnly *Wizard*"

# Format + compile gate (required before commit)
sbt --client fixup
```

## Project Structure

```
liga/src/main/scala/ph/samson/atbp/liga/
  bracket/
    BracketRounds.scala          → replace with RaceToScopes.scala (scope string keys)
  model/Types.scala              → TournamentState.raceToByScope, RaceToSetPayload
  tournament/
    MatchLifecycle.scala         → resolveRaceTo via keyForMatch
    TournamentPhase.scala        → raceToComplete on scope keys
    Replay.scala                 → apply RaceToSet events
    Seed.scala                   → validate + emit scope events
    events/TournamentEvent.scala → RaceToSet event type
  serve/
    DirectorRoutes.scala         → SeedRequest / RaceToRequest payloads
    ApiJson.scala                → API response field rename

liga-js/src/main/scala/ph/samson/atbp/liga/js/
  api/Models.scala               → mirror API types
  api/Client.scala               → setRaceTo / seed signatures
  director/
    WizardView.scala             → section-grouped UI + cascade
    WizardRounds.scala           → replace with RaceToScopes mirror
    BracketLayout.scala          → defaultRaceTo via scope; drop raceToRoundLabel
    DirectorApp.scala            → wire new map type

liga/src/test/resources/tournaments/**/
  *-round-race-to.json           → rewrite as *-race-to.json with scope payloads

docs/ideas/section-aware-race-to.md  → design rationale (reference only)
```

## Core Design

### Scope keys

| Key | Matches | Example use |
|-----|---------|-------------|
| `wb-1` … `wb-N` | Winners bracket round N | early rounds → 7 |
| `lb-1` … `lb-M` | Losers bracket round N | often 5 |
| `gf` | Grand Final only (`gf-1`) | usually 9 or 11 |

**Resolve path:**

```
wb-3-1  → wb-3
lb-4-2  → lb-4
gf-1    → gf
```

`MatchLifecycle.resolveRaceTo` looks up `state.raceToByScope(RaceToScopes.keyForMatch(matchId))`.

`RaceToScopes.requiredKeys(playerCount)` returns all `wb-*`, `lb-*`, and `gf` keys derived from bracket topology (replaces `BracketRounds.requiredKeys`).

**Example (8 players, bracket size 8):** `wb-1`, `wb-2`, `wb-3`, `lb-1`, `lb-2`, `lb-3`, `lb-4`, `gf` — eight scope keys.

### API shape

```json
{
  "raceToByScope": {
    "wb-1": 7, "wb-2": 7, "wb-3": 7,
    "lb-1": 7, "lb-2": 7, "lb-3": 7, "lb-4": 7,
    "gf": 9
  }
}
```

Single POST payload with the full map on seed and on race-to save (same as today, more keys).

### Events

`RoundRaceToSet` → `RaceToSet`:

```json
{
  "type": "RaceToSet",
  "seq": 4,
  "at": "2026-03-15T18:00:00Z",
  "payload": { "scope": "wb-1", "raceTo": 7 }
}
```

One event per scope key (same emission pattern as today).

### Wizard layout

Section-grouped at all bracket sizes:

```
Winners Bracket
  Round 1  [7]
  Round 2  [7]
  ...

Losers Bracket
  Round 1  [7]
  ...

Grand Final
  [7]
  (helper: "usually longer than finals — set explicitly.")
```

Labels derive from scope keys (`wb-3` → "Winners — round 3"). Remove `BracketLayout.raceToRoundLabel` multi-scope concatenation.

### Cascade rules (client-side only)

On save, send the full `raceToByScope` map. Cascade runs only in the wizard.

| Rule | Behavior |
|------|----------|
| General | Editing round **k** in a section sets **k through N** in that section |
| `wb-1` special | Also sets `lb-1` to the same value, which cascades to `lb-2`..`lb-M` |
| Grand Final | Tracks `wb-N` until director edits `gf` directly (**pinned**); pinned GF ignores later `wb-*` edits |

| Edit | Winners | Losers | GF (unpinned) |
|------|---------|--------|---------------|
| `wb-1` | `wb-1`..`wb-N` | `lb-1`..`lb-M` (via `lb-1`) | `wb-N` |
| `wb-k` (k ≥ 2) | `wb-k`..`wb-N` | — | `wb-N` if changed |
| `lb-k` | — | `lb-k`..`lb-M` | — |
| `gf` | — | — | value set; GF pinned |

**Initial state on wizard load:**

| Scope | Value | Source |
|-------|-------|--------|
| `wb-1`..`wb-N` | 7 | hardcoded default |
| `lb-1` | = `wb-1` | sync on load |
| `lb-2`..`lb-M` | = `lb-1` | cascade on load |
| `gf` | = `wb-N` | sync on load; unpinned |

## Code Style

Follow existing Liga conventions: `object` companions for pure helpers, `Either[Error, T]` for domain validation, zio-json `@jsonHint` on events, Laminar `Var`/`Signal` in the wizard.

**New scope helper (illustrative):**

```scala
object RaceToScopes {

  def keyForMatch(matchId: String): Option[String] =
    matchId match {
      case s"wb-$round-$_" => Some(s"wb-$round")
      case s"lb-$round-$_" => Some(s"lb-$round")
      case "gf-1"           => Some("gf")
      case _                => None
    }

  def requiredKeys(playerCount: Int): SortedSet[String] = {
    val bracketSize = Seeding.bracketSize(playerCount)
    BracketTopology(bracketSize).matches.keys
      .flatMap(keyForMatch)
      .to(SortedSet)
  }

  def scopeLabel(scope: String): String =
    scope match {
      case s"wb-$n" => s"Winners — round $n"
      case s"lb-$n" => s"Losers — round $n"
      case "gf"     => "Grand Final"
      case other    => other
    }
}
```

**Resolve race-to (illustrative change in `MatchLifecycle`):**

```scala
def resolveRaceTo(state: TournamentState, matchId: String): Either[Error, Int] =
  for {
    scope <- RaceToScopes.keyForMatch(matchId)
      .toRight(InvalidTransitionError(matchId, "ready", "unknown match scope"))
    raceTo <- state.raceToByScope
      .get(scope)
      .toRight(MissingRaceToError(matchId, scope))
  } yield raceTo
```

Rename `MissingRaceToError` round field to `scope: String` where applicable.

## Testing Strategy

**Framework:** ZIO Test in `liga/src/test` and `liga-js/src/test`.

| Concern | Where | Approach |
|---------|-------|----------|
| `keyForMatch` / `requiredKeys` | `RaceToScopesSpec` | Unit: 8/16/64 player key sets; match-id → scope mapping |
| `resolveRaceTo` | `MatchLifecycle` tests or dedicated spec | Winners, losers, GF resolve independently |
| Event codec | `EventCodecSpec` | `RaceToSet` JSON round-trip |
| Replay | `ReplaySpec` + fixture JSON | Rewritten `*-race-to.json` files replay to correct state |
| Phase gating | `TournamentPhase` / `SeedSpec` | `raceToComplete` requires every scope key |
| API contract | `ApiClientContractSpec`, `WriteApiSpec`, `ReadApiSpec` | `raceToByScope` in responses and POST bodies |
| E2E seed flow | `EndToEndSpec` | Seed with differentiated wb/lb/gf values |
| Wizard cascade | `liga-js` (optional) | Pure extraction of cascade into testable object if `WizardView` logic grows |

**Fixture rewrite pattern:**

```json
// before
{"type":"RoundRaceToSet","payload":{"round":3,"raceTo":7}}

// after
{"type":"RaceToSet","payload":{"scope":"wb-3","raceTo":7}}
```

Add separate `lb-*` and `gf` events where fixtures previously relied on shared integer round keys.

**Coverage expectation:** all existing race-to / seed / replay tests pass after rewrite; new spec covers scope-key edge cases (GF pinned behavior is UI-only — no server test unless cascade helper is extracted).

## Boundaries

### Always

- Keep JVM `RaceToScopes` and JS mirror in sync (same keys for a given `playerCount`).
- Validate all `requiredKeys(playerCount)` are present before seed.
- Emit one `RaceToSet` event per scope on save/seed (full map, no sparse storage).
- Run `sbt --client fixup` and `liga/test` before committing Scala changes.
- Rewrite replay fixtures when event shape changes.

### Ask first

- Moving `RaceToScopes` into `liga-common` cross-project (would touch build deps).
- Changing event log filename conventions beyond `round-race-to` → `race-to`.
- Adding server-side cascade or partial-map merge semantics.
- Supporting old `roundRaceTo` / `RoundRaceToSet` alongside the new shape.

### Never

- Migrate production tournament directories (explicitly out of scope).
- Add per-match race-to override at `MatchReady` time.
- Change match ID format (`wb-2-1`, `lb-3-2`, `gf-1` stay as-is).
- Store cascade pin state (`gfPinned`) on the server — client-only.
- Remove failing tests or skip fixture updates to greenwash the change.

## Success Criteria

- [ ] `TournamentState`, API models, `SeedRequest`, and `RaceToRequest` expose `raceToByScope: Map[String, Int]` (no `roundRaceTo`).
- [ ] `RaceToScopes.keyForMatch` and `requiredKeys` exist on JVM; JS mirror returns identical key sets for 8, 16, 32, 64 players.
- [ ] `resolveRaceTo("lb-2-1", …)` and `resolveRaceTo("wb-2-1", …)` can return different values for the same numeric round.
- [ ] `resolveRaceTo("gf-1", …)` uses `gf` scope, independent of `wb-N`.
- [ ] `TournamentPhase.raceToComplete` passes only when every required scope key is set.
- [ ] `RaceToSet` events replay correctly; all tournament fixture directories updated.
- [ ] Director wizard shows Winners / Losers / Grand Final sections with cascade + GF pin behavior per tables above.
- [ ] `sbt --client "liga/test"` and `sbt --client "ligaJs/test"` pass.
- [ ] Typical 8-player flow works: set `lb-1` → 5 (losers all 5), `wb-2` → 5 (winners 7,5,5; GF → 5 if unpinned), `gf` → 9 (pinned).

## MVP Scope

**In:** everything in Success Criteria above.

**Out:**

- Old tournament directory migration
- Server-side cascade / sparse anchor storage
- Per-match race-to override
- Bulk "copy winners → losers" button
- Post-seed race-to changes
- Smart GF default (9/11) — helper text only
- Match ID format changes

## Open Questions

All items from the idea doc are resolved:

- **Single POST with full map?** Yes.
- **`wb-1` cascades to losers via `lb-1`?** Yes.

No blocking open questions remain. If you want a different event rename (`RaceToByScopeSet`) or to hoist `RaceToScopes` into `liga-common`, say so before implementation.
