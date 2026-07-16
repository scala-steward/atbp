# Section-Aware Race-To

## Problem Statement

How might we let directors configure race-to independently per bracket section
(Winners round N, Losers round N, Grand Final) instead of collapsing them into a
single integer round key — with wizard defaults and cascade rules that keep input
minimal for typical events?

## Recommended Direction

Replace `roundRaceTo: Map[Int, Int]` with **`raceToByScope: Map[String, Int]`**
using keys that mirror match ID structure:

| Key | Matches | Example |
|-----|---------|---------|
| `wb-1` … `wb-N` | Winners bracket | early rounds to 5 |
| `lb-1` … `lb-M` | Losers bracket | often shorter than winners |
| `gf` | Grand Final only | usually longer (9 or 11) |

**Resolve path:** derive scope from `matchId`:

```
wb-3-1  → wb-3
lb-4-2  → lb-4
gf-1    → gf
```

`MatchLifecycle.resolveRaceTo` looks up `raceToByScope(keyForMatch(matchId))`.
`BracketRounds.requiredKeys` becomes `RaceToScopes.requiredKeys(playerCount)`
returning all `wb-*`, `lb-*`, and `gf` keys from topology.

**Events:** evolve `RoundRaceToSet` → `RaceToSet(scope: String, raceTo: Int)` (or
rename the payload field from `round: Int` to `scope: String`).

**API:**

```json
{
  "raceToByScope": {
    "wb-1": 7, "wb-2": 7, "wb-3": 7,
    "lb-1": 7, "lb-2": 7, "lb-3": 7, "lb-4": 7,
    "gf": 9
  }
}
```

No migration of old tournament directories — breaking change is acceptable.

### Wizard layout

Section-grouped at all bracket sizes (8 through 64 players):

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
```

Each input label comes from its scope key (`wb-3` → "Winners — round 3"). Drop
`raceToRoundLabel` multi-scope concatenation.

### Cascade rules (wizard UX only)

Cascade is **client-side only**. On save, the full `raceToByScope` map is sent;
the backend stores one value per scope key with no anchor/sparse semantics.

**General rule:** editing round **k** in a section sets **k through N** in that
section.

**`wb-1` special case:** uses the general winners rule **plus** sets `lb-1` to
the same value. It does **not** cascade to `lb-2`..`lb-M`.

**Grand Final:** tracks `wb-N` until the director edits `gf` directly (pin). When
pinned, later `wb-*` edits do not change `gf`.

| Edit | Winners | Losers | GF (unpinned) |
|------|---------|--------|---------------|
| `wb-1` | `wb-1`..`wb-N` | `lb-1` only | `wb-N` |
| `wb-k` (k ≥ 2) | `wb-k`..`wb-N` | — | `wb-N` if changed |
| `lb-k` | — | `lb-k`..`lb-M` | — |
| `gf` | — | — | value set; GF pinned |

**Initial state:**

| Scope | Value | Source |
|-------|-------|--------|
| `wb-1`..`wb-N` | 7 | hardcoded default |
| `lb-1` | = `wb-1` | sync on load |
| `lb-2`..`lb-M` | = `lb-1` | cascade on load |
| `gf` | = `wb-N` | sync on load; unpinned |

**Typical director flow (3 edits for 8 players):**

1. `lb-1` → 5 — all losers `5,5,5,5`
2. `wb-2` → 5 — winners `7,5,5`; GF unpinned → `5`
3. `gf` → 9 — pins GF

Helper text under Grand Final: "usually longer than finals — set explicitly."

## Key Assumptions to Validate

- [x] **Losers ≠ winners race-to is frequent** — losers are often shorter (e.g. 5
  vs 7); section keys are required
- [x] **Grand Final is almost always longer** — needs own scope (`gf`); cannot
  share `wb-N` in practice (GF pinned after manual edit)
- [x] **Events run up to 64 players** — section-grouped wizard with ~17 inputs
  is acceptable
- [x] **Uniform race-to within a scope** — all `lb-2-*` matches share one value;
  per-match keys not needed in wizard
- [x] **Breaking `roundRaceTo` / old events is OK** — no production migration
- [x] **Cascade reduces friction** — directors mostly edit `wb-1`, `wb-2`, `lb-1`,
  and `gf` rather than every field
- [ ] **Per-match override gap is acceptable for v1** — documented in
  `docs/ideas/liga.md` but not implemented; section-aware wizard is the only path
  today
- [ ] **Post-seed race-to edits remain out of scope** — misconfiguration requires
  re-seed / new tournament (current behavior)

## MVP Scope

**In:**

- `raceToByScope: Map[String, Int]` on `TournamentState`, API models,
  seed/race-to requests
- `RaceToScope.keyForMatch(matchId)` + `requiredKeys(playerCount)` (JVM + JS
  mirror)
- `resolveRaceTo`, `raceToComplete`, seed validation on scope keys
- `RaceToSet` events (one per scope, same pattern as today)
- Section-grouped wizard with cascade logic and GF pin state
- Initial defaults: all `wb-*` = 7; `lb-1` = `wb-1`; `lb-2`..`lb-M` = `lb-1`;
  `gf` = `wb-N` (unpinned)
- Test fixture rewrites (`"round": 3` → `"scope": "wb-3"`, separate `lb-*` and
  `gf` events)

**Out:**

- Migration of old tournament directories
- Server-side cascade / sparse anchor storage
- Per-match race-to override at ready time
- Bulk "copy winners → losers" button (cascade replaces it)
- Post-seed race-to changes
- Changing bracket match ID format (`wb-2-1` stays as-is)

## Not Doing (and Why)

- **`grandFinalRaceTo` + `Map[Int,Int]`** — insufficient; losers still coupled to
  winners at the same round number
- **Sentinel integers (0 = gf, 11+ = lb)** — fragile and opaque
- **Nested `{ winners: {}, losers: {}, grandFinal: N }` API** — readable JSON but
  two representations to keep in sync; string keys are simpler end-to-end
- **Flat wizard list at 64 players** — section headings essential at scale
- **Smart GF default (9/11)** — director bumps GF manually; helper text only

## Open Questions

- **Seed/race-to POST:** single payload with full `raceToByScope` map (one save
  action) — same as today, more keys. **Resolved: yes.**
- **`wb-1` → `lb-1` only (not `lb-2`..`lb-M`):** intentional; director uses
  `lb-1` to cascade all losers. **Resolved: yes.**

## Implementation Notes

**Client cascade sketch:**

```scala
raceToByScope: Var[Map[String, Int]]
gfPinned: Var[Boolean] = false

onEdit(scope, value):
  scope match
    case "wb-1" =>
      set wb-1..wb-N; set lb-1
      if !gfPinned => gf := wb-N
    case s"wb-$k" =>
      set wb-k..wb-N
      if !gfPinned => gf := wb-N
    case s"lb-$k" =>
      set lb-k..lb-M
    case "gf" =>
      gfPinned := true; set gf
```

**Codebase gap:** `MatchReady` only stores `handicapSuggested`; `BracketMatch.raceTo`
is never set during replay. Do not defer GF/losers separation to "override at
ready time."

**Docs drift:** update `docs/ideas/liga.md` race-to bullet when shipping — either
implement per-match override later or remove the escape-hatch claim.
