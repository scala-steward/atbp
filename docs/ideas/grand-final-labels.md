# Grand Final Labels

## Problem Statement

How might we label bracket sections consistently so Grand Final never shows a round
number, while winners and losers rounds keep their numeric labels?

## Recommended Direction

Add `BracketLayout.groupLabel(section, round)` as the single display helper for
bracket section headers and match labels. Grand Final returns `"Grand Final"` with
no round suffix; all other sections use `"Winners — round N"` / `"Losers — round N"`.

Both `BracketView` (director) and `AudienceBracketView` (audience) call
`groupLabel` instead of interpolating `section.label` and `round` directly.
`matchLabel` delegates to `groupLabel`, so section headers and per-match labels
stay in sync.

Internal `RoundGroup.round` remains numeric (`log2(bracketSize)` for `gf-1`) for
sorting and grouping only — display logic is separate from data logic.

## Key Assumptions to Validate

- [x] **Grand Final should never show a round number** — regression tests on
      `groupLabel` and `matchLabel`
- [ ] **`"Winners — round N"` is the right format for non-GF sections** — matches
      existing `matchLabel` behavior; confirm with a director during live use
- [ ] **Wizard and bracket views can keep different section names for now**
      (`"Winners Bracket"` in race-to wizard vs `"Winners"` in bracket views) —
      unifying on `RaceToScopes` is a separate follow-up

## MVP Scope

**In:**

- `BracketLayout.groupLabel(section, round)`
- `BracketView` and `AudienceBracketView` section headers use `groupLabel`
- `matchLabel` delegates to `groupLabel`
- Unit tests in `BracketLayoutSpec`

**Out:**

- Merging `BracketLayout.Section` with `RaceToScopes.Section`
- Semantic round names ("Semifinal", "Final")
- i18n / label-key indirection layer
- Bracket tree visualization redesign

## Not Doing (and Why)

- **Unify section names with race-to wizard** — correct long-term (Direction B) but
  scope creep for a display bug; track separately
- **Semantic finals labels** — product decision, not a bug fix; race-to wizard uses
  "Round N" throughout
- **Change internal round numbering for `gf-1`** — `round` is a grouping key, not a
  display string; changing it would break sort order semantics

## Open Questions

- Should section names eventually match the wizard ("Winners Bracket" vs "Winners")?
- Do late-round winners/losers matches deserve semantic names in a future pass?
