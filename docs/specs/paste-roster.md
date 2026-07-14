# Spec: Paste roster on define

## Objective

Replace the defining-step checkbox / one-guest-at-a-time UX with a paste-first
flow: the director pastes a newline-separated signup list (returning players and
guests mixed), sees the resulting roster sorted by descending rating with guests
highlighted, then saves and locks as today.

**User story:** As a director, I paste the signup name list as-is and visually
confirm who is rated vs new before locking.

## Assumptions

1. Guests display with the same default rating used at seed (`initRating` 1500)
   for sort/display only; seed behavior is unchanged.
2. Paste parsing: split on newlines, trim each line, drop empty lines; exact
   duplicate names in the paste are collapsed (first wins) so Save is not blocked
   by a careless double line.
3. Exact match means exact string equality with `PlayerRating.player.name` on the
   period leaderboard response already loaded in the wizard.
4. When resuming a defining tournament that already has saved players, the roster
   list is rebuilt from those names the same way (match leaderboard → rated;
   else guest).
5. Save roster / Lock roster actions and server validation (8–64 on lock,
   exact duplicate reject on save) stay as they are.
6. No backend or API changes.

→ Correct these in review if wrong.

## Tech Stack

- Scala.js + Laminar director UI (`liga-js`)
- Period ratings from existing `LeaderboardResponse`
- CSS in `liga/src/main/resources/liga/web/css/director.css`
- No new dependencies

## Commands

```bash
sbt --client "ligaJS/test"
sbt --client "liga/test"
sbt --client fixup
```

Manual check: `sbt run` (liga serve), open director `/`, create tournament, paste list.

## Project Structure

```
docs/intent/paste-roster.md          → confirmed intent
docs/specs/paste-roster.md           → this spec
liga-js/.../director/WizardView.scala → defining-step UI
liga-js/.../director/DirectorGuidance.scala → copy/hints if needed
liga/.../web/css/director.css        → roster list + guest highlight styles
liga-js/src/test/...                 → pure logic tests for parse/match/sort
```

## Code Style

Prefer a small pure helper for parse → resolve → sort, then wire it in Laminar.
Match existing `WizardView` / `DirectorGuidance` style.

Example shape (illustrative):

```scala
final case class RosterEntry(name: String, rating: Double, guest: Boolean)

def parsePaste(raw: String): List[String] =
  raw.split("\n").iterator.map(_.trim).filter(_.nonEmpty).toList.distinct

def resolveRoster(
    names: List[String],
    period: List[PlayerRating],
    guestRating: Double
): List[RosterEntry] = {
  val byName = period.map(r => r.player.name -> r.rating).toMap
  names
    .map { name =>
      byName.get(name) match {
        case Some(rating) => RosterEntry(name, rating, guest = false)
        case None         => RosterEntry(name, guestRating, guest = true)
      }
    }
    .sortBy(e => (-e.rating, e.name))
}
```

## Testing Strategy

- Unit-test pure parse / resolve / sort (empty lines, duplicates, exact match,
  guest default rating, descending order, name tie-break).
- No new HTTP contract tests (API unchanged).
- Manual: paste mixed list → guests highlighted → save → lock.

## Boundaries

- **Always:** Keep save/lock API and 8–64 lock rules; exact name match only;
  run `sbt --client fixup` before any Scala commit.
- **Ask first:** Changing guest display rating away from `initRating`; restoring
  checkbox UX; any server/validation change.
- **Never:** Fuzzy matching; file upload; merge paste; invent ratings other than
  period or default guest.

## Success Criteria

1. Defining step has a multiline paste control and no period checkboxes / single
   guest text field.
2. Applying paste replaces the local roster with parsed names (trim, drop empties,
   collapse exact duplicates).
3. Roster list shows each name with rating, sorted descending (then name);
   guests use default 1500 and a clear visual highlight.
4. Count + lock hint (8–64) still drive Lock enablement; Save/Lock still call
   existing observers/API.
5. Unit tests cover parse/resolve/sort; `ligaJS/test` passes.

## Open Questions

None blocking — assumptions above are the review gate.
