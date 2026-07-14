package ph.samson.atbp.liga.roster

import zio.test.*

object RosterPasteSpec extends ZIOSpecDefault {

  def spec = suite("RosterPaste")(
    suite("parsePaste")(
      test("trims lines and drops empties") {
        assertTrue(
          RosterPaste.parsePaste("  Alice\n\nBob  \n\n") == List("Alice", "Bob")
        )
      },
      test("collapses exact duplicates first-wins") {
        assertTrue(
          RosterPaste.parsePaste("Alice\nBob\nAlice") == List("Alice", "Bob")
        )
      },
      test("strips carriage returns from Windows newlines") {
        assertTrue(
          RosterPaste.parsePaste("Alice\r\nBob\r\n") == List("Alice", "Bob")
        )
      }
    ),
    suite("resolveRoster")(
      test("exact match keeps period rating; unknown is guest at 1500") {
        val period = Map("Alice" -> 1800.0, "Bob" -> 1600.0)
        val roster = RosterPaste.resolveRoster(
          List("Bob", "Carol", "Alice"),
          period
        )
        assertTrue(
          roster == List(
            RosterEntry("Alice", 1800, guest = false),
            RosterEntry("Bob", 1600, guest = false),
            RosterEntry("Carol", RosterPaste.GuestDisplayRating, guest = true)
          )
        )
      },
      test("sorts by descending rating then name") {
        val period = Map("Zoe" -> 1500.0, "Amy" -> 1500.0)
        val roster = RosterPaste.resolveRoster(
          List("Zoe", "Amy", "Ned"),
          period
        )
        assertTrue(
          roster == List(
            RosterEntry("Amy", 1500, guest = false),
            RosterEntry("Ned", RosterPaste.GuestDisplayRating, guest = true),
            RosterEntry("Zoe", 1500, guest = false)
          )
        )
      },
      test("case-sensitive: differing case is a guest") {
        val period = Map("Alice" -> 1800.0)
        val roster = RosterPaste.resolveRoster(List("alice"), period)
        assertTrue(
          roster == List(
            RosterEntry("alice", RosterPaste.GuestDisplayRating, guest = true)
          )
        )
      }
    )
  )
}
