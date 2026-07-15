package ph.samson.atbp.liga.roster

import ph.samson.atbp.liga.glicko.Tuning
import ph.samson.atbp.liga.model.Player
import ph.samson.atbp.liga.model.PlayerRating
import zio.test.*

object RosterPasteSpec extends ZIOSpecDefault {

  private def pr(name: String, rating: Double, rd: Double): PlayerRating =
    PlayerRating(Player(name), rating, rd, wins = 0, losses = 0)

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
      },
      test("splits bare carriage returns from classic Mac newlines") {
        assertTrue(
          RosterPaste.parsePaste("Alice\rBob\r") == List("Alice", "Bob")
        )
      },
      test("empty paste yields no names") {
        assertTrue(RosterPaste.parsePaste("") == Nil)
      }
    ),
    suite("resolveRoster")(
      test("exact match keeps period rating; unknown is guest at initRating") {
        val period = Map(
          "Alice" -> pr("Alice", 1800, rd = 100),
          "Bob" -> pr("Bob", 1600, rd = 100)
        )
        val roster = RosterPaste.resolveRoster(
          List("Bob", "Carol", "Alice"),
          period
        )
        assertTrue(
          roster == List(
            RosterEntry("Alice", 1800, guest = false),
            RosterEntry("Bob", 1600, guest = false),
            RosterEntry("Carol", Tuning.Default.initRating, guest = true)
          )
        )
      },
      test("sorts by descending rating then RD then name") {
        val period = Map(
          "Zoe" -> pr("Zoe", 1500, rd = 100),
          "Amy" -> pr("Amy", 1500, rd = 100)
        )
        val roster = RosterPaste.resolveRoster(
          List("Zoe", "Amy", "Ned"),
          period
        )
        assertTrue(
          roster == List(
            RosterEntry("Amy", 1500, guest = false),
            RosterEntry("Zoe", 1500, guest = false),
            RosterEntry("Ned", Tuning.Default.initRating, guest = true)
          )
        )
      },
      test("case-sensitive: differing case is a guest") {
        val period = Map("Alice" -> pr("Alice", 1800, rd = 100))
        val roster = RosterPaste.resolveRoster(List("alice"), period)
        assertTrue(
          roster == List(
            RosterEntry("alice", Tuning.Default.initRating, guest = true)
          )
        )
      },
      test("same rating: lower RD ranks higher") {
        val period = Map(
          "Alice" -> pr("Alice", 1500, rd = 100),
          "Bob" -> pr("Bob", 1500, rd = 200)
        )
        val roster = RosterPaste.resolveRoster(List("Bob", "Alice"), period)
        assertTrue(
          roster == List(
            RosterEntry("Alice", 1500, guest = false),
            RosterEntry("Bob", 1500, guest = false)
          )
        )
      },
      test("guest at initRating loses RD tie to period player at same rating") {
        val tuning = Tuning.Default
        val period = Map("Carol" -> pr("Carol", tuning.initRating, rd = 100))
        val roster = RosterPaste.resolveRoster(List("Ned", "Carol"), period)
        assertTrue(
          roster == List(
            RosterEntry("Carol", tuning.initRating, guest = false),
            RosterEntry("Ned", tuning.initRating, guest = true)
          )
        )
      }
    )
  )
}
