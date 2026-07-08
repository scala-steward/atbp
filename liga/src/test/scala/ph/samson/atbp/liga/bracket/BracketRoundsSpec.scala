package ph.samson.atbp.liga.bracket

import zio.test.*

import scala.collection.immutable.SortedSet

object BracketRoundsSpec extends ZIOSpecDefault {

  def spec = suite("BracketRounds")(
    test("requiredKeys(8) returns rounds 1 through 4") {
      assertTrue(
        BracketRounds.requiredKeys(8) == SortedSet(1, 2, 3, 4)
      )
    },
    test("requiredKeys(16) returns rounds 1 through 6") {
      assertTrue(
        BracketRounds.requiredKeys(16) == SortedSet(1, 2, 3, 4, 5, 6)
      )
    },
    test("requiredKeys(64) returns rounds 1 through 10") {
      assertTrue(
        BracketRounds.requiredKeys(64) == SortedSet.from(1 to 10)
      )
    }
  )
}
