package ph.samson.atbp.liga.glicko

import better.files.File
import ph.samson.atbp.liga.io.PeriodLoader
import ph.samson.atbp.liga.model.PlayerRating
import zio.test.*

object LeaderboardSpec extends ZIOSpecDefault {

  private def fixture(name: String): File =
    File(getClass.getResource(s"/period-loader/$name"))

  private def approx(expected: Double, actual: Double): Boolean =
    Math.abs(expected - actual) <= 0.001

  private def ratingOf(
      ratings: List[PlayerRating],
      name: String
  ): PlayerRating =
    ratings.find(_.player.name == name).get

  def spec = suite("Leaderboard")(
    test("golden fixture batch semantics across two periods") {
      // Period 1 (2026-01-10): Alice beats Bob 7-4 — both debut.
      // Period 2 (2026-03-15): Carol beats Alice 4-7 — Bob sits out.
      // Batch semantics: Bob's RD must inflate in period 2 even though he
      // does not appear in that file; match order within each file is irrelevant.
      val root = fixture("golden")
      for {
        loaded <- PeriodLoader.discover(root)
        afterPeriodOne = Leaderboard.compute(loaded.take(1).map(_.period))
        afterPeriodTwo = Leaderboard.compute(loaded.map(_.period))
      } yield {
        val aliceAfterOne = ratingOf(afterPeriodOne, "Alice")
        val bobAfterOne = ratingOf(afterPeriodOne, "Bob")
        val aliceAfterTwo = ratingOf(afterPeriodTwo, "Alice")
        val bobAfterTwo = ratingOf(afterPeriodTwo, "Bob")
        val carolAfterTwo = ratingOf(afterPeriodTwo, "Carol")

        assertTrue(
          approx(1618.025, aliceAfterOne.rating),
          approx(142.931, aliceAfterOne.rd),
          aliceAfterOne.wins == 7,
          aliceAfterOne.losses == 4,
          approx(1381.975, bobAfterOne.rating),
          approx(142.931, bobAfterOne.rd),
          bobAfterOne.wins == 4,
          bobAfterOne.losses == 7,
          approx(1497.830, aliceAfterTwo.rating),
          approx(106.937, aliceAfterTwo.rd),
          aliceAfterTwo.wins == 11,
          aliceAfterTwo.losses == 11,
          bobAfterTwo.rd > bobAfterOne.rd,
          approx(1381.975, bobAfterTwo.rating),
          approx(143.311, bobAfterTwo.rd),
          bobAfterTwo.wins == bobAfterOne.wins,
          bobAfterTwo.losses == bobAfterOne.losses,
          approx(1714.583, carolAfterTwo.rating),
          approx(114.008, carolAfterTwo.rd),
          carolAfterTwo.wins == 7,
          carolAfterTwo.losses == 4
        )
      }
    }
  )
}
