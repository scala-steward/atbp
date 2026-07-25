package ph.samson.atbp.liga.glicko

import better.files.File
import ph.samson.atbp.liga.glicko.Tuning.Default
import ph.samson.atbp.liga.io.PeriodLoader
import zio.test.*

object LatestRatingsSpec extends ZIOSpecDefault {

  private def fixture(name: String): File =
    File(getClass.getResource(s"/period-loader/$name"))

  private def approx(expected: Double, actual: Double): Boolean =
    Math.abs(expected - actual) <= 0.001

  private def ratingOf(
      ratings: List[LatestRating],
      name: String
  ): LatestRating =
    ratings.find(_.player.name == name).get

  def spec = suite("LatestRatings")(
    test("empty period list returns empty result") {
      assertTrue(LatestRatings.fromPeriods(Nil).isEmpty)
    },
    test("golden fixture: only last-period participants with correct deltas") {
      // Period 1 (2026-01-10): Alice beats Bob 7-4 — both debut.
      // Period 2 (2026-03-15): Carol beats Alice 4-7 — Bob sits out.
      val root = fixture("golden")
      for {
        loaded <- PeriodLoader.discover(root)
        periods = loaded.map(_.period)
        afterPeriodOne = Leaderboard.compute(periods.take(1))
        afterPeriodTwo = Leaderboard.compute(periods)
        latest = LatestRatings.fromPeriods(periods)
      } yield {
        val aliceBefore =
          afterPeriodOne.find(_.player.name == "Alice").get.rating
        val aliceAfter =
          afterPeriodTwo.find(_.player.name == "Alice").get.rating
        val carolAfter =
          afterPeriodTwo.find(_.player.name == "Carol").get.rating
        val alice = ratingOf(latest, "Alice")
        val carol = ratingOf(latest, "Carol")

        assertTrue(
          latest.map(_.player.name).sorted == List("Alice", "Carol"),
          !latest.exists(_.player.name == "Bob"),
          approx(aliceAfter, alice.rating),
          approx(aliceAfter - aliceBefore, alice.delta),
          approx(carolAfter, carol.rating),
          approx(carolAfter - Default.initRating, carol.delta),
          latest == latest.sortBy(-_.rating)
        )
      }
    },
    test("single period: before rating is init rating") {
      val root = fixture("golden")
      for {
        loaded <- PeriodLoader.discover(root)
        periods = loaded.take(1).map(_.period)
        latest = LatestRatings.fromPeriods(periods)
      } yield {
        val alice = ratingOf(latest, "Alice")
        val bob = ratingOf(latest, "Bob")
        assertTrue(
          latest.map(_.player.name).sorted == List("Alice", "Bob"),
          approx(alice.rating - Default.initRating, alice.delta),
          approx(bob.rating - Default.initRating, bob.delta)
        )
      }
    }
  )
}
