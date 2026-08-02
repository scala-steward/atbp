package ph.samson.atbp.liga.handicap

import ph.samson.atbp.liga.glicko.Tuning
import ph.samson.atbp.liga.model.*
import zio.test.*

object HandicapSpec extends ZIOSpecDefault {

  private val tuning = Tuning.Default

  private def rating(
      name: String,
      rating: Double,
      rd: Double,
      wins: Int,
      losses: Int
  ): PlayerRating =
    PlayerRating(Player(name), rating, rd, wins, losses)

  def spec = suite("Handicap")(
    test("requiresZeroHandicap is true when either player is unrated") {
      val unrated = rating(
        "Guest",
        tuning.initRating,
        tuning.maxDeviation,
        wins = 0,
        losses = 0
      )
      val rated = rating("Alice", 1500, rd = 100, wins = 0, losses = 0)
      assertTrue(
        Handicap.requiresZeroHandicap(unrated, rated),
        Handicap.requiresZeroHandicap(rated, unrated),
        Handicap.requiresZeroHandicap(unrated, unrated)
      )
    },
    test("requiresZeroHandicap is false when both players are rated") {
      val ratedA = rating("Alice", 1500, rd = 100, wins = 0, losses = 0)
      val ratedB = rating("Bob", 1543, rd = 80, wins = 12, losses = 8)
      assertTrue(!Handicap.requiresZeroHandicap(ratedA, ratedB))
    },
    test("probabilityNeighborhoodSpots is a distinct ordered neighborhood") {
      assertTrue(
        Handicap.probabilityNeighborhoodSpots(0) == List(0, 1),
        Handicap.probabilityNeighborhoodSpots(1) == List(0, 1, 2),
        Handicap.probabilityNeighborhoodSpots(3) == List(0, 2, 3, 4)
      )
    }
  )
}
