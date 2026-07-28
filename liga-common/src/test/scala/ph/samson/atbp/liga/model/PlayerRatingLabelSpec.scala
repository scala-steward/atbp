package ph.samson.atbp.liga.model

import ph.samson.atbp.liga.glicko.Tuning
import zio.test.*

object PlayerRatingLabelSpec extends ZIOSpecDefault {

  private val tuning = Tuning.Default

  private def rating(
      name: String,
      rating: Double,
      rd: Double,
      wins: Int,
      losses: Int
  ): PlayerRating =
    PlayerRating(Player(name), rating, rd, wins, losses)

  def spec = suite("PlayerRatingLabel")(
    test("fromFrozen returns Unrated for guest seed profile") {
      val guest = rating(
        "Guest",
        tuning.initRating,
        tuning.maxDeviation,
        wins = 0,
        losses = 0
      )
      assertTrue(
        PlayerRatingLabel.fromFrozen(guest) == PlayerRatingLabel.Unrated
      )
    },
    test(
      "fromFrozen returns Rated(1500) for period player at init rating with lower RD"
    ) {
      val period = rating("Alice", 1500, rd = 100, wins = 0, losses = 0)
      assertTrue(
        PlayerRatingLabel.fromFrozen(period) == PlayerRatingLabel.Rated(1500)
      )
    },
    test("fromFrozen returns Rated for established player with W-L record") {
      val veteran = rating("Bob", 1543, rd = 80, wins = 12, losses = 8)
      assertTrue(
        PlayerRatingLabel.fromFrozen(veteran) == PlayerRatingLabel.Rated(1543)
      )
    },
    test("fromFrozen returns Rated when max RD but non-zero wins") {
      val rated = rating(
        "Carol",
        tuning.initRating,
        tuning.maxDeviation,
        wins = 1,
        losses = 0
      )
      assertTrue(
        PlayerRatingLabel.fromFrozen(rated) == PlayerRatingLabel.Rated(
          tuning.initRating
        )
      )
    }
  )
}
