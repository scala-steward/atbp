package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.glicko.Tuning
import ph.samson.atbp.liga.js.api.Models.Player
import ph.samson.atbp.liga.js.api.Models.PlayerRating
import ph.samson.atbp.liga.model.PlayerRatingLabel
import zio.test.*

object BracketHandicapContextSpec extends ZIOSpecDefault {

  private val tuning = Tuning.Default
  private val alice = Player("Alice")
  private val guest = Player("Guest")

  private def jsRating(
      player: Player,
      rating: Double,
      rd: Double,
      wins: Int,
      losses: Int
  ): PlayerRating =
    PlayerRating(player, rating, rd, wins, losses)

  private def context(
      frozenRatings: List[PlayerRating]
  ): BracketHandicapContext =
    BracketHandicapContext(frozenRatings, Map.empty)

  def spec = suite("BracketHandicapContext")(
    suite("ratingLabelFor")(
      test("returns Some(Rated) for period player in frozenRatings") {
        val frozen = List(
          jsRating(alice, 1543, rd = 80, wins = 5, losses = 3)
        )
        assertTrue(
          context(frozen).ratingLabelFor("Alice") ==
            Some(PlayerRatingLabel.Rated(1543))
        )
      },
      test("returns Some(Unrated) for guest seed profile") {
        val frozen = List(
          jsRating(
            guest,
            tuning.initRating,
            tuning.maxDeviation,
            wins = 0,
            losses = 0
          )
        )
        assertTrue(
          context(frozen).ratingLabelFor("Guest") ==
            Some(PlayerRatingLabel.Unrated)
        )
      },
      test("returns None for unknown player name") {
        val frozen = List(
          jsRating(alice, 1543, rd = 80, wins = 0, losses = 0)
        )
        assertTrue(context(frozen).ratingLabelFor("Unknown").isEmpty)
      },
      test("returns None for TBD slot") {
        val frozen = List(
          jsRating(alice, 1543, rd = 80, wins = 0, losses = 0)
        )
        assertTrue(context(frozen).ratingLabelFor("—").isEmpty)
      },
      test("returns None when frozenRatings is empty") {
        assertTrue(context(Nil).ratingLabelFor("Alice").isEmpty)
      }
    )
  )
}
