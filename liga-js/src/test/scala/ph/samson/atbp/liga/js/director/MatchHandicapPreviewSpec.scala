package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.handicap.Handicap
import ph.samson.atbp.liga.js.api.Models.*
import ph.samson.atbp.liga.model as shared
import zio.test.*

object MatchHandicapPreviewSpec extends ZIOSpecDefault {

  private val alice = Player("Alice")
  private val bob = Player("Bob")

  private def jsRating(
      player: Player,
      rating: Double,
      rd: Double
  ): PlayerRating =
    PlayerRating(player, rating, rd, wins = 0, losses = 0)

  private def sharedRating(
      player: Player,
      rating: Double,
      rd: Double
  ): shared.PlayerRating =
    shared.PlayerRating(
      shared.Player(player.name),
      rating,
      rd,
      wins = 0,
      losses = 0
    )

  private def readyMatch: BracketMatch =
    readyMatchWithSuggestion(None)

  private def readyMatchWithSuggestion(
      handicapSuggested: Option[Int]
  ): BracketMatch =
    BracketMatch(
      id = "wb-1-1",
      playerA = Some(alice),
      playerB = Some(bob),
      state = BracketMatchState.Ready,
      handicapSuggested = handicapSuggested
    )

  private def tournament(
      frozenRatings: List[PlayerRating]
  ): TournamentResponse =
    TournamentResponse(
      name = "Test",
      players = List(alice, bob),
      completed = false,
      phase = "active",
      raceToByScope = Map("wb-1" -> 7),
      bracket = None,
      frozenRatings = frozenRatings
    )

  def spec = suite("MatchHandicapPreview")(
    test("fromMatch returns weaker-first ratings and shared suggestion") {
      val frozen = List(
        jsRating(alice, 1700, 80),
        jsRating(bob, 1450, 90)
      )
      val preview =
        MatchHandicapPreview.fromMatch(tournament(frozen), readyMatch, 7)
      val expected =
        Handicap.suggest(
          sharedRating(bob, 1450, 90),
          sharedRating(alice, 1700, 80),
          7
        )
      assertTrue(
        preview.isDefined,
        preview.exists(_.weaker.player.name == "Bob"),
        preview.exists(_.stronger.player.name == "Alice"),
        preview.exists(_.suggestion == expected),
        preview.exists(_.suggestedHandicap == expected.handicap)
      )
    },
    test("fromMatch is none when either player rating is missing") {
      val frozen = List(jsRating(alice, 1700, 80))
      assertTrue(
        MatchHandicapPreview
          .fromMatch(tournament(frozen), readyMatch, 7)
          .isEmpty
      )
    }
  )
}
