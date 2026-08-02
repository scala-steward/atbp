package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.glicko.Tuning
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

  private def handicapContext(
      frozenRatings: List[PlayerRating]
  ): BracketHandicapContext =
    BracketHandicapContext(frozenRatings, Map("wb-1" -> 7))

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
    },
    test("forMatch resolves preview from frozen ratings") {
      val frozen = List(
        jsRating(alice, 1700, 80),
        jsRating(bob, 1450, 90)
      )
      val matchDef = BracketMatch(
        id = "wb-1-1",
        playerA = Some(alice),
        playerB = Some(bob),
        state = BracketMatchState.Started,
        handicapApplied = Some(2)
      )
      assertTrue(
        MatchHandicapPreview
          .forMatch(handicapContext(frozen), matchDef)
          .isDefined,
        MatchHandicapPreview
          .forMatch(handicapContext(frozen), matchDef)
          .exists(_.weakerName == "Bob")
      )
    },
    test("fromMatch suggests zero when either player is unrated") {
      val tuning = Tuning.Default
      val guest = Player("Guest")
      val frozen = List(
        jsRating(
          guest,
          tuning.initRating,
          tuning.maxDeviation
        ),
        jsRating(bob, 1450, 90)
      )
      val matchDef = BracketMatch(
        id = "wb-1-1",
        playerA = Some(guest),
        playerB = Some(bob),
        state = BracketMatchState.Ready
      )
      val preview =
        MatchHandicapPreview.fromMatch(tournament(frozen), matchDef, 7)
      assertTrue(
        preview.isDefined,
        preview.exists(_.suggestedHandicap == 0)
      )
    }
  )
}
