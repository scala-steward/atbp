package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.glicko.Tuning
import ph.samson.atbp.liga.js.api.Models.*
import ph.samson.atbp.liga.model as shared
import zio.test.*

object ReadyHandicapPolicySpec extends ZIOSpecDefault {

  private val alice = Player("Alice")
  private val bob = Player("Bob")
  private val guest = Player("Guest")

  private val tuning = Tuning.Default

  private def jsRating(
      player: Player,
      rating: Double,
      rd: Double,
      wins: Int,
      losses: Int
  ): PlayerRating =
    PlayerRating(player, rating, rd, wins = wins, losses = losses)

  private val ratedFrozen = List(
    jsRating(alice, 1700, 80, wins = 0, losses = 0),
    jsRating(bob, 1450, 90, wins = 0, losses = 0)
  )

  private val guestFrozen = List(
    jsRating(
      guest,
      tuning.initRating,
      tuning.maxDeviation,
      wins = 0,
      losses = 0
    ),
    jsRating(bob, 1450, 90, wins = 0, losses = 0)
  )

  private def readyMatch(
      handicapSuggested: Option[Int],
      playerA: Player,
      playerB: Player
  ): BracketMatch =
    BracketMatch(
      id = "wb-1-1",
      playerA = Some(playerA),
      playerB = Some(playerB),
      state = BracketMatchState.Ready,
      handicapSuggested = handicapSuggested
    )

  private val weaker =
    shared.PlayerRating(shared.Player("Bob"), 1450, 90, wins = 0, losses = 0)
  private val stronger =
    shared.PlayerRating(shared.Player("Alice"), 1700, 80, wins = 0, losses = 0)
  private val preview = MatchHandicapPreview(
    weaker,
    stronger,
    shared.HandicapSuggestion(shared.Player("Bob"), handicap = 3, raceTo = 7)
  )

  def spec = suite("ReadyHandicapPolicy")(
    test("previewWaitingMessage is the ratings-missing copy") {
      assertTrue(
        ReadyHandicapPolicy.previewWaitingMessage ==
          "Waiting for player ratings to compute a preview."
      )
    },
    test("surface is Preview with client suggestion when ratings exist") {
      assertTrue(
        ReadyHandicapPolicy.surface(
          readyMatch(None, alice, bob),
          ratedFrozen,
          Some(preview)
        ) ==
          ReadyHandicapPolicy.Surface.Preview(preview)
      )
    },
    test("surface is PreviewWaiting before Ready when ratings are missing") {
      assertTrue(
        ReadyHandicapPolicy.surface(
          readyMatch(None, alice, bob),
          ratedFrozen,
          None
        ) ==
          ReadyHandicapPolicy.Surface.PreviewWaiting
      )
    },
    test("surface is Adjust with preview once server suggestion exists") {
      assertTrue(
        ReadyHandicapPolicy.surface(
          readyMatch(Some(3), alice, bob),
          ratedFrozen,
          Some(preview)
        ) ==
          ReadyHandicapPolicy.Surface.Adjust(suggested = 3, Some(preview))
      )
    },
    test("surface Adjust keeps controls while waiting for ratings") {
      assertTrue(
        ReadyHandicapPolicy.surface(
          readyMatch(Some(3), alice, bob),
          ratedFrozen,
          None
        ) ==
          ReadyHandicapPolicy.Surface.Adjust(suggested = 3, None)
      )
    },
    test("surface Adjust anchors neighborhood on server suggestion") {
      assertTrue(
        ReadyHandicapPolicy.surface(
          readyMatch(Some(4), alice, bob),
          ratedFrozen,
          Some(preview)
        ) ==
          ReadyHandicapPolicy.Surface.Adjust(suggested = 4, Some(preview))
      )
    },
    test("surface is ZeroLocked after Ready when either player is unrated") {
      val matchDef = readyMatch(
        handicapSuggested = Some(0),
        playerA = guest,
        playerB = bob
      )
      assertTrue(
        ReadyHandicapPolicy.surface(matchDef, guestFrozen, Some(preview)) ==
          ReadyHandicapPolicy.Surface.ZeroLocked(suggested = 0, Some(preview))
      )
    },
    test("surface stays Adjust when rated pair legitimately applies zero") {
      val matchDef = readyMatch(handicapSuggested = Some(0), alice, bob)
      assertTrue(
        ReadyHandicapPolicy.surface(
          matchDef,
          ratedFrozen,
          Some(preview)
        ) ==
          ReadyHandicapPolicy.Surface.Adjust(suggested = 0, Some(preview))
      )
    },
    test("requiresZeroHandicap is true when guest profile is frozen") {
      assertTrue(
        ReadyHandicapPolicy.requiresZeroHandicap(
          readyMatch(None, playerA = guest, playerB = bob),
          guestFrozen
        )
      )
    }
  )
}
