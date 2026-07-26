package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.js.api.Models.*
import ph.samson.atbp.liga.model as shared
import zio.test.*

object ReadyHandicapPolicySpec extends ZIOSpecDefault {

  private val alice = Player("Alice")
  private val bob = Player("Bob")

  private def readyMatch(
      handicapSuggested: Option[Int]
  ): BracketMatch =
    BracketMatch(
      id = "wb-1-1",
      playerA = Some(alice),
      playerB = Some(bob),
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
        ReadyHandicapPolicy.surface(readyMatch(None), Some(preview)) ==
          ReadyHandicapPolicy.Surface.Preview(preview)
      )
    },
    test("surface is PreviewWaiting before Ready when ratings are missing") {
      assertTrue(
        ReadyHandicapPolicy.surface(readyMatch(None), None) ==
          ReadyHandicapPolicy.Surface.PreviewWaiting
      )
    },
    test("surface is Adjust with preview once server suggestion exists") {
      assertTrue(
        ReadyHandicapPolicy.surface(readyMatch(Some(3)), Some(preview)) ==
          ReadyHandicapPolicy.Surface.Adjust(suggested = 3, Some(preview))
      )
    },
    test("surface Adjust keeps controls while waiting for ratings") {
      assertTrue(
        ReadyHandicapPolicy.surface(readyMatch(Some(3)), None) ==
          ReadyHandicapPolicy.Surface.Adjust(suggested = 3, None)
      )
    },
    test("surface Adjust anchors neighborhood on server suggestion") {
      assertTrue(
        ReadyHandicapPolicy.surface(readyMatch(Some(4)), Some(preview)) ==
          ReadyHandicapPolicy.Surface.Adjust(suggested = 4, Some(preview))
      )
    }
  )
}
