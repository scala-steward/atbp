package ph.samson.atbp.liga.js.audience

import ph.samson.atbp.liga.js.api.Models.*
import ph.samson.atbp.liga.js.api.Models.BracketMatchState
import zio.test.*

object AudienceSpatialBracketViewSpec extends ZIOSpecDefault {

  private val playerA = Some(Player("Alice"))
  private val playerB = Some(Player("Bob"))

  def spec = suite("AudienceSpatialBracketView")(
    suite("cellStateClass")(
      test("started matches get live chrome") {
        assertTrue(
          AudienceSpatialBracketView.cellStateClass(
            BracketMatchState.Started
          ) == "spatial-cell-live"
        )
      },
      test("completed matches get done chrome") {
        assertTrue(
          AudienceSpatialBracketView.cellStateClass(
            BracketMatchState.Completed
          ) == "spatial-cell-done"
        )
      },
      test("pending matches have no state chrome") {
        assertTrue(
          AudienceSpatialBracketView
            .cellStateClass(
              BracketMatchState.Pending
            )
            .isEmpty
        )
      }
    ),
    suite("cellClassList")(
      test("combines base spatial-cell with state chrome") {
        assertTrue(
          AudienceSpatialBracketView.cellClassList(
            BracketMatchState.Started
          ) == List("spatial-cell", "spatial-cell-live")
        )
      },
      test("pending keeps only base class") {
        assertTrue(
          AudienceSpatialBracketView.cellClassList(
            BracketMatchState.Pending
          ) == List("spatial-cell")
        )
      }
    ),
    suite("spatialUsesInlineResult")(
      test("completed matches inline score or bye") {
        val completed = BracketMatch(
          id = "wb-1-1",
          playerA = playerA,
          playerB = playerB,
          state = BracketMatchState.Completed,
          result = Some(MatchResult(scoreA = 7, scoreB = 4))
        )
        val bye = BracketMatch(
          id = "wb-1-2",
          playerA = playerA,
          playerB = None,
          state = BracketMatchState.Completed,
          isBye = true
        )
        assertTrue(
          AudienceSpatialBracketView.spatialUsesInlineResult(completed),
          AudienceSpatialBracketView.spatialUsesInlineResult(bye)
        )
      },
      test("unfinished matches keep vs separator") {
        val started = BracketMatch(
          id = "wb-1-1",
          playerA = playerA,
          playerB = playerB,
          state = BracketMatchState.Started
        )
        assertTrue(
          !AudienceSpatialBracketView.spatialUsesInlineResult(started)
        )
      }
    )
  )
}
