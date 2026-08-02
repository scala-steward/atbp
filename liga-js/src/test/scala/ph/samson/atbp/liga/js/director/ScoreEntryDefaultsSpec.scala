package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.js.api.Models.MatchResult
import ph.samson.atbp.liga.js.director.BracketLayout.AppliedHandicapDisplay
import ph.samson.atbp.liga.js.director.BracketLayout.AppliedHandicapSide
import zio.test.*

object ScoreEntryDefaultsSpec extends ZIOSpecDefault {

  def spec = suite("ScoreEntryDefaults")(
    suite("initialScoreStrings")(
      test("no result and spot on A initializes handicap floors") {
        val display = AppliedHandicapDisplay.Placed(
          spot = 3,
          side = AppliedHandicapSide.PlayerA,
          weakerName = "Alice"
        )
        assertTrue(
          ScoreEntryDefaults.initialScoreStrings(display, None) == ("3", "0")
        )
      },
      test("no result and spot on B initializes handicap floors") {
        val display = AppliedHandicapDisplay.Placed(
          spot = 2,
          side = AppliedHandicapSide.PlayerB,
          weakerName = "Bob"
        )
        assertTrue(
          ScoreEntryDefaults.initialScoreStrings(display, None) == ("0", "2")
        )
      },
      test("no result with unresolved or hidden display initializes zero") {
        assertTrue(
          ScoreEntryDefaults.initialScoreStrings(
            AppliedHandicapDisplay.Unresolved(3),
            None
          ) == ("0", "0"),
          ScoreEntryDefaults.initialScoreStrings(
            AppliedHandicapDisplay.Hidden,
            None
          ) == ("0", "0")
        )
      },
      test("existing result keeps recorded scores") {
        val display = AppliedHandicapDisplay.Placed(
          spot = 3,
          side = AppliedHandicapSide.PlayerA,
          weakerName = "Alice"
        )
        assertTrue(
          ScoreEntryDefaults.initialScoreStrings(
            display,
            Some(MatchResult(scoreA = 7, scoreB = 5))
          ) == ("7", "5")
        )
      }
    ),
    suite("floors")(
      test("spotted player A starts at handicap, B at zero") {
        assertTrue(
          ScoreEntryDefaults.floors(3, Some(AppliedHandicapSide.PlayerA)) ==
            ScoreEntryDefaults.ScoreFloors(scoreA = 3, scoreB = 0)
        )
      },
      test("spotted player B starts at handicap, A at zero") {
        assertTrue(
          ScoreEntryDefaults.floors(2, Some(AppliedHandicapSide.PlayerB)) ==
            ScoreEntryDefaults.ScoreFloors(scoreA = 0, scoreB = 2)
        )
      },
      test("handicap zero yields zero floors regardless of side") {
        assertTrue(
          ScoreEntryDefaults.floors(0, Some(AppliedHandicapSide.PlayerA)) ==
            ScoreEntryDefaults.ScoreFloors(scoreA = 0, scoreB = 0),
          ScoreEntryDefaults.floors(0, Some(AppliedHandicapSide.PlayerB)) ==
            ScoreEntryDefaults.ScoreFloors(scoreA = 0, scoreB = 0)
        )
      },
      test("unresolved spotted side yields zero floors") {
        assertTrue(
          ScoreEntryDefaults.floors(3, None) ==
            ScoreEntryDefaults.ScoreFloors(scoreA = 0, scoreB = 0)
        )
      }
    ),
    suite("clampOnBlur")(
      test("below floor snaps to floor") {
        assertTrue(ScoreEntryDefaults.clampOnBlur("2", 3) == "3")
      },
      test("at or above floor keeps normalized integer string") {
        assertTrue(
          ScoreEntryDefaults.clampOnBlur("3", 3) == "3",
          ScoreEntryDefaults.clampOnBlur("21", 3) == "21"
        )
      },
      test("empty or non-numeric snaps to floor") {
        assertTrue(
          ScoreEntryDefaults.clampOnBlur("", 3) == "3",
          ScoreEntryDefaults.clampOnBlur("abc", 0) == "0"
        )
      }
    ),
    suite("clampPair")(
      test("clamps each side to its floor for submit without blur") {
        val floors = ScoreEntryDefaults.ScoreFloors(scoreA = 3, scoreB = 0)
        assertTrue(
          ScoreEntryDefaults.clampPair("2", "-1", floors) == ("3", "0"),
          ScoreEntryDefaults.clampPair("", "abc", floors) == ("3", "0"),
          ScoreEntryDefaults.clampPair("7", "5", floors) == ("7", "5")
        )
      }
    )
  )
}
