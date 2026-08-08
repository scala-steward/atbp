package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.js.api.Models.*
import zio.test.*

object RaceToLabelsSpec extends ZIOSpecDefault {

  def spec = suite("RaceToLabels")(
    test("roundHeaderLabel composes SE bare name with Race to N") {
      val raceToByScope = Map("se-2" -> 5)
      assertTrue(
        RaceToLabels.roundHeaderLabel(
          BracketLayout.Section.SingleElimination,
          2,
          raceToByScope,
          seRounds = 3
        ) == "Semifinals · Race to 5"
      )
    },
    test("roundHeaderLabel composes group label with Race to N") {
      val raceToByScope = Map("wb-2" -> 7)
      assertTrue(
        RaceToLabels.roundHeaderLabel(
          BracketLayout.Section.Winners,
          2,
          raceToByScope,
          seRounds = 0
        ) == "Winners — round 2 · Race to 7"
      )
    },
    test("roundHeaderLabel includes bug hint when scope is unresolved") {
      val label = RaceToLabels.roundHeaderLabel(
        BracketLayout.Section.Losers,
        3,
        Map.empty,
        seRounds = 0
      )
      assertTrue(
        label.contains("Losers — round 3"),
        label.contains("file a bug"),
        label.contains("lb-3")
      )
    },
    test("roundHeaderIsError is true only when scope is unresolved") {
      assertTrue(
        !RaceToLabels.roundHeaderIsError(
          BracketLayout.Section.Winners,
          2,
          Map("wb-2" -> 7)
        ),
        RaceToLabels.roundHeaderIsError(
          BracketLayout.Section.Losers,
          3,
          Map.empty
        )
      )
    },
    test("matchRaceToLabel returns Race to N when race-to resolves") {
      val matchDef = BracketMatch(
        id = "wb-1-1",
        playerA = Some(Player("P1")),
        playerB = Some(Player("P2")),
        state = BracketMatchState.Ready
      )
      assertTrue(
        RaceToLabels.matchRaceToLabel(matchDef, Map("wb-1" -> 7)) ==
          Right("Race to 7")
      )
    },
    test("matchRaceToLabel returns bug hint when race-to cannot resolve") {
      val matchDef = BracketMatch(
        id = "wb-2-1",
        playerA = Some(Player("P1")),
        playerB = Some(Player("P2")),
        state = BracketMatchState.Ready
      )
      val result = RaceToLabels.matchRaceToLabel(matchDef, Map.empty)
      assertTrue(
        result.isLeft,
        result.left.exists(_.contains("file a bug"))
      )
    }
  )
}
