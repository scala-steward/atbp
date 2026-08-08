package ph.samson.atbp.liga.js.audience

import ph.samson.atbp.liga.bracket.BracketFormat
import ph.samson.atbp.liga.js.director.BracketLayout
import ph.samson.atbp.liga.js.director.RaceToLabels
import zio.test.*

/** Audience bracket headers use the same SE naming as the director view.
  *
  * Depth matches `AudienceApp` / `DirectorApp`:
  * `BracketFormat.forBracket(bracket.size, topN).seRounds`.
  */
object AudienceBracketLabelsSpec extends ZIOSpecDefault {

  def spec = suite("AudienceBracketLabels")(
    test("full SE 8 audience headers match director bare names") {
      // Same derivation as AudienceApp(bracket.size, t.topN).
      val seRounds =
        BracketFormat.forBracket(bracketSize = 8, topN = 8).seRounds
      val raceTo = Map("se-1" -> 7, "se-2" -> 7, "se-3" -> 9)
      assertTrue(
        seRounds == 3,
        RaceToLabels.roundHeaderLabel(
          BracketLayout.Section.SingleElimination,
          1,
          raceTo,
          seRounds
        ) == "Quarterfinals · Race to 7",
        RaceToLabels.roundHeaderLabel(
          BracketLayout.Section.SingleElimination,
          3,
          raceTo,
          seRounds
        ) == "Grand Final · Race to 9"
      )
    },
    test("cut Top 8 on 16-slot bracket uses Top 8 SE depth") {
      val seRounds =
        BracketFormat.forBracket(bracketSize = 16, topN = 8).seRounds
      assertTrue(
        seRounds == 3,
        RaceToLabels.roundHeaderLabel(
          BracketLayout.Section.SingleElimination,
          1,
          Map("se-1" -> 7),
          seRounds
        ) == "Quarterfinals · Race to 7"
      )
    },
    test("classic DE audience headers stay Winners — round N") {
      val seRounds =
        BracketFormat.forBracket(bracketSize = 8, topN = 2).seRounds
      assertTrue(
        seRounds == 0,
        RaceToLabels.roundHeaderLabel(
          BracketLayout.Section.Winners,
          2,
          Map("wb-2" -> 7),
          seRounds
        ) == "Winners — round 2 · Race to 7"
      )
    }
  )
}
