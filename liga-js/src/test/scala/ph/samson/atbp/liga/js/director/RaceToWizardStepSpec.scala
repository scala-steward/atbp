package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.bracket.RaceToScopes
import ph.samson.atbp.liga.bracket.RaceToWizard
import ph.samson.atbp.liga.bracket.TopN
import ph.samson.atbp.liga.js.api.Models.*
import zio.test.*

object RaceToWizardStepSpec extends ZIOSpecDefault {

  private def lockedTournament(
      playerCount: Int,
      topN: Int,
      raceToByScope: Map[String, Int]
  ): TournamentResponse =
    TournamentResponse(
      name = "Test",
      players = List.fill(playerCount)(Player("P")),
      completed = false,
      phase = "locked",
      topN = topN,
      raceToByScope = raceToByScope,
      bracket = None,
      frozenRatings = Nil
    )

  def spec = suite("RaceToWizardStep")(
    test("initial state uses defaultTopN before first save") {
      val tournament = lockedTournament(playerCount = 12, topN = 2, Map.empty)
      val state = RaceToWizardStep.initialState(tournament)
      assertTrue(
        state.topN == TopN.defaultTopN(12),
        state.topN == 8
      )
    },
    test("initial state loads saved topN and race-to map") {
      val saved = RaceToWizard.initialState(12, topN = 4).raceToByScope
      val tournament = lockedTournament(
        playerCount = 12,
        topN = 4,
        raceToByScope = saved
      )
      val state = RaceToWizardStep.initialState(tournament)
      assertTrue(
        state.topN == 4,
        state.raceToByScope == saved
      )
    },
    test("legal options match TopN.legalTopNs and exclude illegal N") {
      assertTrue(
        RaceToWizardStep.legalTopNOptions(12) == TopN.legalTopNs(12),
        !RaceToWizardStep.legalTopNOptions(12).contains(16),
        !RaceToWizardStep.legalTopNOptions(12).contains(12)
      )
    },
    test("scrubbing topN merges overlapping scopes and drops new-only keys") {
      val initial = RaceToWizardStep.initialState(
        lockedTournament(playerCount = 12, topN = 2, Map.empty)
      )
      val customized =
        RaceToWizard.applyEdit(initial, "wb-1", 5, playerCount = 12)
      val scrubbed = RaceToWizardStep.changeTopN(customized, newTopN = 4, 12)
      assertTrue(
        scrubbed.topN == 4,
        scrubbed.raceToByScope.get("wb-1") == Some(5),
        !scrubbed.raceToByScope.contains("se-3")
      )
    },
    test("visible sections include SE for Top 8 on 12 players") {
      val sections = RaceToWizardStep.visibleSections(12, 8)
      assertTrue(
        sections.contains(RaceToScopes.Section.SingleElimination),
        !sections.contains(RaceToScopes.Section.GrandFinal)
      )
    },
    test("visible sections include grand final for Top 2") {
      val sections = RaceToWizardStep.visibleSections(12, 2)
      assertTrue(
        sections.contains(RaceToScopes.Section.GrandFinal),
        !sections.contains(RaceToScopes.Section.SingleElimination)
      )
    },
    test("preview reflects topN structure") {
      val top2 = RaceToWizardStep.preview(12, 2)
      val top8 = RaceToWizardStep.preview(12, 8)
      assertTrue(
        top2.sections.exists(_.section == RaceToScopes.Section.GrandFinal),
        top8.sections.exists(
          _.section == RaceToScopes.Section.SingleElimination
        ),
        top8.bracketSize == 16
      )
    },
    test("save request posts topN and race-to map together") {
      val state = RaceToWizard.initialState(12, topN = 8)
      val request = RaceToWizardStep.saveRequest(state)
      assertTrue(
        request.topN == 8,
        request.raceToByScope.keySet ==
          RaceToScopes.requiredKeys(12, 8).toSet
      )
    }
  )
}
