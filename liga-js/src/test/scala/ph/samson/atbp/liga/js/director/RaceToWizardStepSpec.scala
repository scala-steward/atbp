package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.bracket.BracketFormat
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
    },
    test("SE preview bullets use conventional round names") {
      val playerCount = 8
      val topN = 8
      val preview = RaceToWizardStep.preview(playerCount, topN)
      // Same derivation as WizardView.seRoundsFor(topN).
      val seRounds = BracketFormat.forRoster(playerCount, topN).seRounds
      val se = preview.sections
        .find(_.section == RaceToScopes.Section.SingleElimination)
        .get
      val summaries = se.rounds.map { round =>
        RaceToWizardStep.formatRoundSummary(round, se.section, seRounds)
      }
      assertTrue(
        seRounds == 3,
        summaries == List(
          "Quarterfinals: 8 players, 4 matches",
          "Semifinals: 4 players, 2 matches",
          "Grand Final: 2 players, 1 match"
        )
      )
    },
    test("WB preview bullets stay Round N") {
      val playerCount = 8
      val topN = 2
      val preview = RaceToWizardStep.preview(playerCount, topN)
      val seRounds = BracketFormat.forRoster(playerCount, topN).seRounds
      val wb =
        preview.sections.find(_.section == RaceToScopes.Section.Winners).get
      val summary = RaceToWizardStep.formatRoundSummary(
        wb.rounds.head,
        wb.section,
        seRounds
      )
      assertTrue(seRounds == 0, summary.startsWith("Round 1:"))
    },
    test("inputLabel maps classic DE Top 2 scopes to formatRoundSummary") {
      val playerCount = 8
      val topN = 2
      val preview = RaceToWizardStep.preview(playerCount, topN)
      val seRounds = BracketFormat.forRoster(playerCount, topN).seRounds
      val wb =
        preview.sections.find(_.section == RaceToScopes.Section.Winners).get
      val lb =
        preview.sections.find(_.section == RaceToScopes.Section.Losers).get
      assertTrue(
        RaceToWizardStep.inputLabel("wb-1", preview, seRounds) ==
          RaceToWizardStep.formatRoundSummary(
            wb.rounds.head,
            wb.section,
            seRounds
          ),
        RaceToWizardStep.inputLabel("lb-1", preview, seRounds) ==
          RaceToWizardStep.formatRoundSummary(
            lb.rounds.head,
            lb.section,
            seRounds
          ),
        RaceToWizardStep.inputLabel("gf", preview, seRounds) == "Grand Final"
      )
    },
    test("inputLabel maps cut DE to SE scopes to formatRoundSummary") {
      val playerCount = 12
      val topN = 8
      val preview = RaceToWizardStep.preview(playerCount, topN)
      val seRounds = BracketFormat.forRoster(playerCount, topN).seRounds
      val se = preview.sections
        .find(_.section == RaceToScopes.Section.SingleElimination)
        .get
      assertTrue(
        seRounds == 3,
        RaceToWizardStep.inputLabel("se-1", preview, seRounds) ==
          RaceToWizardStep.formatRoundSummary(
            se.rounds.head,
            se.section,
            seRounds
          ),
        RaceToWizardStep.inputLabel("se-3", preview, seRounds) ==
          RaceToWizardStep.formatRoundSummary(
            se.rounds.last,
            se.section,
            seRounds
          )
      )
    },
    test("inputLabel maps full SE scopes to conventional round names") {
      val playerCount = 8
      val topN = 8
      val preview = RaceToWizardStep.preview(playerCount, topN)
      val seRounds = BracketFormat.forRoster(playerCount, topN).seRounds
      val se = preview.sections
        .find(_.section == RaceToScopes.Section.SingleElimination)
        .get
      val labels = se.rounds.map { round =>
        val scope = s"se-${round.round}"
        RaceToWizardStep.inputLabel(scope, preview, seRounds)
      }
      assertTrue(
        labels == List(
          "Quarterfinals: 8 players, 4 matches",
          "Semifinals: 4 players, 2 matches",
          "Grand Final: 2 players, 1 match"
        )
      )
    }
  )
}
