package ph.samson.atbp.liga.bracket

import zio.test.*

object BracketPreviewSpec extends ZIOSpecDefault {

  private def section(
      preview: BracketPreview.Preview,
      section: RaceToScopes.Section
  ): Option[BracketPreview.SectionPreview] =
    preview.sections.find(_.section == section)

  def spec = suite("BracketPreview")(
    test("Top 2 on roster 12 uses 16-slot double elimination") {
      val preview = BracketPreview(12, 2)
      val winners = section(preview, RaceToScopes.Section.Winners).get
      val losers = section(preview, RaceToScopes.Section.Losers).get
      val gf = section(preview, RaceToScopes.Section.GrandFinal).get

      assertTrue(
        preview.bracketSize == 16,
        preview.playerCount == 12,
        preview.topN == 2,
        winners.rounds.size == 4,
        losers.rounds.size == 6,
        winners.rounds.head.matches == 4,
        winners.rounds.head.byes == 4,
        winners.rounds.head.players == 12,
        gf.rounds.size == 1,
        gf.rounds.head.matches == 1,
        gf.resetGrandFinalPossible == false,
        section(preview, RaceToScopes.Section.SingleElimination).isEmpty
      )
    },
    test("30-player classic DE excludes byes from match counts") {
      val preview = BracketPreview(30, 2)
      val winners = section(preview, RaceToScopes.Section.Winners).get
      val losers = section(preview, RaceToScopes.Section.Losers).get
      val wb1 = winners.rounds.head
      val lb1 = losers.rounds.head

      assertTrue(
        preview.bracketSize == 32,
        wb1.players == 30,
        wb1.matches == 14,
        wb1.byes == 2,
        lb1.players == 14,
        lb1.matches == 6,
        lb1.byes == 2,
        winners.rounds(1) == BracketPreview.RoundCounts(2, 16, 8, 0),
        losers.rounds(1) == BracketPreview.RoundCounts(2, 16, 8, 0)
      )
    },
    test("12-player classic DE LB1 is all byes from WB opening byes") {
      val preview = BracketPreview(12, 2)
      val losers = section(preview, RaceToScopes.Section.Losers).get
      val lb1 = losers.rounds.head
      val lb2 = losers.rounds(1)

      assertTrue(
        lb1.players == 4,
        lb1.matches == 0,
        lb1.byes == 4,
        lb2.players == 8,
        lb2.matches == 4,
        lb2.byes == 0
      )
    },
    test("5-player classic DE cascades LB byes into round 2") {
      val preview = BracketPreview(5, 2)
      val winners = section(preview, RaceToScopes.Section.Winners).get
      val losers = section(preview, RaceToScopes.Section.Losers).get

      assertTrue(
        winners.rounds.head == BracketPreview.RoundCounts(1, 5, 1, 3),
        losers.rounds.head == BracketPreview.RoundCounts(1, 1, 0, 2),
        losers.rounds(1) == BracketPreview.RoundCounts(2, 3, 1, 1),
        losers.rounds(2) == BracketPreview.RoundCounts(3, 2, 1, 0)
      )
    },
    test("Top 8 on roster 12 shows DE until cut then single elimination") {
      val preview = BracketPreview(12, 8)
      val winners = section(preview, RaceToScopes.Section.Winners).get
      val losers = section(preview, RaceToScopes.Section.Losers).get
      val se = section(preview, RaceToScopes.Section.SingleElimination).get

      assertTrue(
        preview.bracketSize == 16,
        winners.rounds.size == 2,
        losers.rounds.size == 2,
        se.rounds.size == 3,
        se.rounds.head.players == 8,
        se.rounds.head.matches == 4,
        section(preview, RaceToScopes.Section.GrandFinal).isEmpty
      )
    },
    test("full SE when N equals power-of-two roster has no losers bracket") {
      val preview = BracketPreview(8, 8)
      val se = section(preview, RaceToScopes.Section.SingleElimination).get

      assertTrue(
        preview.bracketSize == 8,
        preview.sections.size == 1,
        se.rounds.size == 3,
        se.rounds.head.players == 8,
        se.rounds.head.matches == 4,
        section(preview, RaceToScopes.Section.Winners).isEmpty,
        section(preview, RaceToScopes.Section.Losers).isEmpty,
        section(preview, RaceToScopes.Section.GrandFinal).isEmpty
      )
    },
    test("Top 1 includes reset grand final indication") {
      val preview = BracketPreview(12, 1)
      val gf = section(preview, RaceToScopes.Section.GrandFinal).get

      assertTrue(
        preview.bracketSize == 16,
        gf.resetGrandFinalPossible,
        gf.rounds.size == 1,
        gf.rounds.head.matches == 1
      )
    },
    test("scope keys align with RaceToScopes.requiredKeys") {
      val preview = BracketPreview(12, 8)
      val scopeRounds =
        preview.sections.flatMap { sectionPreview =>
          sectionPreview.rounds.map(round =>
            (sectionPreview.section, round.round)
          )
        }
      val winnersRounds =
        scopeRounds.count(_._1 == RaceToScopes.Section.Winners)
      val losersRounds =
        scopeRounds.count(_._1 == RaceToScopes.Section.Losers)
      val seRounds =
        scopeRounds.count(_._1 == RaceToScopes.Section.SingleElimination)

      assertTrue(
        winnersRounds == 2,
        losersRounds == 2,
        seRounds == 3,
        RaceToScopes
          .requiredKeys(12, 8)
          .size == winnersRounds + losersRounds + seRounds
      )
    },
    test("rejects illegal topN for roster") {
      val message =
        try {
          val _ = BracketPreview(12, 16)
          ""
        } catch {
          case e: IllegalArgumentException => e.getMessage
        }
      assertTrue(message.contains("topN"))
    }
  )
}
