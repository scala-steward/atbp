package ph.samson.atbp.liga.bracket

import zio.test.*

object BracketFormatSpec extends ZIOSpecDefault {

  def spec = suite("BracketFormat")(
    test("roster Top 8 cut stops DE at round-of-8 depth, not full WB") {
      val top8 = BracketFormat.forRoster(12, 8)
      val top4 = BracketFormat.forRoster(12, 4)
      assertTrue(
        top8.kind == BracketFormat.Kind.CutDoubleElimination,
        top4.kind == BracketFormat.Kind.CutDoubleElimination,
        top8.bracketSize == 16,
        top8.winnersRounds == 2,
        top8.losersRounds == 2,
        top4.winnersRounds == 3,
        top4.losersRounds == 4,
        top8.seRounds == 3,
        top4.seRounds == 2,
        !top8.singleMatchLosersFinal
      )
    },
    test("30-player Top 8 cut has no wb-4 or lb-5 scopes") {
      val shape = BracketFormat.forRoster(30, 8)
      assertTrue(
        shape.kind == BracketFormat.Kind.CutDoubleElimination,
        shape.bracketSize == 32,
        shape.winnersRounds == 3,
        shape.losersRounds == 4,
        shape.seRounds == 3
      )
    },
    test("classic Top 1 and Top 2 keep single-match LB final and GF") {
      val top1 = BracketFormat.forRoster(12, 1)
      val top2 = BracketFormat.forRoster(12, 2)
      assertTrue(
        top1.kind == BracketFormat.Kind.ClassicDoubleElimination,
        top2.hasGrandFinal,
        top1.resetGrandFinalPossible,
        !top2.resetGrandFinalPossible,
        top1.losersRounds == 6,
        top1.singleMatchLosersFinal
      )
    },
    test("full SE when topN equals bracket size") {
      val shape = BracketFormat.forBracket(8, 8)
      assertTrue(
        shape.kind == BracketFormat.Kind.FullSingleElimination,
        shape.seRounds == 3,
        shape.winnersRounds == 0,
        shape.losersRounds == 0,
        !shape.hasGrandFinal
      )
    }
  )
}
