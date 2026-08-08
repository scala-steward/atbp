package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.js.api.Models.*
import zio.test.*

/** Layout coverage for Top N cut SE rounds and Top 1 reset GF. */
object BracketLayoutFormatSpec extends ZIOSpecDefault {

  def spec = suite("BracketLayout format")(
    test("groupLabel labels reset grand final and single elimination rounds") {
      assertTrue(
        BracketLayout.groupLabel(
          BracketLayout.Section.GrandFinal,
          BracketLayout.GrandFinalResetRound
        ) == "Grand Final — reset",
        BracketLayout.groupLabel(BracketLayout.Section.SingleElimination, 2) ==
          "Single Elimination — round 2"
      )
    },
    test("sectionOf maps se-* to Single Elimination") {
      assertTrue(
        BracketLayout.sectionOf("se-1-1") ==
          BracketLayout.Section.SingleElimination,
        BracketLayout.sectionOf("se-3-2") ==
          BracketLayout.Section.SingleElimination,
        BracketLayout.sectionOf("gf-2") == BracketLayout.Section.GrandFinal
      )
    },
    test("bracketRound resolves se-* and gf-2") {
      assertTrue(
        BracketLayout.bracketRound("se-2-1", bracketSize = 8) == Some(2),
        BracketLayout.bracketRound("se-3-2", bracketSize = 8) == Some(3),
        BracketLayout.bracketRound("gf-2", bracketSize = 8) ==
          Some(BracketLayout.GrandFinalResetRound)
      )
    },
    test("matchLabel labels reset grand final") {
      assertTrue(
        BracketLayout.matchLabel("gf-2", bracketSize = 8) ==
          "Grand Final — reset",
        BracketLayout.matchLabel("se-2-1", bracketSize = 8) ==
          "Single Elimination — round 2"
      )
    },
    test("groupMatches groups se-* under Single Elimination by round") {
      val player = Some(Player("P1"))
      val opponent = Some(Player("P2"))
      val se1 = BracketMatch(
        id = "se-1-1",
        playerA = player,
        playerB = opponent,
        state = BracketMatchState.Ready
      )
      val se2 = se1.copy(id = "se-2-1")
      val groups =
        BracketLayout.groupMatches(matches = List(se1, se2), bracketSize = 8)
      assertTrue(
        groups.map(g => (g.section, g.round)) ==
          List(
            (BracketLayout.Section.SingleElimination, 2),
            (BracketLayout.Section.SingleElimination, 1)
          )
      )
    },
    test("groupMatches places gf-2 in Grand Final reset group when present") {
      val player = Some(Player("P1"))
      val opponent = Some(Player("P2"))
      val gf1 = BracketMatch(
        id = "gf-1",
        playerA = player,
        playerB = opponent,
        state = BracketMatchState.Completed
      )
      val gf2 = BracketMatch(
        id = "gf-2",
        playerA = player,
        playerB = opponent,
        state = BracketMatchState.Ready
      )
      val groups =
        BracketLayout.groupMatches(matches = List(gf1, gf2), bracketSize = 8)
      assertTrue(
        groups.length == 2,
        groups.map(g => (g.section, g.round)) ==
          List(
            (
              BracketLayout.Section.GrandFinal,
              BracketLayout.GrandFinalResetRound
            ),
            (BracketLayout.Section.GrandFinal, 3)
          ),
        groups.head.matches.map(_.id) == List("gf-2"),
        BracketLayout.groupLabel(groups.head.section, groups.head.round) ==
          "Grand Final — reset"
      )
    },
    test(
      "groupMatches stacks Grand Final above Single Elimination above Losers"
    ) {
      val player = Some(Player("P1"))
      val opponent = Some(Player("P2"))
      val wb = BracketMatch(
        id = "wb-1-1",
        playerA = player,
        playerB = opponent,
        state = BracketMatchState.Ready
      )
      val lb = wb.copy(id = "lb-1-1")
      val se = wb.copy(id = "se-1-1")
      val gf = wb.copy(id = "gf-1")
      val groups =
        BracketLayout.groupMatches(
          matches = List(wb, lb, se, gf),
          bracketSize = 8
        )
      assertTrue(
        groups.map(_.section) == List(
          BracketLayout.Section.GrandFinal,
          BracketLayout.Section.SingleElimination,
          BracketLayout.Section.Losers,
          BracketLayout.Section.Winners
        )
      )
    },
    test("resolveRoundRaceTo resolves single elimination scopes") {
      val raceToByScope = Map("se-2" -> 5)
      assertTrue(
        BracketLayout.resolveRoundRaceTo(
          BracketLayout.Section.SingleElimination,
          2,
          raceToByScope
        ) == Some(5)
      )
    }
  )
}
