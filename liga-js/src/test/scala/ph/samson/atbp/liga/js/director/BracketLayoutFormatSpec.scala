package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.bracket.BracketFormat
import ph.samson.atbp.liga.js.api.Models.*
import zio.test.*

/** Layout coverage for Top N cut SE rounds and Top 1 reset GF. */
object BracketLayoutFormatSpec extends ZIOSpecDefault {

  def spec = suite("BracketLayout format")(
    test("groupLabel uses seRounds=0 for classic DE and GF reset") {
      assertTrue(
        BracketLayout.groupLabel(
          BracketLayout.Section.GrandFinal,
          BracketLayout.GrandFinalResetRound,
          seRounds = 0
        ) == "Grand Final — reset",
        BracketLayout.groupLabel(
          BracketLayout.Section.Winners,
          2,
          seRounds = 0
        ) == "Winners — round 2"
      )
    },
    test("groupLabel rejects SE when seRounds is not positive") {
      def rejects(seRounds: Int): Boolean =
        try {
          val _ = BracketLayout.groupLabel(
            BracketLayout.Section.SingleElimination,
            1,
            seRounds
          )
          false
        } catch {
          case _: IllegalArgumentException => true
        }
      assertTrue(rejects(0), rejects(-1))
    },
    test("full SE 8 groupLabel and matchLabel are bare conventional names") {
      val seRounds = 3
      assertTrue(
        BracketLayout.groupLabel(
          BracketLayout.Section.SingleElimination,
          1,
          seRounds
        ) == "Quarterfinals",
        BracketLayout.groupLabel(
          BracketLayout.Section.SingleElimination,
          2,
          seRounds
        ) == "Semifinals",
        BracketLayout.groupLabel(
          BracketLayout.Section.SingleElimination,
          3,
          seRounds
        ) == "Grand Final",
        BracketLayout.matchLabel("se-2-1", bracketSize = 8, seRounds) ==
          "Semifinals"
      )
    },
    test("full SE 16 includes Round of 16") {
      val seRounds = 4
      assertTrue(
        BracketLayout.groupLabel(
          BracketLayout.Section.SingleElimination,
          1,
          seRounds
        ) == "Round of 16"
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
    test("matchLabel labels reset grand final and SE with seRounds") {
      val seRounds = 3
      assertTrue(
        BracketLayout.matchLabel("gf-2", bracketSize = 8, seRounds = 0) ==
          "Grand Final — reset",
        BracketLayout.matchLabel("se-2-1", bracketSize = 8, seRounds) ==
          "Semifinals"
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
        BracketLayout.groupLabel(
          groups.head.section,
          groups.head.round,
          seRounds = 0
        ) == "Grand Final — reset"
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
    test("cut Top 8 on 16-slot bracket uses Top 8 depth for layout labels") {
      val seRounds = BracketFormat.forRoster(12, topN = 8).seRounds
      assertTrue(
        seRounds == 3,
        BracketLayout.groupLabel(
          BracketLayout.Section.SingleElimination,
          1,
          seRounds
        ) == "Quarterfinals",
        BracketLayout.matchLabel("se-3-1", bracketSize = 16, seRounds) ==
          "Grand Final"
      )
    },
    test("cut Top 4 on 16-slot bracket uses Top 4 depth for layout labels") {
      val seRounds = BracketFormat.forRoster(12, topN = 4).seRounds
      assertTrue(
        seRounds == 2,
        BracketLayout.groupLabel(
          BracketLayout.Section.SingleElimination,
          1,
          seRounds
        ) == "Semifinals",
        BracketLayout.groupLabel(
          BracketLayout.Section.SingleElimination,
          2,
          seRounds
        ) == "Grand Final",
        BracketLayout.matchLabel("se-1-1", bracketSize = 16, seRounds) ==
          "Semifinals",
        BracketLayout.matchLabel("se-2-1", bracketSize = 16, seRounds) ==
          "Grand Final"
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
