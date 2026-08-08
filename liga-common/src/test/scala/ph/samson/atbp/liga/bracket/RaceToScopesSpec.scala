package ph.samson.atbp.liga.bracket

import zio.test.*

object RaceToScopesSpec extends ZIOSpecDefault {

  def spec = suite("RaceToScopes")(
    suite("keyForMatch")(
      test("maps winners match ids to wb scope keys") {
        assertTrue(
          RaceToScopes.keyForMatch("wb-3-1") == Some("wb-3"),
          RaceToScopes.keyForMatch("wb-1-4") == Some("wb-1")
        )
      },
      test("maps losers match ids to lb scope keys") {
        assertTrue(
          RaceToScopes.keyForMatch("lb-4-2") == Some("lb-4"),
          RaceToScopes.keyForMatch("lb-1-1") == Some("lb-1")
        )
      },
      test("maps grand final to gf scope key") {
        assertTrue(
          RaceToScopes.keyForMatch("gf-1") == Some("gf"),
          RaceToScopes.keyForMatch("gf-2") == Some("gf")
        )
      },
      test("maps single elimination match ids to se scope keys") {
        assertTrue(
          RaceToScopes.keyForMatch("se-1-1") == Some("se-1"),
          RaceToScopes.keyForMatch("se-3-2") == Some("se-3")
        )
      },
      test("returns None for unknown match ids") {
        assertTrue(
          RaceToScopes.keyForMatch("unknown").isEmpty,
          RaceToScopes.keyForMatch("wb-3").isEmpty
        )
      }
    ),
    suite("requiredKeys")(
      test("requiredKeys(3) returns five scope keys for size-4 bracket") {
        assertTrue(
          RaceToScopes.requiredKeys(3) == List(
            "wb-1",
            "wb-2",
            "lb-1",
            "lb-2",
            "gf"
          )
        )
      },
      test("requiredKeys(4) returns five scope keys for size-4 bracket") {
        assertTrue(
          RaceToScopes.requiredKeys(4) == List(
            "wb-1",
            "wb-2",
            "lb-1",
            "lb-2",
            "gf"
          )
        )
      },
      test("requiredKeys(8) returns eight scope keys") {
        assertTrue(
          RaceToScopes.requiredKeys(8) == List(
            RaceToScopes.keyForWinnersRound(1),
            RaceToScopes.keyForWinnersRound(2),
            RaceToScopes.keyForWinnersRound(3),
            RaceToScopes.keyForLosersRound(1),
            RaceToScopes.keyForLosersRound(2),
            RaceToScopes.keyForLosersRound(3),
            RaceToScopes.keyForLosersRound(4),
            RaceToScopes.grandFinalScopeKey
          )
        )
      },
      test("requiredKeys(16) returns eleven scope keys") {
        assertTrue(
          RaceToScopes.requiredKeys(16) == List(
            "wb-1",
            "wb-2",
            "wb-3",
            "wb-4",
            "lb-1",
            "lb-2",
            "lb-3",
            "lb-4",
            "lb-5",
            "lb-6",
            "gf"
          )
        )
      },
      test("requiredKeys(32) returns fourteen scope keys") {
        assertTrue(
          RaceToScopes.requiredKeys(32) ==
            (1 to 5).map(n => s"wb-$n").toList ++
            (1 to 8).map(n => s"lb-$n").toList :+
            "gf"
        )
      },
      test("requiredKeys(64) returns seventeen scope keys") {
        assertTrue(
          RaceToScopes.requiredKeys(64) ==
            (1 to 6).map(n => s"wb-$n").toList ++
            (1 to 10).map(n => s"lb-$n").toList :+
            "gf"
        )
      },
      test("requiredKeys(playerCount, 2) matches requiredKeys(playerCount)") {
        assertTrue(
          (3 to 64).forall { n =>
            RaceToScopes.requiredKeys(n, 2) == RaceToScopes.requiredKeys(n)
          }
        )
      },
      test("requiredKeys with topN 1 includes gf for reset grand final") {
        assertTrue(
          RaceToScopes.requiredKeys(8, 1).contains("gf"),
          RaceToScopes.requiredKeys(8, 1) == RaceToScopes.requiredKeys(8, 2)
        )
      },
      test(
        "requiredKeys for full SE is se-only when roster is a power of two"
      ) {
        assertTrue(
          RaceToScopes.requiredKeys(8, 8) == List("se-1", "se-2", "se-3"),
          RaceToScopes.requiredKeys(16, 16) ==
            (1 to 4).map(n => s"se-$n").toList
        )
      },
      test("requiredKeys for Top 4+ includes se scopes and drops gf") {
        assertTrue(
          RaceToScopes.requiredKeys(12, 8) ==
            (1 to 2).map(n => s"wb-$n").toList ++
            (1 to 2).map(n => s"lb-$n").toList ++
            (1 to 3).map(n => s"se-$n").toList,
          !RaceToScopes.requiredKeys(12, 8).contains("gf")
        )
      },
      test(
        "requiredKeys for Top 4+ truncates DE depth by topN; SE follows topN"
      ) {
        assertTrue(
          RaceToScopes.requiredKeys(12, 4) ==
            (1 to 3).map(n => s"wb-$n").toList ++
            (1 to 4).map(n => s"lb-$n").toList ++
            List("se-1", "se-2"),
          RaceToScopes.requiredKeys(30, 8) ==
            (1 to 3).map(n => s"wb-$n").toList ++
            (1 to 4).map(n => s"lb-$n").toList ++
            (1 to 3).map(n => s"se-$n").toList
        )
      }
    ),
    suite("scope keys for rounds")(
      test("keyForWinnersRound encodes wb scope") {
        assertTrue(
          RaceToScopes.keyForWinnersRound(1) == "wb-1",
          RaceToScopes.keyForWinnersRound(3) == "wb-3"
        )
      },
      test("keyForLosersRound encodes lb scope") {
        assertTrue(
          RaceToScopes.keyForLosersRound(2) == "lb-2",
          RaceToScopes.keyForLosersRound(4) == "lb-4"
        )
      },
      test("grandFinalScopeKey is gf regardless of bracket round number") {
        assertTrue(RaceToScopes.grandFinalScopeKey == "gf")
      }
    ),
    suite("sectionOf")(
      test("maps scope keys to sections") {
        assertTrue(
          RaceToScopes.sectionOf("wb-3") == RaceToScopes.Section.Winners,
          RaceToScopes.sectionOf("lb-4") == RaceToScopes.Section.Losers,
          RaceToScopes.sectionOf("gf") == RaceToScopes.Section.GrandFinal,
          RaceToScopes.sectionOf("se-2") ==
            RaceToScopes.Section.SingleElimination
        )
      }
    ),
    suite("scopeLabel")(
      test("labels winners scopes with round number") {
        val label = RaceToScopes.scopeLabel("wb-3", seRounds = 0)
        assertTrue(
          label.section == RaceToScopes.Section.Winners,
          label.roundLabel == "Round 3"
        )
      },
      test("labels losers scopes with round number") {
        val label = RaceToScopes.scopeLabel("lb-4", seRounds = 0)
        assertTrue(
          label.section == RaceToScopes.Section.Losers,
          label.roundLabel == "Round 4"
        )
      },
      test("labels grand final scope") {
        val label = RaceToScopes.scopeLabel("gf", seRounds = 0)
        assertTrue(
          label.section == RaceToScopes.Section.GrandFinal,
          label.roundLabel == "Grand Final"
        )
      },
      test("full SE 8 uses conventional round names") {
        assertTrue(
          RaceToScopes
            .scopeLabel("se-1", seRounds = 3)
            .roundLabel == "Quarterfinals",
          RaceToScopes
            .scopeLabel("se-2", seRounds = 3)
            .roundLabel == "Semifinals",
          RaceToScopes
            .scopeLabel("se-3", seRounds = 3)
            .roundLabel == "Grand Final"
        )
      },
      test("full SE 16 includes Round of 16 then QF / SF / GF") {
        assertTrue(
          RaceToScopes
            .scopeLabel("se-1", seRounds = 4)
            .roundLabel == "Round of 16",
          RaceToScopes
            .scopeLabel("se-2", seRounds = 4)
            .roundLabel == "Quarterfinals",
          RaceToScopes
            .scopeLabel("se-3", seRounds = 4)
            .roundLabel == "Semifinals",
          RaceToScopes
            .scopeLabel("se-4", seRounds = 4)
            .roundLabel == "Grand Final"
        )
      },
      test("cut Top 8 on larger roster uses Top 8 SE depth not bracket size") {
        val seRounds = BracketFormat.forRoster(12, topN = 8).seRounds
        assertTrue(
          seRounds == 3,
          RaceToScopes
            .scopeLabel("se-1", seRounds)
            .roundLabel == "Quarterfinals",
          RaceToScopes.scopeLabel("se-3", seRounds).roundLabel == "Grand Final"
        )
      },
      test("cut Top 4 uses two-round SE tail") {
        val seRounds = BracketFormat.forRoster(12, topN = 4).seRounds
        assertTrue(
          seRounds == 2,
          RaceToScopes.scopeLabel("se-1", seRounds).roundLabel == "Semifinals",
          RaceToScopes.scopeLabel("se-2", seRounds).roundLabel == "Grand Final"
        )
      },
      test("cut Top 16 uses four-round SE tail") {
        val seRounds = BracketFormat.forRoster(30, topN = 16).seRounds
        assertTrue(
          seRounds == 4,
          RaceToScopes.scopeLabel("se-1", seRounds).roundLabel == "Round of 16"
        )
      },
      test("rejects se scope labels when seRounds is not positive") {
        def rejects(scope: String, seRounds: Int): Boolean =
          try {
            val _ = RaceToScopes.scopeLabel(scope, seRounds)
            false
          } catch {
            case _: IllegalArgumentException => true
          }
        assertTrue(
          rejects("se-1", seRounds = 0),
          rejects("se-1", seRounds = -1)
        )
      }
    )
  )
}
