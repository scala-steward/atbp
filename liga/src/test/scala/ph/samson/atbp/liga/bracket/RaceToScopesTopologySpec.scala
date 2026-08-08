package ph.samson.atbp.liga.bracket

import zio.test.*

object RaceToScopesTopologySpec extends ZIOSpecDefault {

  def spec = suite("RaceToScopes topology alignment")(
    test("requiredKeys(playerCount, topN=2) matches BracketTopology scopes") {
      val topN = 2
      val mismatches = (3 to 64).flatMap { playerCount =>
        val bracketSize = Seeding.bracketSize(playerCount)
        val fromTopology =
          BracketTopology(bracketSize, topN).matches.keys
            .flatMap(RaceToScopes.keyForMatch)
            .toSet
        val fromFormula = RaceToScopes.requiredKeys(playerCount, topN).toSet
        Option.when(fromTopology != fromFormula) {
          s"playerCount=$playerCount topN=$topN: topology=$fromTopology formula=$fromFormula"
        }
      }
      assertTrue(mismatches.isEmpty)
    },
    test("requiredKeys(playerCount, topN=1) matches BracketTopology scopes") {
      val topN = 1
      val mismatches = (3 to 64).flatMap { playerCount =>
        val bracketSize = Seeding.bracketSize(playerCount)
        val fromTopology =
          BracketTopology(bracketSize, topN).matches.keys
            .flatMap(RaceToScopes.keyForMatch)
            .toSet
        val fromFormula = RaceToScopes.requiredKeys(playerCount, topN).toSet
        Option.when(fromTopology != fromFormula) {
          s"playerCount=$playerCount topN=$topN: topology=$fromTopology formula=$fromFormula"
        }
      }
      assertTrue(mismatches.isEmpty)
    },
    test(
      "requiredKeys(playerCount, topN=field) matches BracketTopology scopes for full SE"
    ) {
      val powerOfTwoRosters = List(4, 8, 16, 32, 64)
      val mismatches = powerOfTwoRosters.flatMap { playerCount =>
        val topN = playerCount
        val fromTopology =
          BracketTopology(playerCount, topN).matches.keys
            .flatMap(RaceToScopes.keyForMatch)
            .toSet
        val fromFormula = RaceToScopes.requiredKeys(playerCount, topN).toSet
        Option.when(fromTopology != fromFormula) {
          s"playerCount=$playerCount topN=$topN: topology=$fromTopology formula=$fromFormula"
        }
      }
      assertTrue(mismatches.isEmpty)
    },
    test(
      "requiredKeys(playerCount, topN=4+) matches BracketTopology scopes for cut DE"
    ) {
      val mismatches = (3 to 64).flatMap { playerCount =>
        TopN
          .legalTopNs(playerCount)
          .filter(topN => topN >= 4 && topN < playerCount)
          .flatMap { topN =>
            val bracketSize = Seeding.bracketSize(playerCount)
            val fromTopology =
              BracketTopology(bracketSize, topN).matches.keys
                .flatMap(RaceToScopes.keyForMatch)
                .toSet
            val fromFormula = RaceToScopes.requiredKeys(playerCount, topN).toSet
            Option.when(fromTopology != fromFormula) {
              s"playerCount=$playerCount topN=$topN: topology=$fromTopology formula=$fromFormula"
            }
          }
      }
      assertTrue(mismatches.isEmpty)
    }
  )
}
