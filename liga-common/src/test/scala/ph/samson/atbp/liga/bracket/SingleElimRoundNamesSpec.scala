package ph.samson.atbp.liga.bracket

import zio.test.*

object SingleElimRoundNamesSpec extends ZIOSpecDefault {

  def spec = suite("SingleElimRoundNames")(
    test(
      "name from players remaining maps 2 / 4 / 8 to Grand Final / Semifinals / Quarterfinals"
    ) {
      assertTrue(
        SingleElimRoundNames.name(2) == "Grand Final",
        SingleElimRoundNames.name(4) == "Semifinals",
        SingleElimRoundNames.name(8) == "Quarterfinals"
      )
    },
    test(
      "name from players remaining maps larger power-of-two fields to Round of N"
    ) {
      assertTrue(
        SingleElimRoundNames.name(16) == "Round of 16",
        SingleElimRoundNames.name(32) == "Round of 32"
      )
    },
    test(
      "name from round and seRounds uses remaining = 2^(seRounds - round + 1)"
    ) {
      assertTrue(
        SingleElimRoundNames.name(round = 1, seRounds = 3) == "Quarterfinals",
        SingleElimRoundNames.name(round = 2, seRounds = 3) == "Semifinals",
        SingleElimRoundNames.name(round = 3, seRounds = 3) == "Grand Final"
      )
    },
    test("name rejects unsupported remaining values") {
      val nonPower = zio.ZIO.attempt(SingleElimRoundNames.name(6)).either
      val onePlayer = zio.ZIO.attempt(SingleElimRoundNames.name(1)).either
      for {
        _ <- assertZIO(nonPower)(
          Assertion.isLeft(
            Assertion.isSubtype[IllegalArgumentException](Assertion.anything)
          )
        )
        _ <- assertZIO(onePlayer)(
          Assertion.isLeft(
            Assertion.isSubtype[IllegalArgumentException](Assertion.anything)
          )
        )
      } yield assertCompletes
    },
    test("name rejects out-of-range round / seRounds") {
      def rejects(round: Int, seRounds: Int): Boolean =
        try {
          val _ = SingleElimRoundNames.name(round, seRounds)
          false
        } catch {
          case _: IllegalArgumentException => true
        }
      assertTrue(
        rejects(round = 0, seRounds = 3),
        rejects(round = -1, seRounds = 3),
        rejects(round = 0, seRounds = 0)
      )
    }
  )
}
