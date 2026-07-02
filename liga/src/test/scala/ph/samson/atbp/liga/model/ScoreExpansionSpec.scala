package ph.samson.atbp.liga.model

import zio.test.*

object ScoreExpansionSpec extends ZIOSpecDefault {

  def spec = suite("ScoreExpansion")(
    test("7-4 expands to 11 atomic games") {
      val games = ScoreExpansion.expandGames(scoreA = 7, scoreB = 4)
      assertTrue(
        games.size == 11,
        games.count(_ == GameWinner.PlayerA) == 7,
        games.count(_ == GameWinner.PlayerB) == 4
      )
    },
    test("0-0 expands to no games") {
      val games = ScoreExpansion.expandGames(scoreA = 0, scoreB = 0)
      assertTrue(games.isEmpty)
    },
    test("shutout expands to winner-only games") {
      val games = ScoreExpansion.expandGames(scoreA = 7, scoreB = 0)
      assertTrue(
        games.size == 7,
        games.forall(_ == GameWinner.PlayerA)
      )
    }
  )
}
