package ph.samson.atbp.liga.cli

import ph.samson.atbp.liga.model.*
import zio.test.*

object LeaderboardRendererSpec extends ZIOSpecDefault {

  private val alice = Player("Alice")
  private val bob = Player("Bob")

  def spec = suite("LeaderboardRenderer")(
    test("renders markdown table sorted by rating descending") {
      val ratings = List(
        PlayerRating(bob, 1600, 120.45, 5, 3),
        PlayerRating(alice, 1700, 99.94, 12, 8)
      )
      val rendered = LeaderboardRenderer.render(ratings)
      assertTrue(
        rendered ==
          """#| Player | Rating | RD | W-L |
             #| --- | ---: | ---: | ---: |
             #| Alice | 1700 | 99.9 | 12-8 |
             #| Bob | 1600 | 120.5 | 5-3 |
             #""".stripMargin('#')
      )
    },
    test("formats rating as integer and RD to one decimal") {
      val ratings = List(
        PlayerRating(alice, 1700.6, 99.94, 0, 0)
      )
      val rendered = LeaderboardRenderer.render(ratings)
      assertTrue(
        rendered.contains("| 1701 |"),
        rendered.contains("| 99.9 |")
      )
    }
  )
}
