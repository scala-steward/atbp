package ph.samson.atbp.liga.cli

import ph.samson.atbp.liga.cli.MarkdownTable.Alignment
import zio.test.*

object MarkdownTableSpec extends ZIOSpecDefault {

  def spec = suite("MarkdownTable")(
    test("pads columns for aligned output") {
      val columns = List(
        MarkdownTable.Column("Player", Alignment.Left),
        MarkdownTable.Column("Rating", Alignment.Right)
      )
      val rows = List(
        List("Alice", "1700"),
        List("Bob", "1600")
      )
      val rendered = MarkdownTable.render(columns, rows)
      assertTrue(
        rendered ==
          """| Player | Rating |
             #| ------ | -----: |
             #| Alice  |   1700 |
             #| Bob    |   1600 |
             #""".stripMargin('#')
      )
    }
  )
}
