package ph.samson.atbp.stmt2csv.parsers

import zio.test.*

import java.time.LocalDate

object MayaSavingsParserIntegrationTest
    extends ParserIntegrationTest(MayaSavingsParser) {

  override def spec = suite("MayaSavingsParser")(
    parserTest("Maya.Savings.2026-02-01.pdf") { entries =>
      assertTrue(
        entries.head.date == LocalDate.parse("2026-01-01"),
        entries.head.amount == 1.39,
        entries.last.date == LocalDate.parse("2026-01-31"),
        entries.last.amount == -0.51
      )
    },
    parserTest("Maya.PersonalGoal.2026-04-01.pdf") { entries =>
      // parsed description overruns into section of the statement after the
      // transaction list
      assertTrue(
        entries(6).description.length == 87
      )
    }
  )
}
