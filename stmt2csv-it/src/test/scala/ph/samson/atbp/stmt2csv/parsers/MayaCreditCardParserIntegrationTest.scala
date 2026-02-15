package ph.samson.atbp.stmt2csv.parsers

import zio.test.*

import java.time.LocalDate

object MayaCreditCardParserIntegrationTest
    extends ParserIntegrationTest(MayaCreditCardParser) {

  def spec = suite("MayaCreditCardParser")(
    parserTest("Maya.Credit_Card.2026-02-08.pdf") { entries =>
      assertTrue(
        entries.head.date == LocalDate.parse("2026-01-08"),
        entries.head.amount == 358,
        entries.last.date == LocalDate.parse("2026-02-04"),
        entries.last.amount == 408.48
      )
    },
    parserTest("Maya.Credit_Card.2026-02-13.pdf") { entries =>
      assertTrue(
        entries.head.date == LocalDate.parse("2026-01-25"),
        entries.head.amount == 2445.50,
        entries.last.date == LocalDate.parse("2026-02-01"),
        entries.last.amount == 963.43
      )
    }
  )

}
