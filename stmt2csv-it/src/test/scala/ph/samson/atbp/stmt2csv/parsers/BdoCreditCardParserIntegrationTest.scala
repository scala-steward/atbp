package ph.samson.atbp.stmt2csv.parsers

import zio.test.*

import java.time.LocalDate

object BdoCreditCardParserIntegrationTest
    extends ParserIntegrationTest(BdoCreditCardParser) {

  def spec = suite("BdoCreditCardParser")(
    parserTest("BDO.Credit_Card.2026-02-01.pdf") { entries =>
      assertTrue(
        entries.head.date == LocalDate.parse("2026-02-01"),
        entries.head.amount == 31981.55,
        entries.last.date == LocalDate.parse("2026-01-28"),
        entries.last.amount == 97
      )
    }
  )
}
