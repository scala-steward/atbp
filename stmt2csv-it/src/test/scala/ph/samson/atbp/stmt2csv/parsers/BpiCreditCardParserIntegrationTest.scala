package ph.samson.atbp.stmt2csv.parsers

import zio.test.*

import java.time.LocalDate

object BpiCreditCardParserIntegrationTest
    extends ParserIntegrationTest(BpiCreditCardParser) {

  def spec = suite("BpiCreditCardParser") {
    parserTest("BPI.Credit_Card.2026-01-05.pdf") { entries =>
      assertTrue(
        entries(2).date == LocalDate.parse("2025-12-23"),
        entries(2).amount == 6133.58,
        entries.last.date == LocalDate.parse("2025-12-27"),
        entries.last.amount == 382.79
      )
    }
  }
}
