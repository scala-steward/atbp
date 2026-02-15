package ph.samson.atbp.stmt2csv.parsers

import zio.test.*

import java.time.LocalDate

object UnionBankCreditCardParserIntegrationTest
    extends ParserIntegrationTest(UnionBankCreditCardParser) {

  override def spec = suite("UnionBankCreditCardParser")(
    parserTest("UnionBank.Credit_Card.2026-02-05.pdf") { entries =>
      assertTrue(
        entries.head.date == LocalDate.parse("2026-01-05"),
        entries.head.amount == 204,
        entries.last.date == LocalDate.parse("2026-01-31"),
        entries.last.amount == 179
      )
    }
  )
}
