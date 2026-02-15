package ph.samson.atbp.stmt2csv.parsers

import zio.test.*

import java.time.LocalDate

object BpiAccountParserIntegrationTest
    extends ParserIntegrationTest(BpiAccountParser) {

  def spec = suite("BpiAccountParser") {
    parserTest("BPI.Express_Teller_Checking.2026-01-14.pdf") { entries =>
      assertTrue(
        entries(3).date == LocalDate.parse("2025-12-15"),
        entries(3).amount == -3000,
        entries.last.date == LocalDate.parse("2026-01-14"),
        entries.last.amount == -5250
      )
    }
  }
}
