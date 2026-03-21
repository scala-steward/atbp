package ph.samson.atbp.stmt2csv.parsers

import zio.test.assertTrue

import java.time.LocalDate

object MayaWalletParserIntegrationTest
    extends ParserIntegrationTest(MayaWalletParser) {

  override def spec = suite("MayaWalletParser")(
    parserTest("Maya.Wallet.2026-01.pdf") { entries =>
      assertTrue(
        entries.head.date == LocalDate.parse("2026-01-03"),
        entries.head.amount == -2115,
        entries.last.date == LocalDate.parse("2026-01-30"),
        entries.last.amount == 370
      )
    }
  )
}
