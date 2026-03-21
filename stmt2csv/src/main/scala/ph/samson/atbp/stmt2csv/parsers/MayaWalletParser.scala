package ph.samson.atbp.stmt2csv.parsers

import fastparse.*
import org.apache.pdfbox.text.PDFTextStripper
import ph.samson.atbp.stmt2csv.CsvEntry
import zio.Task
import zio.ZIO

import java.time.LocalDate
import java.time.LocalTime
import java.time.format.DateTimeFormatter

import NoWhitespace.*

object MayaWalletParser extends StatementParser {

  override def stripper: PDFTextStripper = {
    val s = new PDFTextStripper
    s.setSortByPosition(true)
    s
  }

  override def validate(text: String): Task[String] =
    if (
      text.contains(
        "Transaction details\nDate & Time Transaction type & details Reference ID Amount Running balance"
      )
    ) {
      ZIO.succeed(text)
    } else {
      ZIO.fail(new IllegalArgumentException("Not a Maya Wallet Statement"))
    }

  override def parseEntries(text: String): Task[List[CsvEntry]] =
    ZIO.attempt(parse(text, entries(using _)).get.value.toList)

  sealed abstract class Line
  case class Boring(value: String) extends Line
  case class Transaction(
      date: LocalDate,
      time: LocalTime,
      txType: String,
      txDetails: String,
      referenceId: String,
      amount: BigDecimal
  )

  def entries[T: P]: P[Seq[CsvEntry]] = P(
    (transaction | anyLine.map(Boring.apply)).rep
  ).map { lines =>
    val transactions = lines.collect { case t: Transaction => t }
    transactions.map {
      case Transaction(date, time, txType, txDetails, referenceId, amount) =>
        val description = s"$txType | $txDetails | $referenceId"
        CsvEntry(date, description, amount)
    }
  }

  def transaction[T: P]: P[Transaction] = P(
    date ~ " " ~
      (!referenceId ~ AnyChar).rep.! ~
      referenceId ~ " " ~
      (credit | debit) ~
      " PHP " ~ amount ~ eol ~
      time ~ anyLine
  ).map { case (date, txType, referenceId, amount, _, time, txDetails) =>
    Transaction(date, time, txType.trim, txDetails.trim, referenceId, amount)
  }

  private val DateFmt = DateTimeFormatter.ofPattern("d MMM yyyy")
  def date[T: P] = P((shortDay ~ " " ~ shortMonth ~ " " ~ year).!)
    .map(LocalDate.parse(_, DateFmt))

  def referenceId[T: P]: P[String] = P(CharIn("A-Z", "0-9").rep(exactly = 12).!)

  def credit[T: P]: P[BigDecimal] = P("PHP " ~ amount)

  def debit[T: P]: P[BigDecimal] = P("-" ~ credit).map(a => -a)

  private val TimeFmt = DateTimeFormatter.ofPattern("hh:mm:ss a")
  def time[T: P]: P[LocalTime] =
    P(
      (digit.rep(2) ~ ":" ~ digit.rep(2) ~ ":" ~ digit.rep(
        2
      ) ~ (" AM" | " PM")).!
    ).map(s => LocalTime.parse(s, TimeFmt))
}
