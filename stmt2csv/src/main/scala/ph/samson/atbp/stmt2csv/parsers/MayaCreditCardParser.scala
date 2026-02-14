package ph.samson.atbp.stmt2csv.parsers

import fastparse.*
import org.apache.pdfbox.text.PDFTextStripper
import ph.samson.atbp.stmt2csv.CsvEntry
import zio.Task
import zio.ZIO

import java.time.LocalDate
import java.time.MonthDay
import java.time.format.DateTimeFormatter

import NoWhitespace.*

object MayaCreditCardParser extends StatementParser {

  override def stripper: PDFTextStripper = {
    val s = new PDFTextStripper
    s.setSortByPosition(true)
    s
  }

  override def validate(text: String): Task[String] =
    if (
      text.contains("Maya Black Credit Card")
      || text.contains("Landers CC")
    ) {
      ZIO.succeed(text)
    } else {
      ZIO.fail(new IllegalArgumentException("Not a Maya Credit Card Statement"))
    }

  override def parseEntries(text: String): Task[List[CsvEntry]] =
    ZIO.attempt(parse(text, entries(using _)).get.value.toList)

  sealed abstract class Line
  case class Boring(value: String) extends Line
  case class Transaction(
      date: MonthDay,
      posted: MonthDay,
      description: String,
      amount: BigDecimal
  ) extends Line

  def entries[T: P] = P(
    (!billingDate ~ anyLine).rep ~
      billingDate ~
      (transaction | anyLine.map(Boring.apply)).rep
  ).map { case (_, billingDate, lines) =>
    def fix(date: MonthDay): LocalDate = {
      val atYear = date.atYear(billingDate.getYear)
      if (atYear.isAfter(billingDate)) {
        atYear.minusYears(1)
      } else {
        atYear
      }
    }

    val transactions = lines.collect { case t: Transaction => t }

    transactions.map { case Transaction(date, _, description, amount) =>
      CsvEntry(fix(date), description, amount)
    }
  }

  def billingDate[T: P]: P[LocalDate] = P(
    "Billing date: " ~ longDate ~ eol.rep
  )

  private val LongDateFmt = DateTimeFormatter.ofPattern("d MMMM yyyy")
  def longDate[T: P]: P[LocalDate] =
    P((shortDay ~ " " ~ longMonth ~ " " ~ year).!).map(s =>
      LocalDate.parse(s, LongDateFmt)
    )

  def transaction[T: P]: P[Transaction] = P(
    monthDay ~ " " ~
      monthDay ~ " " ~
      (!(amount ~ eol) ~ AnyChar).rep.! ~
      amount ~ eol ~
      wrappedDescription.rep ~
      eol.rep
  ).map { case (date, posted, description, amount, wrapped) =>
    val desc = description.trim :: wrapped.map(_.trim).toList
    Transaction(date, posted, desc.mkString(" "), amount)
  }

  def wrappedDescription[T: P]: P[String] = P(
    !("Page" | monthDay) ~ anyLine
  )

  private val MonthDayFmt = DateTimeFormatter.ofPattern("d MMM")
  def monthDay[T: P]: P[MonthDay] =
    P((shortDay ~ " " ~ shortMonth).!).map(s => MonthDay.parse(s, MonthDayFmt))
}
