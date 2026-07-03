package ph.samson.atbp.liga.io

import better.files.File
import ph.samson.atbp.liga.model.*
import zio.test.*

import java.time.LocalDate

object PeriodIoSpec extends ZIOSpecDefault {

  private val springOpen = File(
    getClass.getResource("/periods/spring-open.liga")
  )

  private val expectedSpringOpen = Period(
    name = "Spring 2026 Open",
    completed = LocalDate.parse("2026-03-15"),
    format = Some("8-ball"),
    raceTo = Some(7),
    matches = List(
      PeriodMatch(
        playerA = Player("Alice"),
        playerB = Player("Bob"),
        scoreA = 7,
        scoreB = 4,
        raceTo = 7,
        handicapSuggested = 2,
        handicapApplied = 2
      ),
      PeriodMatch(
        playerA = Player("Carol"),
        playerB = Player("Dave"),
        scoreA = 5,
        scoreB = 7,
        raceTo = 7,
        handicapSuggested = 0,
        handicapApplied = 0
      )
    )
  )

  def spec = suite("Period I/O")(
    test("parse all fields from fixture HOCON") {
      for {
        period <- PeriodCodec.parseFile(springOpen)
      } yield assertTrue(period == expectedSpringOpen)
    },
    test("completed date is parsed as LocalDate ordering key") {
      for {
        period <- PeriodCodec.parseFile(springOpen)
      } yield assertTrue(
        period.completed == LocalDate.of(2026, 3, 15),
        period.completed.isBefore(LocalDate.of(2026, 3, 16))
      )
    },
    test("write produces valid HOCON that re-parses identically") {
      for {
        parsed <- PeriodCodec.parseFile(springOpen)
        written = PeriodWriter.write(parsed)
        roundTripped <- PeriodCodec.parseString(written)
      } yield assertTrue(roundTripped == parsed)
    },
    test("invalid HOCON surfaces a clear error") {
      val invalid = """name = "Broken"
                      |completed = "not-a-date"
                      |matches = []
                      |""".stripMargin
      for {
        result <- PeriodCodec.parseString(invalid).either
      } yield assertTrue(
        result.isLeft,
        result.left.exists(_.getMessage.contains("completed"))
      )
    }
  )
}
