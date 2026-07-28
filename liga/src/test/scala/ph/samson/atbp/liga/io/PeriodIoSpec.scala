package ph.samson.atbp.liga.io

import better.files.File
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.testsupport.PeriodHoconTestSupport
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
    test("plain write does not emit bracket match-id comments") {
      for {
        parsed <- PeriodCodec.parseFile(springOpen)
        written = PeriodWriter.write(parsed)
      } yield assertTrue(
        PeriodHoconTestSupport.matchObjectsLackBracketIdComments(written)
      )
    },
    test("write with mismatched match ID count returns Left") {
      for {
        parsed <- PeriodCodec.parseFile(springOpen)
      } yield {
        val result = PeriodWriter.write(parsed, List("only-one"))
        assertTrue(
          result.isLeft,
          result.left.exists(_.contains("match ID count"))
        )
      }
    },
    test("match ID comments ignore braces outside matches array") {
      val hocon = """name = "Test"
                    |completed = "2026-03-15"
                    |notes = {
                    |  reviewer = "human"
                    |}
                    |matches = [
                    |  {
                    |    player-a = "Alice"
                    |    player-b = "Bob"
                    |    score-a = 7
                    |    score-b = 4
                    |    race-to = 7
                    |    handicap-suggested = 0
                    |    handicap-applied = 0
                    |  }
                    |]
                    |""".stripMargin
      val result = PeriodWriter.injectMatchIdComments(hocon, List("wb-1-1"))
      val written = result.getOrElse("")
      val notesBlock = written.split("matches = \\[").head
      assertTrue(
        result.isRight,
        written.contains("# wb-1-1"),
        !notesBlock.contains("# wb-1-1")
      )
    },
    test(
      "write with match IDs puts # <id> as first line inside each match object"
    ) {
      for {
        parsed <- PeriodCodec.parseFile(springOpen)
        ids = List("wb-1-1", "wb-1-2")
        written = PeriodWriter.write(parsed, ids).getOrElse("")
        roundTripped <- PeriodCodec.parseString(written)
      } yield {
        val firstLines = PeriodHoconTestSupport.firstInteriorLines(written)
        assertTrue(
          PeriodWriter.write(parsed, ids).isRight,
          firstLines == List("# wb-1-1", "# wb-1-2"),
          roundTripped == parsed
        )
      }
    },
    test("invalid HOCON surfaces a clear error") {
      val invalid = """name = "Broken"
                      |completed = "not-a-date"
                      |matches = [
                      |  {
                      |    player-a = "Alice"
                      |    player-b = "Bob"
                      |    score-a = 1
                      |    score-b = 0
                      |    race-to = 1
                      |    handicap-suggested = 0
                      |    handicap-applied = 0
                      |  }
                      |]
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
