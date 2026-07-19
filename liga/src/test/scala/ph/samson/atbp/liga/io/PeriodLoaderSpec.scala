package ph.samson.atbp.liga.io

import better.files.File
import ph.samson.atbp.liga.glicko.Leaderboard
import zio.test.*

import java.time.LocalDate

object PeriodLoaderSpec extends ZIOSpecDefault {

  private def fixture(name: String): File =
    File(getClass.getResource(s"/period-loader/$name"))

  def spec = suite("PeriodLoader")(
    test("discovery ignores paths under tournament-*") {
      val root = fixture("with-tournament")
      val files = DataLayout.discoverPeriodFiles(root)
      assertTrue(
        files.map(_.name) == List("visible.liga"),
        files.forall(!_.pathAsString.contains("tournament-"))
      )
    },
    test("orders periods by completed date regardless of filename") {
      for {
        loaded <- PeriodLoader.discover(fixture("good"))
      } yield assertTrue(
        loaded.map(_.period.completed) == List(
          LocalDate.parse("2026-01-10"),
          LocalDate.parse("2026-03-15")
        ),
        loaded.head.file.name == "a-first.liga",
        loaded.last.file.name == "z-last.liga"
      )
    },
    test("duplicate completed dates fail with both file paths") {
      for {
        result <- PeriodLoader.discover(fixture("duplicate")).either
      } yield assertTrue(
        result.isLeft,
        result.left.exists { error =>
          val message = error.getMessage
          message.contains("2026-05-01") &&
          message.contains("one.liga") &&
          message.contains("two.liga")
        }
      )
    },
    test("empty period files fail with a clear error") {
      for {
        result <- PeriodCodec.parseFile(fixture("empty/empty.liga")).either
      } yield assertTrue(
        result.isLeft,
        result.left.exists(_.getMessage.contains("zero matches"))
      )
    },
    test("golden fixture folds two periods through Glicko2") {
      val root = fixture("golden")
      for {
        loaded <- PeriodLoader.discover(root)
        ratings <- PeriodLoader.loadAll(root)
      } yield assertTrue(
        loaded.size == 2,
        ratings == Leaderboard.compute(loaded.map(_.period))
      )
    },
    test("same inputs produce identical output on repeated loads") {
      val root = fixture("golden")
      for {
        first <- PeriodLoader.loadAll(root)
        second <- PeriodLoader.loadAll(root)
      } yield assertTrue(first == second)
    }
  )
}
