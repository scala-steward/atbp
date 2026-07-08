package ph.samson.atbp.liga.tournament

import better.files.File
import zio.*
import zio.test.*

import java.time.LocalDate

object ResumeSpec extends ZIOSpecDefault {

  private val createdOn = LocalDate.parse("2026-03-15")

  private def withTempDir[R, A](
      f: File => ZIO[R, Throwable, A]
  ): ZIO[R, Throwable, A] =
    ZIO.acquireReleaseWith(
      ZIO.attemptBlocking(File.newTemporaryDirectory("liga-resume"))
    )(dir => ZIO.attemptBlocking(dir.delete()).ignore)(f)

  private def copyFixture(
      source: File,
      targetName: String,
      root: File
  ): Unit = {
    val target = root / targetName
    source.copyTo(target, overwrite = true)
    ()
  }

  private val partialFixture =
    File(getClass.getResource("/tournaments/eight-player-partial"))

  private val completeFixture =
    File(getClass.getResource("/tournaments/eight-player-complete"))

  def spec = suite("Resume")(
    test("slugify normalizes director-provided names") {
      assertTrue(
        Resume.slugify("Spring Open") == "spring-open",
        Resume.slugify("  Foo---Bar!!  ") == "foo-bar",
        Resume.slugify("ALICE") == "alice"
      )
    },
    test("tournamentDirName uses date and slug") {
      assertTrue(
        Resume.tournamentDirName("Spring Open", createdOn) ==
          "tournament-20260315-spring-open"
      )
    },
    test("resolve resumes sole incomplete tournament") {
      withTempDir { root =>
        copyFixture(
          partialFixture,
          "tournament-20260315-spring-open",
          root
        )
        for {
          dir <- Resume.resolve(root)
        } yield assertTrue(
          dir.contains(root / "tournament-20260315-spring-open")
        )
      }
    },
    test("resolve fails when multiple incomplete tournaments exist") {
      withTempDir { root =>
        copyFixture(
          partialFixture,
          "tournament-20260315-spring-open",
          root
        )
        copyFixture(
          partialFixture,
          "tournament-20260315-fall-league",
          root
        )
        for {
          result <- Resume.resolve(root).either
        } yield assertTrue(
          result.isLeft,
          result.left.exists(_.getMessage.contains("multiple incomplete")),
          result.left.exists(_.getMessage.contains("spring-open")),
          result.left.exists(_.getMessage.contains("fall-league"))
        )
      }
    },
    test("resolve ignores completed tournaments") {
      withTempDir { root =>
        copyFixture(
          completeFixture,
          "tournament-20260315-spring-open",
          root
        )
        copyFixture(
          partialFixture,
          "tournament-20260316-fall-league",
          root
        )
        for {
          dir <- Resume.resolve(root)
        } yield assertTrue(
          dir.exists(_.name == "tournament-20260316-fall-league")
        )
      }
    },
    test("resolve returns None when no incomplete tournament exists") {
      withTempDir { root =>
        for {
          dir <- Resume.resolve(root)
        } yield assertTrue(dir.isEmpty)
      }
    }
  )
}
