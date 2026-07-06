package ph.samson.atbp.liga.tournament

import better.files.File
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.tournament.events.TournamentEvent
import zio.*
import zio.test.*

import java.time.Instant
import java.time.LocalDate

object ResumeSpec extends ZIOSpecDefault {

  private val createdOn = LocalDate.parse("2026-03-15")
  private val at = Instant.parse("2026-03-15T18:00:00Z")

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
          dir <- Resume.resolve(root, newName = None, createdOn = createdOn, at = at)
        } yield assertTrue(dir.name == "tournament-20260315-spring-open")
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
          result <- Resume
            .resolve(root, newName = None, createdOn = createdOn, at = at)
            .either
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
          dir <- Resume.resolve(root, newName = None, createdOn = createdOn, at = at)
        } yield assertTrue(dir.name == "tournament-20260316-fall-league")
      }
    },
    test("resolve requires --new when no incomplete tournament exists") {
      withTempDir { root =>
        for {
          result <- Resume
            .resolve(root, newName = None, createdOn = createdOn, at = at)
            .either
        } yield assertTrue(
          result.isLeft,
          result.left.exists(_.getMessage.contains("--new"))
        )
      }
    },
    test("--new creates tournament directory and initial event") {
      withTempDir { root =>
        for {
          dir <- Resume.resolve(
            root,
            newName = Some("Spring Open"),
            createdOn = createdOn,
            at = at
          )
          events <- EventLog.read(dir)
        } yield assertTrue(
          dir.name == "tournament-20260315-spring-open",
          events == List(
            TournamentEvent.Created(
              seq = 1,
              at = at,
              payload = TournamentCreatedPayload(
                name = "Spring Open",
                players = Nil
              )
            )
          )
        )
      }
    },
    test("--new fails when incomplete tournaments already exist") {
      withTempDir { root =>
        copyFixture(
          partialFixture,
          "tournament-20260315-spring-open",
          root
        )
        for {
          result <- Resume
            .resolve(
              root,
              newName = Some("Fall League"),
              createdOn = createdOn,
              at = at
            )
            .either
        } yield assertTrue(
          result.isLeft,
          result.left.exists(_.getMessage.contains("incomplete"))
        )
      }
    }
  )
}
