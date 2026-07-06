package ph.samson.atbp.liga.tournament

import better.files.File
import ph.samson.atbp.liga.io.DataLayout
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.tournament.events.TournamentEvent
import zio.Task
import zio.ZIO

import java.time.Instant
import java.time.LocalDate
import java.time.format.DateTimeFormatter

/** Tournament directory discovery and serve startup resume logic. */
object Resume {

  final case class ResumeError(message: String) extends Exception(message)

  private val DateFormat = DateTimeFormatter.BASIC_ISO_DATE

  def slugify(name: String): String = {
    val collapsed = name
      .trim
      .toLowerCase
      .replaceAll("[^a-z0-9]+", "-")
    collapsed.stripPrefix("-").stripSuffix("-")
  }

  def tournamentDirName(name: String, createdOn: LocalDate): String = {
    val slug = slugify(name)
    s"${DataLayout.TournamentDirPrefix}${createdOn.format(DateFormat)}-$slug"
  }

  def discoverTournamentDirs(dataDir: File): List[File] =
    if (!dataDir.exists) Nil
    else
      dataDir.listRecursively
        .filter(_.isDirectory)
        .filter(dir => dir.name.startsWith(DataLayout.TournamentDirPrefix))
        .toList
        .sortBy(_.pathAsString)

  def isIncomplete(dir: File): Task[Boolean] =
    Replay.replayDir(dir).map(state => !Replay.isComplete(state))

  def resolve(dataDir: File, newName: Option[String]): Task[File] =
    resolve(dataDir, newName, LocalDate.now(), Instant.now())

  def resolve(
      dataDir: File,
      newName: Option[String],
      createdOn: LocalDate,
      at: Instant
  ): Task[File] =
    for {
      dirs <- ZIO.attemptBlocking(discoverTournamentDirs(dataDir))
      incomplete <- ZIO.filter(dirs)(isIncomplete)
      selected <- (newName, incomplete) match {
        case (Some(name), Nil) =>
          createNew(dataDir, name, createdOn, at)

        case (Some(_), existing) =>
          ZIO.fail(
            ResumeError(
              "cannot use --new while incomplete tournaments exist: " +
                existing.map(_.pathAsString).mkString(", ")
            )
          )

        case (None, dir :: Nil) =>
          ZIO.succeed(dir)

        case (None, Nil) =>
          ZIO.fail(
            ResumeError(
              "no incomplete tournament found; use --new <name> to start one"
            )
          )

        case (None, multiple) =>
          ZIO.fail(
            ResumeError(
              "multiple incomplete tournaments: " +
                multiple.map(_.pathAsString).mkString(", ")
            )
          )
      }
    } yield selected

  private def createNew(
      dataDir: File,
      name: String,
      createdOn: LocalDate,
      at: Instant
  ): Task[File] = {
    val slug = slugify(name)
    if (slug.isEmpty) {
      ZIO.fail(
        ResumeError("tournament name must contain at least one letter or digit")
      )
    } else {
      val dir = dataDir / tournamentDirName(name, createdOn)
      for {
        exists <- ZIO.attemptBlocking(dir.exists)
        _ <- ZIO.when(exists) {
          ZIO.fail(
            ResumeError(s"tournament directory already exists: ${dir.pathAsString}")
          )
        }
        _ <- ZIO.attemptBlocking(
          dataDir.createDirectoryIfNotExists(createParents = true)
        )
        event = TournamentEvent.Created(
          seq = 1,
          at = at,
          payload = TournamentCreatedPayload(name = name, players = Nil)
        )
        _ <- EventLog.append(dir, event)
      } yield dir
    }
  }
}
