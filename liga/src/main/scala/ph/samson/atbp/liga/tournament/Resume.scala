package ph.samson.atbp.liga.tournament

import better.files.File
import ph.samson.atbp.liga.io.DataLayout
import zio.Task
import zio.ZIO

import java.time.LocalDate

/** Tournament directory discovery and serve startup resume logic. */
object Resume {

  final case class ResumeError(message: String) extends Exception(message)

  def slugify(name: String): String = {
    val collapsed = name.trim.toLowerCase
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

  def resolve(dataDir: File): Task[Option[File]] =
    for {
      dirs <- ZIO.attemptBlocking(discoverTournamentDirs(dataDir))
      incomplete <- ZIO.filter(dirs)(isIncomplete)
      selected <- incomplete match {
        case dir :: Nil =>
          ZIO.succeed(Some(dir))

        case Nil =>
          ZIO.succeed(None)

        case multiple =>
          ZIO.fail(
            ResumeError(
              "multiple incomplete tournaments: " +
                multiple.map(_.pathAsString).mkString(", ")
            )
          )
      }
    } yield selected

  private val DateFormat = java.time.format.DateTimeFormatter.BASIC_ISO_DATE
}
