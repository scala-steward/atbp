package ph.samson.atbp.liga.io

import better.files.File
import ph.samson.atbp.liga.glicko.Leaderboard
import ph.samson.atbp.liga.model.*
import zio.Task
import zio.ZIO

import java.time.LocalDate

final case class LoadedPeriod(file: File, period: Period)

final case class DuplicateCompletedDate(
    completed: LocalDate,
    files: List[File]
) extends Exception(
      s"Duplicate completed date $completed in ${files.map(_.name).sorted.mkString(", ")}"
    )

object PeriodLoader {

  def discover(root: File): Task[List[LoadedPeriod]] =
    for {
      files <- ZIO.attemptBlocking(DataLayout.discoverPeriodFiles(root))
      loaded <- ZIO.foreach(files)(loadFile)
      _ <- validateUniqueCompletedDates(loaded)
    } yield loaded.sortBy(_.period.completed)

  def loadAll(root: File): Task[List[PlayerRating]] =
    discover(root).map(loaded => Leaderboard.compute(loaded.map(_.period)))

  private def loadFile(file: File): Task[LoadedPeriod] =
    PeriodCodec.parseFile(file).map(LoadedPeriod(file, _))

  private def validateUniqueCompletedDates(
      loaded: List[LoadedPeriod]
  ): Task[Unit] =
    ZIO
      .foreach(
        loaded
          .groupBy(_.period.completed)
          .collect { case (date, entries) if entries.size > 1 => date -> entries }
      ) { case (date, entries) =>
        ZIO.fail(DuplicateCompletedDate(date, entries.map(_.file)))
      }
      .unit
}
