package ph.samson.atbp.liga.serve

import better.files.File
import ph.samson.atbp.liga.io.PeriodLoader
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.tournament.EventLog
import ph.samson.atbp.liga.tournament.Replay
import ph.samson.atbp.liga.tournament.Resume
import ph.samson.atbp.liga.tournament.Seed
import ph.samson.atbp.liga.tournament.Tournament
import ph.samson.atbp.liga.tournament.events.TournamentEvent
import zio.Task
import zio.ZIO

import java.time.Instant
import java.time.LocalDate

/** Runtime data roots for serve mode (period files + active tournament). */
object ServeContext {
  final case class CommandError(message: String) extends Exception(message)

  private val emptyState: TournamentState =
    TournamentState(name = "", players = Nil)
}

final case class ServeContext(
    dataDir: File,
    tournamentDir: Option[File]
) {
  def loadTournament: Task[TournamentState] =
    activeDirOption.flatMap {
      case Some(dir) => Replay.replayDir(dir)
      case None      => ZIO.succeed(ServeContext.emptyState)
    }

  /** Period-start snapshot: frozen tournament ratings when seeded, otherwise
    * ratings from discovered period files.
    */
  def loadLeaderboard(state: TournamentState): Task[List[PlayerRating]] =
    if (state.frozenRatings.nonEmpty) {
      ZIO.succeed(ApiJson.sortRatings(state.frozenRatings.values.toList))
    } else {
      PeriodLoader.loadAll(dataDir).map(ApiJson.sortRatings)
    }

  def createTournament(name: String): Task[TournamentState] =
    for {
      existing <- Resume.resolve(dataDir)
      _ <- ZIO.when(existing.nonEmpty) {
        ZIO.fail(
          ServeContext.CommandError("tournament directory already exists")
        )
      }
      createdOn = LocalDate.now()
      at = Instant.now()
      event <- ZIO.fromEither(
        Tournament
          .create(name, seq = 1, at)
          .left
          .map(err => ServeContext.CommandError(err.message))
      )
      dirName = Resume.tournamentDirName(name, createdOn)
      dir = dataDir / dirName
      exists <- ZIO.attemptBlocking(dir.exists)
      _ <- ZIO.when(exists) {
        ZIO.fail(
          ServeContext.CommandError(
            s"tournament directory already exists: ${dir.pathAsString}"
          )
        )
      }
      _ <- ZIO.attemptBlocking(
        dataDir.createDirectoryIfNotExists(createParents = true)
      )
      _ <- EventLog.append(dir, event)
      updated <- Replay.replayDir(dir)
    } yield updated

  def appendWizardEvents(
      events: List[TournamentEvent]
  ): Task[TournamentState] =
    for {
      dir <- activeDir
      _ <- ZIO.foldLeft(events)(()) { (_, event) =>
        EventLog.append(dir, event)
      }
      updated <- Replay.replayDir(dir)
    } yield updated

  def appendWizardEvent(
      event: TournamentEvent
  ): Task[TournamentState] =
    appendWizardEvents(List(event))

  def nextSeq: Task[Int] =
    for {
      dir <- activeDir
      events <- EventLog.read(dir)
    } yield events.map(_.seq).maxOption.getOrElse(0) + 1

  def seedBracket(roundRaceTo: Map[Int, Int]): Task[TournamentState] =
    for {
      state <- loadTournament
      periodRatings <- PeriodLoader.loadAll(dataDir)
      startSeq <- nextSeq
      at = Instant.now()
      events <- ZIO.fromEither(
        Seed
          .buildEvents(state, periodRatings, roundRaceTo, startSeq, at)
          .left
          .map(err => ServeContext.CommandError(err.message))
      )
      updated <- appendWizardEvents(events)
    } yield updated

  def applyMatchCommand[A <: TournamentEvent](
      command: (TournamentState, Int, Instant) => Either[Tournament.Error, A]
  ): Task[TournamentState] =
    for {
      state <- loadTournament
      dir <- activeDir
      seq <- nextSeq
      at = Instant.now()
      event <- ZIO.fromEither(
        command(state, seq, at).left.map(err =>
          ServeContext.CommandError(err.message)
        )
      )
      _ <- EventLog.append(dir, event)
      updated <- Replay.replayDir(dir)
    } yield updated

  def withTournamentDir(dir: File): ServeContext =
    copy(tournamentDir = Some(dir))

  private def activeDir: Task[File] =
    activeDirOption.flatMap {
      case Some(dir) => ZIO.succeed(dir)
      case None      =>
        ZIO.fail(
          ServeContext.CommandError(
            "no tournament directory; create one from the Director UI"
          )
        )
    }

  private def activeDirOption: Task[Option[File]] =
    tournamentDir match {
      case Some(dir) => ZIO.succeed(Some(dir))
      case None      => Resume.resolve(dataDir)
    }
}
