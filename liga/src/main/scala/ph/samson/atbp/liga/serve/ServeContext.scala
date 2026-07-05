package ph.samson.atbp.liga.serve

import better.files.File
import ph.samson.atbp.liga.io.PeriodLoader
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.tournament.EventLog
import ph.samson.atbp.liga.tournament.Replay
import ph.samson.atbp.liga.tournament.Seed
import ph.samson.atbp.liga.tournament.Tournament
import ph.samson.atbp.liga.tournament.events.TournamentEvent
import zio.Task
import zio.ZIO

import java.time.Instant

/** Runtime data roots for serve mode (period files + active tournament). */
object ServeContext {
  final case class CommandError(message: String) extends Exception(message)
}

final case class ServeContext(
    dataDir: File,
    tournamentDir: File
) {
  def loadTournament: Task[TournamentState] =
    Replay.replayDir(tournamentDir)

  /** Period-start snapshot: frozen tournament ratings when seeded, otherwise
    * ratings from discovered period files.
    */
  def loadLeaderboard(state: TournamentState): Task[List[PlayerRating]] =
    if (state.frozenRatings.nonEmpty) {
      ZIO.succeed(ApiJson.sortRatings(state.frozenRatings.values.toList))
    } else {
      PeriodLoader.loadAll(dataDir).map(ApiJson.sortRatings)
    }

  def nextSeq: Task[Int] =
    EventLog.read(tournamentDir).map(_.map(_.seq).maxOption.getOrElse(0) + 1)

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
      updated <- appendEvents(events)
    } yield updated

  def applyMatchCommand[A <: TournamentEvent](
      command: (TournamentState, Int, Instant) => Either[Tournament.Error, A]
  ): Task[TournamentState] =
    for {
      state <- loadTournament
      seq <- nextSeq
      at = Instant.now()
      event <- ZIO.fromEither(
        command(state, seq, at).left.map(err =>
          ServeContext.CommandError(err.message)
        )
      )
      _ <- EventLog.append(tournamentDir, event)
      updated <- loadTournament
    } yield updated

  private def appendEvents(
      events: List[TournamentEvent]
  ): Task[TournamentState] =
    ZIO
      .foldLeft(events)(()) { (_, event) =>
        EventLog.append(tournamentDir, event)
      }
      .flatMap(_ => loadTournament)
}
