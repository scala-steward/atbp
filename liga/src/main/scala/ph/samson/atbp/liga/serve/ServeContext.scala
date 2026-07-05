package ph.samson.atbp.liga.serve

import better.files.File
import ph.samson.atbp.liga.io.PeriodLoader
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.tournament.Replay
import zio.Task
import zio.ZIO

/** Runtime data roots for serve mode (period files + active tournament). */
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
}
