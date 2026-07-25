package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.js.api.Models.*

/** Which idle feeds the director surface needs for a tournament phase. */
object DirectorIdlePolicy {

  /** Phase-scoped director UI: loading stages carry no payload; ready stages
    * do.
    */
  enum View {
    case LoadingTournament
    case LoadingLeaderboard
    case LoadingLatestRatings
    case Idle(latestRatings: LatestRatingsResponse)
    case Wizard(
        tournament: TournamentResponse,
        leaderboard: LeaderboardResponse
    )
    case Live(tournament: TournamentResponse)
  }

  def needsLatestRatings(phase: TournamentPhase): Boolean =
    phase == TournamentPhase.None

  def needsLeaderboard(phase: TournamentPhase): Boolean =
    phase match {
      case TournamentPhase.Defining | TournamentPhase.Locked |
          TournamentPhase.RaceTo =>
        true
      case TournamentPhase.None | TournamentPhase.Active |
          TournamentPhase.Completed =>
        false
    }

  def view(
      maybeTournament: Option[TournamentResponse],
      maybeLeaderboard: Option[LeaderboardResponse],
      maybeLatestRatings: Option[LatestRatingsResponse]
  ): View =
    maybeTournament match {
      case None             => View.LoadingTournament
      case Some(tournament) =>
        val phase = TournamentPhase.fromApi(tournament.phase)
        if (needsLeaderboard(phase)) {
          maybeLeaderboard match {
            case None     => View.LoadingLeaderboard
            case Some(lb) => View.Wizard(tournament, lb)
          }
        } else if (needsLatestRatings(phase)) {
          maybeLatestRatings match {
            case None     => View.LoadingLatestRatings
            case Some(lr) => View.Idle(lr)
          }
        } else {
          View.Live(tournament)
        }
    }
}
