package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.js.api.Models.*

/** Which idle feeds the director surface needs for a tournament phase. */
object DirectorIdlePolicy {

  /** Phase-scoped director UI: loading stages carry no payload; ready stages
    * do.
    *
    * Wizard does not embed the leaderboard: Define soft-remove state must
    * survive leaderboard refreshes, so the shell remounts on tournament (and
    * idle latest-ratings) changes only. Leaderboard arrives via signal.
    */
  enum View {
    case LoadingTournament
    case LoadingLatestRatings
    case Idle(latestRatings: LatestRatingsResponse)
    case Wizard(tournament: TournamentResponse)
    case Live(tournament: TournamentResponse)
  }

  def needsLatestRatings(phase: TournamentPhase): Boolean =
    phase == TournamentPhase.None || phase == TournamentPhase.Completed

  /** After a tournament write, drop any cached latest-ratings feed.
    *
    * Idle loads pre-tournament movement; keeping that `Some` across create /
    * play / Complete would let `view` skip LoadingLatestRatings and join stale
    * deltas into a completed bracket. Callers refetch when
    * `needsLatestRatings(phase)`.
    */
  def retainedLatestRatingsAfterWrite(
      _phase: TournamentPhase,
      _previous: Option[LatestRatingsResponse]
  ): Option[LatestRatingsResponse] =
    None

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
      maybeLatestRatings: Option[LatestRatingsResponse]
  ): View =
    maybeTournament match {
      case None             => View.LoadingTournament
      case Some(tournament) =>
        val phase = TournamentPhase.fromApi(tournament.phase)
        if (needsLeaderboard(phase)) {
          View.Wizard(tournament)
        } else if (needsLatestRatings(phase)) {
          maybeLatestRatings match {
            case None =>
              View.LoadingLatestRatings
            case Some(lr) if phase == TournamentPhase.None =>
              View.Idle(lr)
            case Some(_) =>
              View.Live(tournament)
          }
        } else {
          View.Live(tournament)
        }
    }
}
