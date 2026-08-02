package ph.samson.atbp.liga.js.audience

import ph.samson.atbp.liga.js.api.Models.*

/** Which idle feeds the audience surface needs for a tournament phase. */
object AudienceIdlePolicy {

  /** Phase-scoped audience UI: loading stages carry no payload; ready stages
    * do.
    */
  enum View {
    case LoadingTournament
    case LoadingLatestRatings
    case Idle(latestRatings: LatestRatingsResponse)
    case Setup(tournament: TournamentResponse)
    case Bracket(tournament: TournamentResponse)
  }

  def needsLatestRatings(phase: TournamentPhase): Boolean =
    phase == TournamentPhase.None || phase == TournamentPhase.Completed

  /** Whether a poll should hit `GET /api/latest-ratings`.
    *
    * Completed brackets join an immutable post-period feed — once cached, skip
    * refetch on every audience poll. Idle still refreshes so a remote Complete
    * can update the table.
    */
  def shouldFetchLatestRatingsOnRefresh(
      phase: TournamentPhase,
      cached: Option[LatestRatingsResponse]
  ): Boolean =
    needsLatestRatings(phase) &&
      (phase != TournamentPhase.Completed || cached.isEmpty)

  /** Map a latest-ratings attempt into a committed feed so idle can reach
    * Ready.
    *
    * On failure, yield an empty table and keep the error message for the
    * banner. Leaving `None` would stick the UI on LoadingLatestRatings forever.
    */
  def idleLatestRatingsFeed(
      attempt: Either[String, LatestRatingsResponse]
  ): (LatestRatingsResponse, Option[String]) =
    attempt match {
      case Right(feed) => (feed, None)
      case Left(err)   => (LatestRatingsResponse(Nil), Some(err))
    }

  def view(
      maybeTournament: Option[TournamentResponse],
      maybeLatestRatings: Option[LatestRatingsResponse]
  ): View =
    maybeTournament match {
      case None             => View.LoadingTournament
      case Some(tournament) =>
        val phase = TournamentPhase.fromApi(tournament.phase)
        if (needsLatestRatings(phase)) {
          maybeLatestRatings match {
            case None =>
              View.LoadingLatestRatings
            case Some(lr) if phase == TournamentPhase.None =>
              View.Idle(lr)
            case Some(_) =>
              View.Bracket(tournament)
          }
        } else {
          phase match {
            case TournamentPhase.Defining | TournamentPhase.Locked |
                TournamentPhase.RaceTo =>
              View.Setup(tournament)
            case TournamentPhase.Active | TournamentPhase.Completed =>
              View.Bracket(tournament)
            case TournamentPhase.None =>
              // needsLatestRatings(None) is true; unreachable guard for exhaustiveness
              View.LoadingLatestRatings
          }
        }
    }
}
