package ph.samson.atbp.liga.tournament

import ph.samson.atbp.liga.handicap.Handicap
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.tournament.events.TournamentEvent

import java.time.Instant

/** Pure tournament command handlers that validate lifecycle rules and produce
  * events.
  */
object Tournament {

  type Error = MatchLifecycle.Error

  def ready(
      state: TournamentState,
      matchId: String,
      seq: Int,
      at: Instant
  ): Either[Error, TournamentEvent.MatchReady] =
    for {
      matchDef <- MatchLifecycle.findMatch(state, matchId)
      _ <- MatchLifecycle.validateReady(state, matchDef)
      raceTo <- MatchLifecycle.resolveRaceTo(state, matchId)
      ratingA <- frozenRating(state, matchDef.playerA.get)
      ratingB <- frozenRating(state, matchDef.playerB.get)
      suggestion = Handicap.suggest(ratingA, ratingB, raceTo)
    } yield TournamentEvent.MatchReady(
      seq = seq,
      at = at,
      payload = MatchReadyPayload(
        matchId = matchId,
        handicapSuggested = suggestion.handicap
      )
    )

  def applyHandicap(
      state: TournamentState,
      matchId: String,
      handicap: Int,
      seq: Int,
      at: Instant
  ): Either[Error, TournamentEvent.HandicapApplied] =
    for {
      _ <- MatchLifecycle.requireActive(state)
      matchDef <- MatchLifecycle.findMatch(state, matchId)
      _ <- MatchLifecycle.validateHandicap(matchDef)
    } yield TournamentEvent.HandicapApplied(
      seq = seq,
      at = at,
      payload = HandicapAppliedPayload(
        matchId = matchId,
        handicapApplied = handicap
      )
    )

  def start(
      state: TournamentState,
      matchId: String,
      seq: Int,
      at: Instant
  ): Either[Error, TournamentEvent.MatchStarted] =
    for {
      _ <- MatchLifecycle.requireActive(state)
      matchDef <- MatchLifecycle.findMatch(state, matchId)
      _ <- MatchLifecycle.validateStart(matchDef)
    } yield TournamentEvent.MatchStarted(
      seq = seq,
      at = at,
      payload = MatchStartedPayload(matchId = matchId)
    )

  def recordResult(
      state: TournamentState,
      matchId: String,
      scoreA: Int,
      scoreB: Int,
      seq: Int,
      at: Instant
  ): Either[Error, TournamentEvent.MatchResult] =
    for {
      _ <- MatchLifecycle.requireActive(state)
      matchDef <- MatchLifecycle.findMatch(state, matchId)
      _ <- MatchLifecycle.validateResult(matchDef)
      _ <- validateScores(matchDef, scoreA, scoreB)
    } yield TournamentEvent.MatchResult(
      seq = seq,
      at = at,
      payload = MatchResultPayload(
        matchId = matchId,
        scoreA = scoreA,
        scoreB = scoreB
      )
    )

  private def frozenRating(
      state: TournamentState,
      player: Player
  ): Either[Error, PlayerRating] =
    state.frozenRatings
      .get(player)
      .toRight(MatchLifecycle.MissingRatingError(player))

  private def validateScores(
      matchDef: BracketMatch,
      scoreA: Int,
      scoreB: Int
  ): Either[Error, Unit] =
    if (scoreA == scoreB) {
      Left(
        MatchLifecycle.InvalidTransitionError(
          matchDef.id,
          "result",
          "scores cannot tie"
        )
      )
    } else if (scoreA < 0 || scoreB < 0) {
      Left(
        MatchLifecycle.InvalidTransitionError(
          matchDef.id,
          "result",
          "scores must be non-negative"
        )
      )
    } else {
      Right(())
    }
}
