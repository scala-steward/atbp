package ph.samson.atbp.liga.tournament

import ph.samson.atbp.liga.handicap.Handicap
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.tournament.events.TournamentEvent

import java.time.Instant
import java.time.LocalDate

/** Pure tournament command handlers that validate lifecycle rules and produce
  * events.
  */
object Tournament {

  type Error = MatchLifecycle.Error

  sealed trait WizardError {
    def message: String
  }

  final case class RosterLockedError() extends WizardError {
    val message: String = "cannot set players after roster is locked"
  }

  final case class AlreadySeededError() extends WizardError {
    val message: String = "cannot modify tournament after bracket is seeded"
  }

  final case class RosterAlreadyLockedError() extends WizardError {
    val message: String = "roster is already locked"
  }

  final case class NoPlayersError() extends WizardError {
    val message: String = "cannot lock roster with no players"
  }

  final case class InvalidPlayerCountError(count: Int) extends WizardError {
    val message: String = s"player count must be 8–64: $count"
  }

  final case class EmptyTournamentNameError() extends WizardError {
    val message: String =
      "tournament name must contain at least one letter or digit"
  }

  final case class DuplicatePlayersError() extends WizardError {
    val message: String = "roster contains duplicate player names"
  }

  def create(
      name: String,
      seq: Int,
      at: Instant
  ): Either[WizardError, TournamentEvent.Created] =
    if (Resume.slugify(name).isEmpty) {
      Left(EmptyTournamentNameError())
    } else {
      Right(
        TournamentEvent.Created(
          seq = seq,
          at = at,
          payload = TournamentCreatedPayload(name = name.trim, players = Nil)
        )
      )
    }

  def setPlayers(
      state: TournamentState,
      players: List[Player],
      seq: Int,
      at: Instant
  ): Either[WizardError, TournamentEvent.PlayersSet] =
    if (state.playersLocked) {
      Left(RosterLockedError())
    } else if (state.bracket.nonEmpty) {
      Left(AlreadySeededError())
    } else if (players.distinct.size != players.size) {
      Left(DuplicatePlayersError())
    } else {
      Right(
        TournamentEvent.PlayersSet(
          seq = seq,
          at = at,
          payload = PlayersSetPayload(players = players)
        )
      )
    }

  def lockPlayers(
      state: TournamentState,
      seq: Int,
      at: Instant
  ): Either[WizardError, TournamentEvent.PlayersLocked] =
    if (state.playersLocked) {
      Left(RosterAlreadyLockedError())
    } else if (state.players.isEmpty) {
      Left(NoPlayersError())
    } else if (state.players.size < 8 || state.players.size > 64) {
      Left(InvalidPlayerCountError(state.players.size))
    } else {
      Right(
        TournamentEvent.PlayersLocked(
          seq = seq,
          at = at,
          payload = PlayersLockedPayload()
        )
      )
    }

  def setRoundRaceTo(
      state: TournamentState,
      roundRaceTo: Map[Int, Int],
      startSeq: Int,
      at: Instant
  ): Either[WizardError, List[TournamentEvent.RoundRaceToSet]] =
    if (state.bracket.nonEmpty) {
      Left(AlreadySeededError())
    } else {
      Right(
        roundRaceTo.toList.sortBy(_._1).zipWithIndex.map {
          case ((round, raceTo), index) =>
            TournamentEvent.RoundRaceToSet(
              seq = startSeq + index,
              at = at,
              payload = RoundRaceToSetPayload(round = round, raceTo = raceTo)
            )
        }
      )
    }

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
      raceTo <- MatchLifecycle.resolveRaceTo(state, matchId)
      cap = (0.75 * raceTo).floor.toInt
      _ <-
        if (handicap < 0) {
          Left(
            MatchLifecycle.InvalidTransitionError(
              matchId,
              "handicap",
              "handicap must be non-negative"
            )
          )
        } else if (handicap > cap) {
          Left(
            MatchLifecycle.InvalidTransitionError(
              matchId,
              "handicap",
              s"handicap must be at most $cap for race-to $raceTo"
            )
          )
        } else {
          Right(())
        }
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
      _ <- validateScores(state, matchDef, scoreA, scoreB)
    } yield TournamentEvent.MatchResult(
      seq = seq,
      at = at,
      payload = MatchResultPayload(
        matchId = matchId,
        scoreA = scoreA,
        scoreB = scoreB
      )
    )

  def complete(
      state: TournamentState,
      completed: LocalDate,
      seq: Int,
      at: Instant
  ): Either[Error, TournamentEvent.TournamentCompleted] =
    for {
      _ <- MatchLifecycle.requireActive(state)
      _ <- PeriodEmission
        .toPeriod(state, completed)
        .left
        .map(message =>
          MatchLifecycle.InvalidTransitionError("", "complete", message)
        )
    } yield TournamentEvent.TournamentCompleted(
      seq = seq,
      at = at,
      payload = TournamentCompletedPayload(completed = completed)
    )

  private def frozenRating(
      state: TournamentState,
      player: Player
  ): Either[Error, PlayerRating] =
    state.frozenRatings
      .get(player)
      .toRight(MatchLifecycle.MissingRatingError(player))

  private def validateScores(
      state: TournamentState,
      matchDef: BracketMatch,
      scoreA: Int,
      scoreB: Int
  ): Either[Error, Unit] =
    for {
      raceTo <- MatchLifecycle.resolveRaceTo(state, matchDef.id)
      _ <-
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
          val winnerScore = math.max(scoreA, scoreB)
          val loserScore = math.min(scoreA, scoreB)
          if (winnerScore != raceTo) {
            Left(
              MatchLifecycle.InvalidTransitionError(
                matchDef.id,
                "result",
                s"winner score must be $raceTo"
              )
            )
          } else if (loserScore >= raceTo) {
            Left(
              MatchLifecycle.InvalidTransitionError(
                matchDef.id,
                "result",
                s"loser score must be less than $raceTo"
              )
            )
          } else {
            Right(())
          }
        }
    } yield ()
}
