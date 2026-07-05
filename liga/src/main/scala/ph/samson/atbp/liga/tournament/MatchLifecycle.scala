package ph.samson.atbp.liga.tournament

import ph.samson.atbp.liga.model.*

/** Validates match lifecycle transitions for tournament replay and commands. */
object MatchLifecycle {

  sealed trait Error {
    def message: String
  }

  final case class TournamentCompletedError() extends Error {
    val message: String = "tournament is already completed"
  }

  final case class NoBracketError() extends Error {
    val message: String = "no bracket loaded"
  }

  final case class UnknownMatchError(matchId: String) extends Error {
    val message: String = s"unknown match: $matchId"
  }

  final case class InvalidTransitionError(
      matchId: String,
      action: String,
      reason: String
  ) extends Error {
    val message: String = s"$action rejected for $matchId: $reason"
  }

  final case class MissingRaceToError(matchId: String, round: Int)
      extends Error {
    val message: String =
      s"no race-to configured for round $round (match $matchId)"
  }

  final case class MissingRatingError(player: Player) extends Error {
    val message: String = s"no frozen rating for ${player.name}"
  }

  def requireActive(state: TournamentState): Either[Error, Unit] =
    if (state.completed) {
      Left(TournamentCompletedError())
    } else {
      Right(())
    }

  def findMatch(
      state: TournamentState,
      matchId: String
  ): Either[Error, BracketMatch] =
    state.bracket match {
      case None          => Left(NoBracketError())
      case Some(bracket) =>
        bracket.matches.find(_.id == matchId) match {
          case None    => Left(UnknownMatchError(matchId))
          case Some(m) => Right(m)
        }
    }

  def validateReady(
      state: TournamentState,
      matchDef: BracketMatch
  ): Either[Error, Unit] =
    for {
      _ <- requireActive(state)
      _ <-
        if (matchDef.state == BracketMatchState.Ready) {
          Right(())
        } else {
          Left(
            InvalidTransitionError(
              matchDef.id,
              "ready",
              s"match is ${matchDef.state}"
            )
          )
        }
      _ <-
        if (matchDef.playerA.nonEmpty && matchDef.playerB.nonEmpty) {
          Right(())
        } else {
          Left(
            InvalidTransitionError(
              matchDef.id,
              "ready",
              "both players must be assigned"
            )
          )
        }
      _ <-
        if (matchDef.handicapSuggested.isEmpty) {
          Right(())
        } else {
          Left(
            InvalidTransitionError(
              matchDef.id,
              "ready",
              "handicap already suggested"
            )
          )
        }
    } yield ()

  def validateHandicap(matchDef: BracketMatch): Either[Error, Unit] =
    matchDef.state match {
      case BracketMatchState.Ready =>
        if (matchDef.handicapSuggested.isEmpty) {
          Left(
            InvalidTransitionError(
              matchDef.id,
              "handicap",
              "match must be readied first"
            )
          )
        } else {
          Right(())
        }
      case BracketMatchState.Started | BracketMatchState.Completed =>
        Left(
          InvalidTransitionError(
            matchDef.id,
            "handicap",
            s"match is ${matchDef.state}"
          )
        )
      case BracketMatchState.Pending =>
        Left(
          InvalidTransitionError(
            matchDef.id,
            "handicap",
            "match is not ready"
          )
        )
    }

  def validateStart(matchDef: BracketMatch): Either[Error, Unit] =
    matchDef.state match {
      case BracketMatchState.Ready =>
        if (matchDef.handicapApplied.isEmpty) {
          Left(
            InvalidTransitionError(
              matchDef.id,
              "start",
              "handicap must be applied first"
            )
          )
        } else {
          Right(())
        }
      case other =>
        Left(
          InvalidTransitionError(
            matchDef.id,
            "start",
            s"match is $other"
          )
        )
    }

  def validateResult(matchDef: BracketMatch): Either[Error, Unit] =
    if (matchDef.state == BracketMatchState.Started) {
      Right(())
    } else {
      Left(
        InvalidTransitionError(
          matchDef.id,
          "result",
          s"match is ${matchDef.state}"
        )
      )
    }

  /** Bracket round encoded in match ids (`wb-2-1`, `lb-3-2`, `gf-1`). */
  def bracketRound(matchId: String, bracketSize: Int): Option[Int] =
    matchId match {
      case s"wb-$round-$_" =>
        round.toIntOption
      case s"lb-$round-$_" =>
        round.toIntOption
      case "gf-1" =>
        Some(log2(bracketSize))
      case _ => None
    }

  def resolveRaceTo(
      state: TournamentState,
      matchId: String
  ): Either[Error, Int] =
    for {
      bracket <- state.bracket.toRight(NoBracketError(): Error)
      round <- bracketRound(matchId, bracket.size)
        .toRight(
          InvalidTransitionError(
            matchId,
            "ready",
            "cannot determine bracket round"
          )
        )
      raceTo <- state.roundRaceTo
        .get(round)
        .toRight(MissingRaceToError(matchId, round))
    } yield raceTo

  private def log2(n: Int): Int =
    (math.log(n) / math.log(2)).toInt
}
