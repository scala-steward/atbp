package ph.samson.atbp.liga.tournament

import better.files.File
import ph.samson.atbp.liga.bracket.Advancement
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.tournament.events.TournamentEvent
import zio.Task
import zio.ZIO

/** Fold append-only tournament events into replay state. */
object Replay {

  final case class ReplayError(message: String) extends Exception(message)

  def replayDir(dir: File): Task[TournamentState] =
    EventLog.read(dir).flatMap { events =>
      ZIO.fromEither(replay(events).left.map(ReplayError(_)))
    }

  def replay(events: List[TournamentEvent]): Either[String, TournamentState] =
    for {
      _ <- EventCodec.validateMonotonicSeq(events)
      state <- events.sortBy(_.seq).foldLeft(Right(empty): Either[String, TournamentState]) {
        case (Right(current), event) => applyEvent(current, event)
        case (left, _)               => left
      }
    } yield state

  def isComplete(state: TournamentState): Boolean =
    state.completed

  private val empty: TournamentState =
    TournamentState(name = "", players = Nil)

  private def applyEvent(
      state: TournamentState,
      event: TournamentEvent
  ): Either[String, TournamentState] =
    event match {
      case TournamentEvent.Created(_, _, payload) =>
        Right(
          state.copy(
            name = payload.name,
            players = payload.players
          )
        )

      case TournamentEvent.RoundRaceToSet(_, _, payload) =>
        Right(
          state.copy(
            roundRaceTo = state.roundRaceTo.updated(payload.round, payload.raceTo)
          )
        )

      case TournamentEvent.BracketSeeded(_, _, payload) =>
        Right(
          state.copy(
            frozenRatings = payload.frozenRatings.map(r => r.player -> r).toMap,
            bracket = Some(payload.bracket)
          )
        )

      case TournamentEvent.MatchReady(_, _, payload) =>
        updateMatch(state, payload.matchId) { matchDef =>
          matchDef.copy(
            state = BracketMatchState.Ready,
            handicapSuggested = Some(payload.handicapSuggested)
          )
        }

      case TournamentEvent.HandicapApplied(_, _, payload) =>
        updateMatch(state, payload.matchId) { matchDef =>
          matchDef.copy(handicapApplied = Some(payload.handicapApplied))
        }

      case TournamentEvent.MatchStarted(_, _, payload) =>
        updateMatch(state, payload.matchId) { matchDef =>
          matchDef.copy(state = BracketMatchState.Started)
        }

      case TournamentEvent.MatchResult(_, _, payload) =>
        applyMatchResult(state, payload)

      case TournamentEvent.TournamentCompleted(_, _, _) =>
        Right(state.copy(completed = true))
    }

  private def updateMatch(
      state: TournamentState,
      matchId: String
  )(
      update: BracketMatch => BracketMatch
  ): Either[String, TournamentState] =
    state.bracket match {
      case None => Left(s"no bracket loaded for match $matchId")
      case Some(bracket) =>
        bracket.matches.find(_.id == matchId) match {
          case None => Left(s"unknown match: $matchId")
          case Some(_) =>
            val updatedMatches = bracket.matches.map { matchDef =>
              if (matchDef.id == matchId) {
                update(matchDef)
              } else {
                matchDef
              }
            }
            Right(state.copy(bracket = Some(bracket.copy(matches = updatedMatches))))
        }
    }

  private def applyMatchResult(
      state: TournamentState,
      payload: MatchResultPayload
  ): Either[String, TournamentState] =
    for {
      bracket <- state.bracket.toRight("no bracket loaded for match result")
      matchDef <- bracket.matches
        .find(_.id == payload.matchId)
        .toRight(s"unknown match: ${payload.matchId}")
      winner <- winnerFromScores(matchDef, payload.scoreA, payload.scoreB)
      advanced <- Advancement.advance(bracket, payload.matchId, winner)
      withScores = advanced.bracket.copy(
        matches = advanced.bracket.matches.map { current =>
          if (current.id == payload.matchId) {
            current.copy(
              state = BracketMatchState.Completed,
              result = Some(MatchResult(payload.scoreA, payload.scoreB))
            )
          } else {
            current
          }
        }
      )
    } yield state.copy(bracket = Some(withScores))

  private def winnerFromScores(
      matchDef: BracketMatch,
      scoreA: Int,
      scoreB: Int
  ): Either[String, Player] =
  if (scoreA == scoreB) {
      Left(s"tie score in ${matchDef.id}")
    } else if (scoreA > scoreB) {
      matchDef.playerA.toRight(s"no player A in ${matchDef.id}")
    } else {
      matchDef.playerB.toRight(s"no player B in ${matchDef.id}")
    }
}
