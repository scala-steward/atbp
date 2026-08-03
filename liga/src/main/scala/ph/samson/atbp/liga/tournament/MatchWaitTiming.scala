package ph.samson.atbp.liga.tournament

import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.tournament.events.TournamentEvent

import java.time.Instant

/** API-only wait / Done Instants derived from the tournament event log. */
object MatchWaitTiming {

  final case class Timing(
      waitStartedAt: Option[Instant],
      completedAt: Option[Instant],
      newPlayerRestSince: Option[Instant] = None
  )

  private final case class SideArrivals(
      a: Option[Instant] = None,
      b: Option[Instant] = None
  )

  private final case class Accumulator(
      state: TournamentState,
      arrivals: Map[String, SideArrivals],
      completedAt: Map[String, Instant],
      playerCompletedAt: Map[String, Instant]
  )

  def project(
      events: List[TournamentEvent]
  ): Either[String, Map[String, Timing]] =
    projectWithState(events).map(_._2)

  /** Single-pass replay + timing projection for serve loads. */
  def projectWithState(
      events: List[TournamentEvent]
  ): Either[String, (TournamentState, Map[String, Timing])] =
    EventCodec.validateMonotonicSeq(events).flatMap { _ =>
      events
        .sortBy(_.seq)
        .foldLeft(
          Right(
            Accumulator(
              state = TournamentState(name = "", players = Nil),
              arrivals = Map.empty,
              completedAt = Map.empty,
              playerCompletedAt = Map.empty
            )
          ): Either[String, Accumulator]
        ) { (acc, event) =>
          acc.flatMap { prior =>
            Replay.applyEvent(prior.state, event).map { newState =>
              Accumulator(
                state = newState,
                arrivals = recordArrivals(
                  prior.state,
                  newState,
                  event.at,
                  prior.arrivals
                ),
                completedAt = recordCompletedAt(event, prior.completedAt),
                playerCompletedAt = recordPlayerCompletions(
                  event,
                  newState,
                  prior.playerCompletedAt
                )
              )
            }
          }
        }
        .map { finalAcc =>
          val timing = buildOutput(
            finalAcc.state,
            finalAcc.arrivals,
            finalAcc.completedAt,
            finalAcc.playerCompletedAt
          )
          (finalAcc.state, timing)
        }
    }

  private def recordArrivals(
      before: TournamentState,
      after: TournamentState,
      at: Instant,
      arrivals: Map[String, SideArrivals]
  ): Map[String, SideArrivals] = {
    val beforeById = bracketMatches(before).map(m => m.id -> m).toMap
    bracketMatches(after).foldLeft(arrivals) { (acc, matchDef) =>
      val prev = beforeById.get(matchDef.id)
      val prevA = prev.flatMap(_.playerA)
      val prevB = prev.flatMap(_.playerB)
      val current = acc.getOrElse(matchDef.id, SideArrivals())
      val withA =
        if (matchDef.playerA.isDefined && prevA.isEmpty) {
          current.copy(a = Some(at))
        } else {
          current
        }
      val withB =
        if (matchDef.playerB.isDefined && prevB.isEmpty) {
          withA.copy(b = Some(at))
        } else {
          withA
        }
      if (withB != acc.getOrElse(matchDef.id, SideArrivals())) {
        acc.updated(matchDef.id, withB)
      } else {
        acc
      }
    }
  }

  private def recordCompletedAt(
      event: TournamentEvent,
      completedAt: Map[String, Instant]
  ): Map[String, Instant] =
    event match {
      case TournamentEvent.MatchResult(_, at, payload) =>
        completedAt.updated(payload.matchId, at)
      case TournamentEvent.MatchForfeit(_, at, payload) =>
        completedAt.updated(payload.matchId, at)
      case _ => completedAt
    }

  private def recordPlayerCompletions(
      event: TournamentEvent,
      state: TournamentState,
      playerCompletedAt: Map[String, Instant]
  ): Map[String, Instant] =
    event match {
      case TournamentEvent.MatchResult(_, at, payload) =>
        matchById(state, payload.matchId).fold(playerCompletedAt) { matchDef =>
          if (matchDef.isBye) playerCompletedAt
          else recordPlayers(playerCompletedAt, matchDef, at)
        }
      case TournamentEvent.MatchForfeit(_, at, payload) =>
        matchById(state, payload.matchId).fold(playerCompletedAt) { matchDef =>
          if (matchDef.isBye) playerCompletedAt
          else recordPlayers(playerCompletedAt, matchDef, at)
        }
      case _ => playerCompletedAt
    }

  private def recordPlayers(
      map: Map[String, Instant],
      matchDef: BracketMatch,
      at: Instant
  ): Map[String, Instant] = {
    val withA =
      matchDef.playerA.fold(map) { player => map.updated(player.name, at) }
    matchDef.playerB.fold(withA) { player => withA.updated(player.name, at) }
  }

  private def bracketMatches(state: TournamentState): List[BracketMatch] =
    state.bracket.map(_.matches).getOrElse(Nil)

  private def matchById(
      state: TournamentState,
      matchId: String
  ): Option[BracketMatch] =
    bracketMatches(state).find(_.id == matchId)

  private def waitStartedAt(arrivals: SideArrivals): Option[Instant] = {
    val times = List(arrivals.a, arrivals.b).flatten
    if (times.isEmpty) None else Some(times.min)
  }

  private def laterArrivingSide(arrivals: SideArrivals): Option[MatchSide] =
    (arrivals.a, arrivals.b) match {
      case (Some(aAt), Some(bAt)) if aAt == bAt       => None
      case (Some(aAt), Some(bAt)) if aAt.isAfter(bAt) => Some(MatchSide.A)
      case (Some(_), Some(_))                         => Some(MatchSide.B)
      case _                                          => None
    }

  private def playerOnSide(
      matchDef: BracketMatch,
      side: MatchSide
  ): Option[Player] =
    MatchSide.select(side, matchDef.playerA, matchDef.playerB)

  private def newPlayerRestSince(
      matchDef: BracketMatch,
      arrivals: SideArrivals,
      playerCompletedAt: Map[String, Instant]
  ): Option[Instant] =
    if (matchDef.state == BracketMatchState.Ready) {
      laterArrivingSide(arrivals).flatMap { side =>
        playerOnSide(matchDef, side).flatMap(player =>
          playerCompletedAt.get(player.name)
        )
      }
    } else {
      None
    }

  private def buildOutput(
      state: TournamentState,
      arrivals: Map[String, SideArrivals],
      completedAt: Map[String, Instant],
      playerCompletedAt: Map[String, Instant]
  ): Map[String, Timing] =
    bracketMatches(state).map { matchDef =>
      val side = arrivals.getOrElse(matchDef.id, SideArrivals())
      val wait = waitStartedAt(side)
      val done =
        if (matchDef.state == BracketMatchState.Completed && !matchDef.isBye) {
          completedAt.get(matchDef.id)
        } else {
          None
        }
      val rest =
        newPlayerRestSince(matchDef, side, playerCompletedAt)
      matchDef.id -> Timing(
        waitStartedAt = wait,
        completedAt = done,
        newPlayerRestSince = rest
      )
    }.toMap
}
