package ph.samson.atbp.liga.serve

import ph.samson.atbp.liga.glicko.LatestRating
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.tournament.EventCodec
import ph.samson.atbp.liga.tournament.MatchWaitTiming
import ph.samson.atbp.liga.tournament.TournamentPhase
import zio.json.*

import java.time.Instant

/** Stable JSON shapes for read-only HTTP API responses. */
object ApiJson {

  import EventCodec.given

  final case class ApiBracketMatch(
      id: String,
      playerA: Option[Player],
      playerB: Option[Player],
      state: BracketMatchState,
      raceTo: Option[Int] = None,
      handicapSuggested: Option[Int] = None,
      handicapApplied: Option[Int] = None,
      result: Option[MatchResult] = None,
      isBye: Boolean = false,
      forfeit: Option[MatchForfeitInfo] = None,
      waitStartedAt: Option[String] = None,
      startedAt: Option[String] = None,
      completedAt: Option[String] = None,
      newPlayerRestSince: Option[String] = None
  )

  final case class ApiBracket(
      size: Int,
      matches: List[ApiBracketMatch]
  )

  final case class TournamentResponse(
      name: String,
      players: List[Player],
      completed: Boolean,
      phase: String,
      raceToByScope: Map[String, Int],
      bracket: Option[ApiBracket],
      frozenRatings: List[PlayerRating]
  )

  final case class LeaderboardResponse(
      ratings: List[PlayerRating]
  )

  final case class LatestRatingsResponse(
      ratings: List[LatestRating]
  )

  final case class ConfigResponse(
      audiencePollIntervalSeconds: Int
  )

  given JsonCodec[ApiBracketMatch] = DeriveJsonCodec.gen
  given JsonCodec[ApiBracket] = DeriveJsonCodec.gen
  given JsonCodec[TournamentResponse] = DeriveJsonCodec.gen
  given JsonCodec[LeaderboardResponse] = DeriveJsonCodec.gen
  given JsonCodec[LatestRating] = DeriveJsonCodec.gen
  given JsonCodec[LatestRatingsResponse] = DeriveJsonCodec.gen
  given JsonCodec[ConfigResponse] = DeriveJsonCodec.gen

  def sortRatings(ratings: List[PlayerRating]): List[PlayerRating] =
    ratings.sortBy(-_.rating)

  def apiBracketMatchFrom(
      matchDef: BracketMatch,
      timing: Option[MatchWaitTiming.Timing]
  ): ApiBracketMatch =
    ApiBracketMatch(
      id = matchDef.id,
      playerA = matchDef.playerA,
      playerB = matchDef.playerB,
      state = matchDef.state,
      raceTo = matchDef.raceTo,
      handicapSuggested = matchDef.handicapSuggested,
      handicapApplied = matchDef.handicapApplied,
      result = matchDef.result,
      isBye = matchDef.isBye,
      forfeit = matchDef.forfeit,
      waitStartedAt = timing.flatMap(_.waitStartedAt.map(instantToIso)),
      startedAt = timing.flatMap(_.startedAt.map(instantToIso)),
      completedAt = timing.flatMap(_.completedAt.map(instantToIso)),
      newPlayerRestSince =
        timing.flatMap(_.newPlayerRestSince.map(instantToIso))
    )

  private def instantToIso(instant: Instant): String = instant.toString

  def apiBracketFrom(
      bracket: Bracket,
      timing: Map[String, MatchWaitTiming.Timing]
  ): ApiBracket =
    ApiBracket(
      size = bracket.size,
      matches = bracket.matches.map { matchDef =>
        apiBracketMatchFrom(matchDef, timing.get(matchDef.id))
      }
    )

  def tournamentFrom(
      state: TournamentState,
      hasDir: Boolean,
      timing: Map[String, MatchWaitTiming.Timing]
  ): TournamentResponse =
    TournamentResponse(
      name = state.name,
      players = state.players,
      completed = state.completed,
      phase = phaseLabel(TournamentPhase.derive(state, hasDir)),
      raceToByScope = state.raceToByScope,
      bracket = state.bracket.map(apiBracketFrom(_, timing)),
      frozenRatings = sortRatings(state.frozenRatings.values.toList)
    )

  private def phaseLabel(phase: TournamentPhase): String =
    phase match {
      case TournamentPhase.None      => "none"
      case TournamentPhase.Defining  => "defining"
      case TournamentPhase.Locked    => "locked"
      case TournamentPhase.RaceTo    => "raceTo"
      case TournamentPhase.Active    => "active"
      case TournamentPhase.Completed => "completed"
    }

  def leaderboardFrom(ratings: List[PlayerRating]): LeaderboardResponse =
    LeaderboardResponse(ratings = sortRatings(ratings))

  def configFrom(bind: BindConfig): ConfigResponse =
    ConfigResponse(
      audiencePollIntervalSeconds = bind.audiencePollIntervalSeconds
    )
}
