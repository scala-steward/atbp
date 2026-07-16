package ph.samson.atbp.liga.serve

import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.tournament.EventCodec
import ph.samson.atbp.liga.tournament.TournamentPhase
import zio.json.*

/** Stable JSON shapes for read-only HTTP API responses. */
object ApiJson {

  import EventCodec.given

  final case class TournamentResponse(
      name: String,
      players: List[Player],
      completed: Boolean,
      phase: String,
      raceToByScope: Map[String, Int],
      bracket: Option[Bracket],
      frozenRatings: List[PlayerRating]
  )

  final case class LeaderboardResponse(
      ratings: List[PlayerRating]
  )

  final case class ConfigResponse(
      audiencePollIntervalSeconds: Int
  )

  given JsonCodec[TournamentResponse] = DeriveJsonCodec.gen
  given JsonCodec[LeaderboardResponse] = DeriveJsonCodec.gen
  given JsonCodec[ConfigResponse] = DeriveJsonCodec.gen

  def sortRatings(ratings: List[PlayerRating]): List[PlayerRating] =
    ratings.sortBy(-_.rating)

  def tournamentFrom(
      state: TournamentState,
      hasDir: Boolean
  ): TournamentResponse =
    TournamentResponse(
      name = state.name,
      players = state.players,
      completed = state.completed,
      phase = phaseLabel(TournamentPhase.derive(state, hasDir)),
      raceToByScope = state.raceToByScope,
      bracket = state.bracket,
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
