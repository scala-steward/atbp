package ph.samson.atbp.liga.serve

import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.tournament.EventCodec
import zio.json.*

/** Stable JSON shapes for read-only HTTP API responses. */
object ApiJson {

  import EventCodec.given

  final case class TournamentResponse(
      name: String,
      players: List[Player],
      completed: Boolean,
      roundRaceTo: Map[Int, Int],
      bracket: Option[Bracket],
      frozenRatings: List[PlayerRating]
  )

  final case class LeaderboardResponse(
      ratings: List[PlayerRating]
  )

  given JsonCodec[TournamentResponse] = DeriveJsonCodec.gen
  given JsonCodec[LeaderboardResponse] = DeriveJsonCodec.gen

  def sortRatings(ratings: List[PlayerRating]): List[PlayerRating] =
    ratings.sortBy(-_.rating)

  def tournamentFrom(state: TournamentState): TournamentResponse =
    TournamentResponse(
      name = state.name,
      players = state.players,
      completed = state.completed,
      roundRaceTo = state.roundRaceTo,
      bracket = state.bracket,
      frozenRatings = sortRatings(state.frozenRatings.values.toList)
    )

  def leaderboardFrom(ratings: List[PlayerRating]): LeaderboardResponse =
    LeaderboardResponse(ratings = sortRatings(ratings))
}
