package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.js.api.Models.PlayerRating
import ph.samson.atbp.liga.js.api.Models.TournamentResponse

/** Frozen ratings and race-to scope needed for bracket handicap labels. */
final case class BracketHandicapContext(
    frozenRatings: List[PlayerRating],
    raceToByScope: Map[String, Int]
)

object BracketHandicapContext {

  def fromTournament(tournament: TournamentResponse): BracketHandicapContext =
    BracketHandicapContext(
      tournament.frozenRatings,
      tournament.raceToByScope
    )
}
