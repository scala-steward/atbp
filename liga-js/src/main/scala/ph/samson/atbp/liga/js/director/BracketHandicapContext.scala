package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.js.api.Models.PlayerRating
import ph.samson.atbp.liga.js.api.Models.TournamentResponse
import ph.samson.atbp.liga.js.api.PlayerRatingConversions
import ph.samson.atbp.liga.model.PlayerRatingLabel

/** Frozen ratings and race-to scope needed for bracket handicap labels. */
final case class BracketHandicapContext(
    frozenRatings: List[PlayerRating],
    raceToByScope: Map[String, Int]
) {

  def ratingLabelFor(name: String): Option[PlayerRatingLabel] =
    if (name == "—") None
    else
      frozenRatings
        .find(_.player.name == name)
        .map(r =>
          PlayerRatingLabel.fromFrozen(PlayerRatingConversions.toShared(r))
        )
}

object BracketHandicapContext {

  def fromTournament(tournament: TournamentResponse): BracketHandicapContext =
    BracketHandicapContext(
      tournament.frozenRatings,
      tournament.raceToByScope
    )
}
