package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.handicap.Handicap
import ph.samson.atbp.liga.js.api.Models.*
import ph.samson.atbp.liga.model as shared

/** Single handicap preview for a bracket match from frozen tournament ratings.
  */
final case class MatchHandicapPreview(
    weaker: shared.PlayerRating,
    stronger: shared.PlayerRating,
    suggestion: shared.HandicapSuggestion
) {
  def suggestedHandicap: Int = suggestion.handicap
  def weakerName: String = weaker.player.name
}

object MatchHandicapPreview {

  def fromMatch(
      frozenRatings: List[PlayerRating],
      matchDef: BracketMatch,
      raceTo: Int
  ): Option[MatchHandicapPreview] = {
    val ratingA =
      matchDef.playerA.flatMap(p => frozenRatings.find(_.player.name == p.name))
    val ratingB =
      matchDef.playerB.flatMap(p => frozenRatings.find(_.player.name == p.name))
    for {
      a <- ratingA
      b <- ratingB
    } yield {
      val sharedA = toSharedRating(a)
      val sharedB = toSharedRating(b)
      val suggestion = Handicap.suggest(sharedA, sharedB, raceTo)
      val (weaker, stronger) =
        if (suggestion.weakerPlayer == sharedA.player) (sharedA, sharedB)
        else (sharedB, sharedA)
      MatchHandicapPreview(weaker, stronger, suggestion)
    }
  }

  def fromMatch(
      tournament: TournamentResponse,
      matchDef: BracketMatch,
      raceTo: Int
  ): Option[MatchHandicapPreview] =
    fromMatch(tournament.frozenRatings, matchDef, raceTo)

  private def toSharedRating(rating: PlayerRating): shared.PlayerRating =
    shared.PlayerRating(
      shared.Player(rating.player.name),
      rating.rating,
      rating.rd,
      rating.wins,
      rating.losses
    )

  def forMatch(
      context: BracketHandicapContext,
      matchDef: BracketMatch
  ): Option[MatchHandicapPreview] =
    BracketLayout
      .resolveMatchRaceTo(matchDef, context.raceToByScope)
      .flatMap(fromMatch(context.frozenRatings, matchDef, _))
}
