package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.js.api.Models.*
import ph.samson.atbp.liga.model.PlayerRatingLabel

/** Joined tournament bracket data for completed results annotations. */
final case class BracketResultsContext(
    completed: Boolean,
    bracketSize: Int,
    matches: List[BracketMatch],
    latestRatingsByPlayer: Map[String, LatestRating],
    indexes: BracketResults.PlayerIndexes,
    earnedIndex: BracketEarnedRacks.EarnedIndex
) {

  def earnedFor(playerName: String, matchId: String): Option[(Int, Int)] =
    earnedIndex.earnedFor(playerName, matchId)

  def cellDisplay(
      playerName: String,
      matchId: String,
      frozenLabel: Option[PlayerRatingLabel]
  ): BracketResults.ResultsCellDisplay = {
    val (wins, losses) =
      indexes.recordByPlayer.getOrElse(playerName, (0, 0))
    BracketResults.resultsCellDisplay(
      completed = completed,
      matchId = matchId,
      lastMatchId = indexes.lastMatchByPlayer.get(playerName),
      wins = wins,
      losses = losses,
      frozenLabel = frozenLabel,
      latestRating = latestRatingsByPlayer.get(playerName)
    )
  }

  def nameEmphasis(
      playerName: String,
      matchId: String
  ): BracketResults.ResultsNameEmphasis =
    BracketResults.resultsNameEmphasis(
      completed = completed,
      playerName = playerName,
      matchId = matchId,
      matches = matches,
      lastMatchId = indexes.lastMatchByPlayer.get(playerName)
    )
}

object BracketResultsContext {

  def fromTournament(
      tournament: TournamentResponse,
      latestRatings: LatestRatingsResponse
  ): BracketResultsContext = {
    val bracket = tournament.bracket
    val bracketSize = bracket.map(_.size).getOrElse(0)
    val matches = bracket.map(_.matches).getOrElse(Nil)
    BracketResultsContext(
      completed = tournament.completed,
      bracketSize = bracketSize,
      matches = matches,
      latestRatingsByPlayer =
        latestRatings.ratings.map(row => row.player.name -> row).toMap,
      indexes = BracketResults.playerIndexes(matches, bracketSize),
      earnedIndex = BracketEarnedRacks.earnedIndex(
        matches,
        bracketSize,
        tournament.frozenRatings
      )
    )
  }

  def inactive(tournament: TournamentResponse): BracketResultsContext =
    fromTournament(tournament, LatestRatingsResponse(Nil))
}
