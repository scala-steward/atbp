package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.bracket.EarnedRacks
import ph.samson.atbp.liga.js.api.Models.*
import ph.samson.atbp.liga.js.api.PlayerRatingConversions
import ph.samson.atbp.liga.model.Player
import ph.samson.atbp.liga.model.PlayerRating as SharedPlayerRating

import scala.math.Ordering.Implicits.infixOrderingOps

/** Pure earned-rack totals per bracket cell, derived from wire matches. */
object BracketEarnedRacks {

  final case class EarnedIndex(
      earnedByPlayerAndMatch: Map[(String, String), Option[(Int, Int)]]
  ) {

    def earnedFor(playerName: String, matchId: String): Option[(Int, Int)] =
      earnedByPlayerAndMatch.getOrElse((playerName, matchId), None)
  }

  def earnedIndex(
      matches: List[BracketMatch],
      bracketSize: Int,
      frozenRatings: List[ph.samson.atbp.liga.js.api.Models.PlayerRating]
  ): EarnedIndex = {
    val ratingsByPlayer = frozenRatings.map { rating =>
      Player(rating.player.name) -> PlayerRatingConversions.toShared(rating)
    }.toMap
    val names = matches.iterator.flatMap { matchDef =>
      matchDef.playerA.iterator.map(_.name) ++ matchDef.playerB.iterator.map(
        _.name
      )
    }.toSet
    val earnedByPlayerAndMatch = names.iterator.flatMap { name =>
      matches.iterator.map { matchDef =>
        (name, matchDef.id) -> earnedAtCell(
          name,
          matchDef,
          matches,
          bracketSize,
          ratingsByPlayer
        )
      }
    }.toMap
    EarnedIndex(earnedByPlayerAndMatch)
  }

  def earnedAtCell(
      playerName: String,
      matchDef: BracketMatch,
      matches: List[BracketMatch],
      bracketSize: Int,
      frozenRatings: Map[Player, SharedPlayerRating]
  ): Option[(Int, Int)] =
    if (playerName == "—") None
    else {
      val cutoffKey = cutoffProgressionKey(
        playerName,
        matchDef,
        matches,
        bracketSize
      )
      cutoffKey.flatMap { cutoff =>
        val played = matches
          .filter(m =>
            m.state == BracketMatchState.Completed &&
              BracketResults.playerInMatch(playerName, m) &&
              progressionKey(m.id, bracketSize) <= cutoff
          )
          .flatMap(toPlayedMatch(_, frozenRatings))
        val (won, lost) =
          EarnedRacks.earnedRecord(played, Player(playerName))
        if (won == 0 && lost == 0) None else Some((won, lost))
      }
    }

  private def cutoffProgressionKey(
      playerName: String,
      matchDef: BracketMatch,
      matches: List[BracketMatch],
      bracketSize: Int
  ): Option[(Int, Int)] =
    if (
      matchDef.state == BracketMatchState.Completed &&
      BracketResults.playerInMatch(playerName, matchDef) &&
      BracketResults.hasRecordedParticipation(matchDef)
    ) {
      Some(progressionKey(matchDef.id, bracketSize))
    } else {
      BracketResults
        .lastParticipatingMatchId(playerName, matches, bracketSize)
        .map(progressionKey(_, bracketSize))
    }

  private def progressionKey(matchId: String, bracketSize: Int): (Int, Int) =
    (
      sectionOrdinal(BracketLayout.sectionOf(matchId)),
      BracketLayout.roundOf(matchId, bracketSize)
    )

  private def sectionOrdinal(section: BracketLayout.Section): Int =
    section match {
      case BracketLayout.Section.Winners           => 0
      case BracketLayout.Section.Losers            => 1
      case BracketLayout.Section.SingleElimination => 2
      case BracketLayout.Section.GrandFinal        => 3
    }

  def toPlayedMatch(
      matchDef: BracketMatch,
      frozenRatings: Map[Player, SharedPlayerRating]
  ): Option[EarnedRacks.PlayedMatch] =
    if (matchDef.state != BracketMatchState.Completed) {
      None
    } else if (matchDef.isBye) {
      playedKind(matchDef, frozenRatings, EarnedRacks.MatchKind.Bye)
    } else if (matchDef.forfeit.nonEmpty) {
      playedKind(matchDef, frozenRatings, EarnedRacks.MatchKind.Forfeit)
    } else {
      matchDef.result.flatMap { _ =>
        playedKind(matchDef, frozenRatings, EarnedRacks.MatchKind.Played)
      }
    }

  private def playedKind(
      matchDef: BracketMatch,
      frozenRatings: Map[Player, SharedPlayerRating],
      kind: EarnedRacks.MatchKind
  ): Option[EarnedRacks.PlayedMatch] =
    for {
      playerA <- matchDef.playerA
      playerB <- matchDef.playerB
      ratingA <- frozenRatings.get(Player(playerA.name))
      ratingB <- frozenRatings.get(Player(playerB.name))
    } yield {
      val weaker = weakerPlayer(ratingA, ratingB)
      val (scoreA, scoreB) =
        matchDef.result match {
          case Some(result) => (result.scoreA, result.scoreB)
          case None         => (0, 0)
        }
      EarnedRacks.PlayedMatch(
        playerA = Player(playerA.name),
        playerB = Player(playerB.name),
        scoreA = scoreA,
        scoreB = scoreB,
        handicapApplied = matchDef.handicapApplied.getOrElse(0),
        weaker = Some(weaker),
        kind = kind
      )
    }

  private def weakerPlayer(
      a: SharedPlayerRating,
      b: SharedPlayerRating
  ): Player =
    if (a.rating != b.rating) {
      if (a.rating < b.rating) a.player else b.player
    } else {
      List(a, b).minBy(_.player.name).player
    }

  def formatLiveRatingLine(
      label: ph.samson.atbp.liga.model.PlayerRatingLabel,
      earned: Option[(Int, Int)]
  ): String =
    earned match {
      case Some((w, l)) =>
        label match {
          case ph.samson.atbp.liga.model.PlayerRatingLabel.Rated(r) =>
            f"${r}%.0f ($w-$l)"
          case ph.samson.atbp.liga.model.PlayerRatingLabel.Unrated =>
            s"unrated ($w-$l)"
        }
      case None =>
        label match {
          case ph.samson.atbp.liga.model.PlayerRatingLabel.Rated(r) =>
            f"${r}%.0f"
          case ph.samson.atbp.liga.model.PlayerRatingLabel.Unrated =>
            "unrated"
        }
    }

  def appendEarnedToMovementLine(
      movementLine: String,
      earned: Option[(Int, Int)]
  ): String =
    earned match {
      case Some((w, l)) => s"$movementLine ($w-$l)"
      case None         => movementLine
    }
}
