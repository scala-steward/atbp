package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.js.LatestRatingsView
import ph.samson.atbp.liga.js.api.Models.BracketMatch
import ph.samson.atbp.liga.js.api.Models.LatestRating
import ph.samson.atbp.liga.model.PlayerRatingLabel

/** Pure helpers for completed-bracket results annotations. */
object BracketResults {

  enum RatingMovementDisplay {
    case RatedDelta(frozen: Double, delta: Double)
    case NewRating(post: Double)
  }

  enum ResultsCellDisplay {
    case Skip
    case Annotate(wins: Int, losses: Int, movement: RatingMovementDisplay)
  }

  /** Name weight on completed brackets: last slot emphasized, earlier dimmed.
    */
  enum ResultsNameEmphasis {
    case Live
    case Last
    case Prior
  }

  /** Per-player last-slot and W–L, built once per completed bracket view. */
  final case class PlayerIndexes(
      lastMatchByPlayer: Map[String, String],
      recordByPlayer: Map[String, (Int, Int)]
  )

  def playerIndexes(
      matches: List[BracketMatch],
      bracketSize: Int
  ): PlayerIndexes = {
    val names = matches.iterator.flatMap { m =>
      m.playerA.iterator.map(_.name) ++ m.playerB.iterator.map(_.name)
    }.toSet
    val lastMatchByPlayer = names.iterator.flatMap { name =>
      lastParticipatingMatchId(name, matches, bracketSize).map(name -> _)
    }.toMap
    val recordByPlayer = names.iterator.map { name =>
      name -> tournamentRecord(name, matches)
    }.toMap
    PlayerIndexes(lastMatchByPlayer, recordByPlayer)
  }

  /** Non-bye match with a recorded score or forfeit. */
  def hasRecordedParticipation(matchDef: BracketMatch): Boolean =
    !matchDef.isBye &&
      (matchDef.result.nonEmpty || matchDef.forfeit.nonEmpty)

  def playerInMatch(playerName: String, matchDef: BracketMatch): Boolean =
    matchDef.playerA.exists(_.name == playerName) ||
      matchDef.playerB.exists(_.name == playerName)

  private def sectionOrdinal(section: BracketLayout.Section): Int =
    section match {
      case BracketLayout.Section.Winners           => 0
      case BracketLayout.Section.Losers            => 1
      case BracketLayout.Section.SingleElimination => 2
      case BracketLayout.Section.GrandFinal        => 3
    }

  private def progressionKey(matchId: String, bracketSize: Int): (Int, Int) =
    (
      sectionOrdinal(BracketLayout.sectionOf(matchId)),
      BracketLayout.roundOf(matchId, bracketSize)
    )

  /** Last match id where the player has a recorded non-bye result. */
  def lastParticipatingMatchId(
      playerName: String,
      matches: List[BracketMatch],
      bracketSize: Int
  ): Option[String] =
    matches
      .filter(m => playerInMatch(playerName, m) && hasRecordedParticipation(m))
      .maxByOption(m => progressionKey(m.id, bracketSize))
      .map(_.id)

  /** Tournament wins and losses for a player, excluding byes. */
  def tournamentRecord(
      playerName: String,
      matches: List[BracketMatch]
  ): (Int, Int) =
    matches.foldLeft((0, 0)) { case ((wins, losses), matchDef) =>
      if (
        playerInMatch(playerName, matchDef) && hasRecordedParticipation(
          matchDef
        )
      ) {
        BracketLayout.winnerSide(matchDef).fold((wins, losses)) { winner =>
          val onA = matchDef.playerA.exists(_.name == playerName)
          val onB = matchDef.playerB.exists(_.name == playerName)
          val won =
            (winner == BracketLayout.MatchWinnerSide.A && onA) ||
              (winner == BracketLayout.MatchWinnerSide.B && onB)
          if (won) (wins + 1, losses)
          else if (onA || onB) (wins, losses + 1)
          else (wins, losses)
        }
      } else {
        (wins, losses)
      }
    }

  def ratedDeltaCssClasses(delta: Double): List[String] = {
    val rounded = Math.round(delta)
    if (rounded > 0) List("rating-up")
    else if (rounded < 0) List("rating-down")
    else Nil
  }

  def formatRatedDeltaLine(frozen: Double, delta: Double): String =
    f"${frozen}%.0f ${LatestRatingsView.formatDelta(delta)}"

  def formatNewRatingLine(post: Double): String =
    f"${post}%.0f (new)"

  def resultsNameEmphasis(
      completed: Boolean,
      playerName: String,
      matchId: String,
      matches: List[BracketMatch],
      bracketSize: Int
  ): ResultsNameEmphasis = {
    val lastId =
      lastParticipatingMatchId(playerName, matches, bracketSize)
    resultsNameEmphasis(
      completed = completed,
      playerName = playerName,
      matchId = matchId,
      matches = matches,
      lastMatchId = lastId
    )
  }

  def resultsNameEmphasis(
      completed: Boolean,
      playerName: String,
      matchId: String,
      matches: List[BracketMatch],
      lastMatchId: Option[String]
  ): ResultsNameEmphasis = {
    if (!completed || playerName == "—") ResultsNameEmphasis.Live
    else {
      lastMatchId match {
        case None                          => ResultsNameEmphasis.Live
        case Some(last) if last == matchId => ResultsNameEmphasis.Last
        case Some(_)                       =>
          matches
            .find(_.id == matchId)
            .filter(m =>
              playerInMatch(playerName, m) && hasRecordedParticipation(m)
            )
            .fold(ResultsNameEmphasis.Live)(_ => ResultsNameEmphasis.Prior)
      }
    }
  }

  def nameClasses(
      emphasis: ResultsNameEmphasis,
      isWinner: Boolean
  ): String =
    emphasis match {
      case ResultsNameEmphasis.Last =>
        if (isWinner) "match-winner results-last" else "results-last"
      case ResultsNameEmphasis.Prior =>
        if (isWinner) "match-winner results-prior" else "results-prior"
      case ResultsNameEmphasis.Live =>
        if (isWinner) "match-winner" else ""
    }

  def resultsCellDisplay(
      completed: Boolean,
      playerName: String,
      matchId: String,
      matches: List[BracketMatch],
      bracketSize: Int,
      frozenLabel: Option[PlayerRatingLabel],
      latestRating: Option[LatestRating]
  ): ResultsCellDisplay = {
    val lastId =
      lastParticipatingMatchId(playerName, matches, bracketSize)
    val (wins, losses) = tournamentRecord(playerName, matches)
    resultsCellDisplay(
      completed = completed,
      matchId = matchId,
      lastMatchId = lastId,
      wins = wins,
      losses = losses,
      frozenLabel = frozenLabel,
      latestRating = latestRating
    )
  }

  def resultsCellDisplay(
      completed: Boolean,
      matchId: String,
      lastMatchId: Option[String],
      wins: Int,
      losses: Int,
      frozenLabel: Option[PlayerRatingLabel],
      latestRating: Option[LatestRating]
  ): ResultsCellDisplay = {
    if (!completed) ResultsCellDisplay.Skip
    else if (lastMatchId != Some(matchId) || latestRating.isEmpty) {
      ResultsCellDisplay.Skip
    } else {
      val movement = ratingMovementDisplay(frozenLabel, latestRating.get)
      ResultsCellDisplay.Annotate(wins, losses, movement)
    }
  }

  private def ratingMovementDisplay(
      frozenLabel: Option[PlayerRatingLabel],
      latestRating: LatestRating
  ): RatingMovementDisplay =
    frozenLabel match {
      case Some(PlayerRatingLabel.Unrated) =>
        RatingMovementDisplay.NewRating(latestRating.rating)
      case Some(PlayerRatingLabel.Rated(frozen)) =>
        RatingMovementDisplay.RatedDelta(frozen, latestRating.delta)
      case None =>
        RatingMovementDisplay.NewRating(latestRating.rating)
    }
}
