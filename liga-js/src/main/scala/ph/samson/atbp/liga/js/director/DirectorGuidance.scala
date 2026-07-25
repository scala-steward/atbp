package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.bracket.RaceToScopes
import ph.samson.atbp.liga.bracket.TournamentBounds
import ph.samson.atbp.liga.handicap.HandicapCap
import ph.samson.atbp.liga.js.api.Models.BracketMatch
import ph.samson.atbp.liga.js.api.Models.BracketMatchState
import ph.samson.atbp.liga.js.api.Models.TournamentResponse

/** Director-facing copy for match setup and API error translation. */
object DirectorGuidance {

  val matchWorkflowOverview: String =
    "Each match: Ready (compute spot) → Apply handicap → Start → Record result."

  def scoreboardScoreHint(raceTo: Int): String = {
    val exampleLoser = math.max(0, raceTo - 2)
    "Enter the numbers on the scoreboard (include spotted games). " +
      s"Example: Bob spotted 2, board reads $raceTo–$exampleLoser → enter $raceTo and $exampleLoser."
  }

  val localhostNote: String =
    "Director controls are localhost-only. Open /audience on the club TV."

  def missingRaceToBugHint(scopeKey: String): String =
    s"Race-to missing for $scopeKey — please file a bug."

  def missingRaceToHint(matchId: String): String =
    missingRaceToBugHint(
      RaceToScopes.keyForMatch(matchId).getOrElse(matchId)
    )

  def lockRosterHint(playerCount: Int): String =
    if (playerCount < TournamentBounds.MinPlayers) {
      s"Need at least ${TournamentBounds.MinPlayers} players to lock (currently $playerCount)."
    } else if (playerCount > TournamentBounds.MaxPlayers) {
      s"At most ${TournamentBounds.MaxPlayers} players allowed (currently $playerCount)."
    } else {
      ""
    }

  val lockSavesHint: String =
    "Lock saves your roster."

  val applyPasteHint: String =
    "Apply paste to update the preview — or Lock will use your pasted list."

  def seedHint: String =
    "Seeding freezes each player's rating from the period leaderboard. " +
      "Guests receive default ratings."

  def matchStepHint(
      matchDef: BracketMatch,
      resolvedRaceTo: Option[Int]
  ): String = {
    val needsRaceTo =
      matchDef.state == BracketMatchState.Ready ||
        matchDef.state == BracketMatchState.Started
    if (needsRaceTo && resolvedRaceTo.isEmpty) {
      missingRaceToHint(matchDef.id)
    } else {
      matchStepHintByState(matchDef)
    }
  }

  private def matchStepHintByState(matchDef: BracketMatch): String =
    matchDef.state match {
      case BracketMatchState.Pending =>
        "Waiting for both players to be assigned from earlier results."
      case BracketMatchState.Ready if matchDef.handicapSuggested.isEmpty =>
        "Step 1 of 4: click Ready to compute a handicap spot from frozen ratings."
      case BracketMatchState.Ready if matchDef.handicapApplied.isEmpty =>
        "Step 2 of 4: adjust the spot if needed, then Apply handicap."
      case BracketMatchState.Ready =>
        "Step 3 of 4: Start match once the spot is agreed."
      case BracketMatchState.Started =>
        "Step 4 of 4: enter scoreboard totals and record the result."
      case BracketMatchState.Completed =>
        "Match finished."
    }

  def handicapCap(raceTo: Int): Int =
    HandicapCap.capFor(raceTo)

  def handicapSpotLabel(weakerName: String, raceTo: Int, cap: Int): String =
    s"Spot for $weakerName (max $cap for Race to $raceTo)."

  def friendlyApiError(raw: String): String =
    friendlyApiError(raw, None)

  def friendlyApiErrorForSelectedMatch(
      raw: String,
      tournament: Option[TournamentResponse],
      selectedMatchId: Option[String]
  ): String = {
    val raceTo = tournament.flatMap { t =>
      selectedMatchId.flatMap { id =>
        t.bracket
          .flatMap(_.matches.find(_.id == id))
          .flatMap(matchDef =>
            BracketLayout.resolveMatchRaceTo(matchDef, t.raceToByScope)
          )
      }
    }
    friendlyApiError(raw, raceTo)
  }

  def friendlyApiError(raw: String, raceTo: Option[Int]): String = {
    val lower = raw.toLowerCase
    if (lower.contains("handicap must be applied first")) {
      "Apply a handicap before starting play."
    } else if (lower.contains("handicap already suggested")) {
      "Handicap is already computed — adjust and apply, or start after applying."
    } else if (lower.contains("match must be readied first")) {
      "Click Ready match first to compute a handicap suggestion."
    } else if (lower.contains("both players must be assigned")) {
      "Both players must be in the bracket before you can ready this match."
    } else if (lower.contains("scores cannot tie")) {
      "Scores cannot tie — one player must win."
    } else if (lower.contains("scores must be non-negative")) {
      "Scores must be zero or greater."
    } else if (lower.contains("duplicate player names")) {
      "Each player name must be unique on the roster."
    } else if (lower.contains("handicap must be at most")) {
      handicapCapError(raw, raceTo)
    } else if (lower.contains("handicap must be non-negative")) {
      "Handicap cannot be negative."
    } else if (lower.contains("winner score must be")) {
      winnerScoreError(raw, raceTo)
    } else if (lower.contains("loser score must be less than")) {
      loserScoreError(raw, raceTo)
    } else if (lower.startsWith("player count must be")) {
      s"Roster must have ${TournamentBounds.MinPlayers}–${TournamentBounds.MaxPlayers} players"
    } else {
      raw
    }
  }

  private def handicapCapError(raw: String, raceTo: Option[Int]): String = {
    val parsed = for {
      cap <- """at most (\d+)""".r.findFirstMatchIn(raw).map(_.group(1).toInt)
      rt <- raceToFromRaw(raw).orElse(raceTo)
    } yield (cap, rt)
    parsed match {
      case Some((cap, rt)) =>
        s"Handicap exceeds the maximum ($cap) for Race to $rt."
      case None =>
        "Handicap exceeds the maximum allowed for this race-to."
    }
  }

  private def winnerScoreError(raw: String, raceTo: Option[Int]): String =
    raceToFromRaw(raw).orElse(raceTo) match {
      case Some(rt) =>
        s"Winner's scoreboard total must be $rt (Race to $rt)."
      case None =>
        "Winner's scoreboard total must match the race-to for this round."
    }

  private def loserScoreError(raw: String, raceTo: Option[Int]): String =
    raceToFromRaw(raw).orElse(raceTo) match {
      case Some(rt) =>
        s"Loser's scoreboard total must be below $rt (Race to $rt)."
      case None =>
        "Loser's scoreboard total must be below the race-to."
    }

  private def raceToFromRaw(raw: String): Option[Int] =
    """race-to (\d+)""".r
      .findFirstMatchIn(raw)
      .map(_.group(1).toInt)
      .orElse(trailingInt(raw))

  private def trailingInt(raw: String): Option[Int] =
    """(\d+)\s*$""".r.findFirstMatchIn(raw).map(_.group(1).toInt)
}
