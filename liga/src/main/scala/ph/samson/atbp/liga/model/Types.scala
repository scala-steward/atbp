package ph.samson.atbp.liga.model

import java.time.LocalDate

/** Display-name player identity (case-sensitive). */
final case class Player(name: String)

/** Glicko2 rating snapshot plus career win–loss record. */
final case class PlayerRating(
    player: Player,
    rating: Double,
    rd: Double,
    wins: Int,
    losses: Int
)

/** Final scores for a completed match. */
final case class MatchResult(
    scoreA: Int,
    scoreB: Int
)

/** One completed period (typically one tournament) from a `*.liga` file. */
final case class Period(
    name: String,
    completed: LocalDate,
    format: Option[String] = None,
    raceTo: Option[Int] = None,
    matches: List[PeriodMatch] = Nil
)

/** One match within a completed period file. */
final case class PeriodMatch(
    playerA: Player,
    playerB: Player,
    scoreA: Int,
    scoreB: Int,
    raceTo: Int,
    handicapSuggested: Int,
    handicapApplied: Int
)

/** Handicap suggestion for a race-to-N match. */
final case class HandicapSuggestion(
    weakerPlayer: Player,
    handicap: Int,
    raceTo: Int
)

/** Bracket match lifecycle: pending → ready → started → completed. */
enum BracketMatchState {
  case Pending, Ready, Started, Completed
}

/** One match slot in a double-elimination bracket. */
final case class BracketMatch(
    id: String,
    playerA: Option[Player],
    playerB: Option[Player],
    state: BracketMatchState,
    raceTo: Option[Int] = None,
    handicapSuggested: Option[Int] = None,
    handicapApplied: Option[Int] = None,
    result: Option[MatchResult] = None
)

/** Full bracket tree for a tournament. */
final case class Bracket(
    size: Int,
    matches: List[BracketMatch]
)

/** Replay-derived tournament state. */
final case class TournamentState(
    name: String,
    players: List[Player],
    bracket: Option[Bracket] = None,
    frozenRatings: Map[Player, PlayerRating] = Map.empty,
    completed: Boolean = false
)

// --- Tournament event payloads (v1) ---

final case class TournamentCreatedPayload(
    name: String,
    players: List[Player]
)

final case class RoundRaceToSetPayload(
    round: Int,
    raceTo: Int
)

final case class BracketSeededPayload(
    frozenRatings: List[PlayerRating],
    bracket: Bracket
)

final case class MatchReadyPayload(
    matchId: String,
    handicapSuggested: Int
)

final case class HandicapAppliedPayload(
    matchId: String,
    handicapApplied: Int
)

final case class MatchStartedPayload(
    matchId: String
)

final case class MatchResultPayload(
    matchId: String,
    scoreA: Int,
    scoreB: Int
)

final case class TournamentCompletedPayload(
    completed: LocalDate
)
