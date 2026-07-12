package ph.samson.atbp.liga.model

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

/** Handicap suggestion for a race-to-N match. */
final case class HandicapSuggestion(
    weakerPlayer: Player,
    handicap: Int,
    raceTo: Int
)
