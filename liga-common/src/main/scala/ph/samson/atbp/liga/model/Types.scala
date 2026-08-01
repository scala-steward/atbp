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

/** Bracket slot A or B (wire format "A" / "B"). */
enum MatchSide {
  case A, B
}

object MatchSide {
  def parse(value: String): Option[MatchSide] =
    value match {
      case "A" => Some(A)
      case "B" => Some(B)
      case _   => None
    }

  def wire(side: MatchSide): String =
    side match {
      case A => "A"
      case B => "B"
    }

  def select[A](side: MatchSide, whenA: => A, whenB: => A): A =
    side match {
      case A => whenA
      case B => whenB
    }

  def winnerFromForfeiting(forfeiting: MatchSide): MatchSide =
    select(forfeiting, B, A)
}
