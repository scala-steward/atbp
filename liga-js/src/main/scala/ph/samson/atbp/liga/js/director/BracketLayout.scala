package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.js.api.Models.BracketMatch
import ph.samson.atbp.liga.js.api.Models.BracketMatchState

/** Pure bracket layout helpers for the director UI. */
object BracketLayout {

  enum Section(val label: String, val order: Int) {
    case Winners extends Section("Winners", 0)
    case Losers extends Section("Losers", 1)
    case GrandFinal extends Section("Grand Final", 2)
  }

  final case class RoundGroup(
      section: Section,
      round: Int,
      matches: List[BracketMatch]
  )

  def sectionOf(matchId: String): Section =
    matchId match {
      case s if s.startsWith("wb-") => Section.Winners
      case s if s.startsWith("lb-") => Section.Losers
      case _                        => Section.GrandFinal
    }

  def roundOf(matchId: String): Int =
    matchId match {
      case s"wb-$round-$_" => round.toIntOption.getOrElse(0)
      case s"lb-$round-$_" => round.toIntOption.getOrElse(0)
      case "gf-1"          => 1
      case _               => 0
    }

  def groupMatches(matches: List[BracketMatch]): List[RoundGroup] =
    matches
      .groupBy(m => (sectionOf(m.id), roundOf(m.id)))
      .toList
      .sortBy { case ((section, round), _) => (section.order, round) }
      .map { case ((section, round), grouped) =>
        RoundGroup(section, round, grouped.sortBy(_.id))
      }

  def isActionable(matchDef: BracketMatch): Boolean =
    matchDef.state == BracketMatchState.Ready ||
      matchDef.state == BracketMatchState.Started

  def allMatchesCompleted(matches: List[BracketMatch]): Boolean =
    matches.nonEmpty && matches.forall(_.state == BracketMatchState.Completed)

  def playerLabel(
      player: Option[ph.samson.atbp.liga.js.api.Models.Player]
  ): String =
    player.map(_.name).getOrElse("—")

  def stateLabel(state: BracketMatchState): String =
    state match {
      case BracketMatchState.Pending   => "pending"
      case BracketMatchState.Ready     => "ready"
      case BracketMatchState.Started   => "live"
      case BracketMatchState.Completed => "done"
    }

  def defaultRaceTo(
      matchId: String,
      bracketSize: Int,
      roundRaceTo: Map[Int, Int]
  ): Option[Int] = {
    val round =
      matchId match {
        case s"wb-$r-$_" => r.toIntOption
        case s"lb-$r-$_" => r.toIntOption
        case "gf-1"      =>
          val winnersRounds = (math.log(bracketSize) / math.log(2)).toInt
          Some(winnersRounds)
        case _ => None
      }
    round.flatMap(roundRaceTo.get)
  }
}
