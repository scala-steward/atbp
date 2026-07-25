package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.bracket.RaceToScopes
import ph.samson.atbp.liga.js.api.Models.BracketMatch
import ph.samson.atbp.liga.js.api.Models.BracketMatchState

/** Pure bracket layout helpers for the director UI. */
object BracketLayout {

  enum Section(val label: String) {
    case Winners extends Section("Winners")
    case Losers extends Section("Losers")
    case GrandFinal extends Section("Grand Final")
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

  def groupLabel(section: Section, round: Int): String =
    section match {
      case Section.GrandFinal => section.label
      case _                  => s"${section.label} — round $round"
    }

  def matchLabel(matchId: String, bracketSize: Int): String =
    groupLabel(sectionOf(matchId), roundOf(matchId, bracketSize))

  private def log2(n: Int): Int =
    (math.log(n) / math.log(2)).toInt

  /** Bracket round encoded in match ids (`wb-2-1`, `lb-3-2`, `gf-1`). */
  def bracketRound(matchId: String, bracketSize: Int): Option[Int] =
    matchId match {
      case s"wb-$round-$_" => round.toIntOption
      case s"lb-$round-$_" => round.toIntOption
      case "gf-1"          => Some(log2(bracketSize))
      case _               => None
    }

  def roundOf(matchId: String, bracketSize: Int): Int =
    bracketRound(matchId, bracketSize).getOrElse(0)

  private def showInList(m: BracketMatch): Boolean =
    m.state != BracketMatchState.Pending ||
      m.playerA.isDefined || m.playerB.isDefined

  private def statusSortKey(m: BracketMatch): Int =
    m.state match {
      case BracketMatchState.Ready     => 0
      case BracketMatchState.Pending   => 1
      case BracketMatchState.Started   => 2
      case BracketMatchState.Completed => 3
    }

  private def matchSeedIndex(matchId: String): Int =
    matchId match {
      case s"wb-$_-$index" => index.toIntOption.getOrElse(0)
      case s"lb-$_-$index" => index.toIntOption.getOrElse(0)
      case "gf-1"          => 1
      case _               => 0
    }

  private def matchSortKey(m: BracketMatch): (Int, Int) =
    (statusSortKey(m), matchSeedIndex(m.id))

  private def listSectionOrder(section: Section): Int =
    section match {
      case Section.GrandFinal => 0
      case Section.Losers     => 1
      case Section.Winners    => 2
    }

  private def roundFullyCompleted(matches: List[BracketMatch]): Boolean =
    matches.forall(_.state == BracketMatchState.Completed)

  private def groupStackKey(
      section: Section,
      round: Int,
      allMatches: List[BracketMatch]
  ): (Int, Int, Int) =
    (
      if (roundFullyCompleted(allMatches)) 1 else 0,
      listSectionOrder(section),
      -round
    )

  private final case class PreparedGroup(
      section: Section,
      round: Int,
      allMatches: List[BracketMatch],
      shownMatches: List[BracketMatch]
  )

  def groupMatches(
      matches: List[BracketMatch],
      bracketSize: Int
  ): List[RoundGroup] =
    matches
      .groupBy(m => (sectionOf(m.id), roundOf(m.id, bracketSize)))
      .toList
      .map { case ((section, round), grouped) =>
        PreparedGroup(
          section = section,
          round = round,
          allMatches = grouped,
          shownMatches = grouped.filter(showInList).sortBy(matchSortKey)
        )
      }
      .filter(_.shownMatches.nonEmpty)
      .sortBy(prepared =>
        groupStackKey(prepared.section, prepared.round, prepared.allMatches)
      )
      .map(prepared =>
        RoundGroup(prepared.section, prepared.round, prepared.shownMatches)
      )

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
      raceToByScope: Map[String, Int]
  ): Option[Int] =
    RaceToScopes.keyForMatch(matchId).flatMap(raceToByScope.get)

  def resultLabel(matchDef: BracketMatch): Option[String] =
    if (matchDef.isBye && matchDef.state == BracketMatchState.Completed) {
      Some("bye")
    } else {
      matchDef.result.map(result => s"${result.scoreA}–${result.scoreB}")
    }
}
