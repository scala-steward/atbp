package ph.samson.atbp.liga.bracket

/** Scope keys for section-aware race-to configuration (`wb-3`, `lb-4`, `gf`,
  * `se-2`).
  */
object RaceToScopes {

  enum Section(val label: String, val order: Int) {
    case Winners extends Section("Winners Bracket", 0)
    case Losers extends Section("Losers Bracket", 1)
    case GrandFinal extends Section("Grand Final", 2)
    case SingleElimination extends Section("Single Elimination", 3)
  }

  final case class ScopeLabel(section: Section, roundLabel: String)

  def keyForMatch(matchId: String): Option[String] =
    matchId match {
      case s"wb-$round-$_" => round.toIntOption.map(keyForWinnersRound)
      case s"lb-$round-$_" => round.toIntOption.map(keyForLosersRound)
      case s"se-$round-$_" => round.toIntOption.map(keyForSingleElimRound)
      case "gf-1" | "gf-2" => Some(grandFinalScopeKey)
      case _               => None
    }

  def keyForWinnersRound(round: Int): String =
    s"wb-$round"

  def keyForLosersRound(round: Int): String =
    s"lb-$round"

  def keyForSingleElimRound(round: Int): String =
    s"se-$round"

  def grandFinalScopeKey: String =
    "gf"

  def requiredKeys(playerCount: Int): List[String] =
    requiredKeys(playerCount, 2)

  def requiredKeys(playerCount: Int, topN: Int): List[String] = {
    val shape = BracketFormat.forRoster(playerCount, topN)
    shape.kind match {
      case BracketFormat.Kind.FullSingleElimination =>
        (1 to shape.seRounds).map(keyForSingleElimRound).toList
      case BracketFormat.Kind.ClassicDoubleElimination =>
        val wb = (1 to shape.winnersRounds).map(keyForWinnersRound)
        val lb = (1 to shape.losersRounds).map(keyForLosersRound)
        wb.toList ++ lb.toList :+ grandFinalScopeKey
      case BracketFormat.Kind.CutDoubleElimination =>
        val wb = (1 to shape.winnersRounds).map(keyForWinnersRound)
        val lb = (1 to shape.losersRounds).map(keyForLosersRound)
        val se = (1 to shape.seRounds).map(keyForSingleElimRound)
        wb.toList ++ lb.toList ++ se.toList
    }
  }

  def scopeLabel(scope: String): ScopeLabel =
    scope match {
      case s"wb-$round" =>
        ScopeLabel(Section.Winners, s"Round $round")
      case s"lb-$round" =>
        ScopeLabel(Section.Losers, s"Round $round")
      case s"se-$round" =>
        ScopeLabel(Section.SingleElimination, s"Round $round")
      case "gf" =>
        ScopeLabel(Section.GrandFinal, "Grand Final")
      case _ =>
        ScopeLabel(Section.Winners, scope)
    }
}
