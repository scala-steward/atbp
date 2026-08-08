package ph.samson.atbp.liga.bracket

/** Pure bracket structure preview from locked roster size and Top N cut. */
object BracketPreview {

  final case class RoundCounts(
      round: Int,
      players: Int,
      matches: Int,
      byes: Int
  )

  final case class SectionPreview(
      section: RaceToScopes.Section,
      rounds: List[RoundCounts],
      resetGrandFinalPossible: Boolean = false
  )

  final case class Preview(
      playerCount: Int,
      topN: Int,
      bracketSize: Int,
      sections: List[SectionPreview]
  )

  def apply(playerCount: Int, topN: Int): Preview = {
    val shape = BracketFormat.forRoster(playerCount, topN)
    val sections =
      shape.kind match {
        case BracketFormat.Kind.FullSingleElimination =>
          List(singleElimSection(shape))
        case BracketFormat.Kind.ClassicDoubleElimination =>
          List(
            winnersSection(shape, playerCount),
            losersSection(shape),
            grandFinalSection(shape)
          )
        case BracketFormat.Kind.CutDoubleElimination =>
          List(
            winnersSection(shape, playerCount),
            losersSection(shape),
            singleElimSection(shape)
          )
      }
    Preview(playerCount, topN, shape.bracketSize, sections)
  }

  private def winnersSection(
      shape: BracketFormat.Shape,
      playerCount: Int
  ): SectionPreview = {
    val openingByes = shape.bracketSize - playerCount
    val rounds =
      (1 to shape.winnersRounds).map { round =>
        val matches = shape.bracketSize >> round
        val players =
          if (round == 1) playerCount else shape.bracketSize >> (round - 1)
        val byes = if (round == 1) openingByes else 0
        RoundCounts(round, players, matches, byes)
      }.toList
    SectionPreview(RaceToScopes.Section.Winners, rounds)
  }

  private def losersSection(shape: BracketFormat.Shape): SectionPreview = {
    val rounds =
      (1 to shape.losersRounds).map { round =>
        val matches = BracketFormat.losersMatchCount(shape, round)
        RoundCounts(round, matches * 2, matches, byes = 0)
      }.toList
    SectionPreview(RaceToScopes.Section.Losers, rounds)
  }

  private def grandFinalSection(shape: BracketFormat.Shape): SectionPreview =
    SectionPreview(
      section = RaceToScopes.Section.GrandFinal,
      rounds = List(RoundCounts(round = 1, players = 2, matches = 1, byes = 0)),
      resetGrandFinalPossible = shape.resetGrandFinalPossible
    )

  private def singleElimSection(shape: BracketFormat.Shape): SectionPreview = {
    val rounds =
      (1 to shape.seRounds).map { round =>
        val matches = shape.topN >> round
        val players = shape.topN >> (round - 1)
        RoundCounts(round, players, matches, byes = 0)
      }.toList
    SectionPreview(RaceToScopes.Section.SingleElimination, rounds)
  }
}
