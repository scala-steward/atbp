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
            losersSection(shape, playerCount),
            grandFinalSection(shape)
          )
        case BracketFormat.Kind.CutDoubleElimination =>
          List(
            winnersSection(shape, playerCount),
            losersSection(shape, playerCount),
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
        val slots = shape.bracketSize >> round
        val players =
          if (round == 1) playerCount else shape.bracketSize >> (round - 1)
        val byes = if (round == 1) openingByes else 0
        val matches = slots - byes
        RoundCounts(round, players, matches, byes)
      }.toList
    SectionPreview(RaceToScopes.Section.Winners, rounds)
  }

  /** Opening WB byes reduce WB R1 losers, so early LB rounds can be
    * under-filled.
    */
  private def losersSection(
      shape: BracketFormat.Shape,
      playerCount: Int
  ): SectionPreview = {
    val openingByes = shape.bracketSize - playerCount
    val wb1Matches = (shape.bracketSize >> 1) - openingByes
    val rounds =
      (1 to shape.losersRounds)
        .foldLeft((List.empty[RoundCounts], 0)) {
          case ((acc, previousWinners), round) =>
            val slots = BracketFormat.losersMatchCount(shape, round)
            val players =
              if (round == 1) {
                wb1Matches
              } else if (round % 2 == 0) {
                val wbLosers = shape.bracketSize >> (round / 2 + 1)
                previousWinners + wbLosers
              } else {
                previousWinners
              }
            val counts = roundFromPlayers(round, players, slots)
            (acc :+ counts, counts.players - counts.matches)
        }
        ._1
    SectionPreview(RaceToScopes.Section.Losers, rounds)
  }

  /** Playable matches only. Under-filled rounds: matches = max(0, players -
    * slots).
    */
  private def roundFromPlayers(
      round: Int,
      players: Int,
      slots: Int
  ): RoundCounts = {
    val matches =
      if (players >= slots * 2) slots
      else math.max(0, players - slots)
    val byes = slots - matches
    RoundCounts(round, players, matches, byes)
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
