package ph.samson.atbp.liga.bracket

/** Shared Top N format shape for scopes, preview, and topology. */
object BracketFormat {

  enum Kind {
    case FullSingleElimination, ClassicDoubleElimination, CutDoubleElimination
  }

  final case class Shape(
      kind: Kind,
      topN: Int,
      bracketSize: Int,
      winnersRounds: Int,
      losersRounds: Int,
      seRounds: Int,
      singleMatchLosersFinal: Boolean,
      hasGrandFinal: Boolean,
      resetGrandFinalPossible: Boolean
  )

  def forRoster(playerCount: Int, topN: Int): Shape = {
    require(
      TournamentBounds.validPlayerCount(playerCount),
      TournamentBounds.invalidPlayerCountMessage(playerCount)
    )
    require(
      TopN.legalTopNs(playerCount).contains(topN),
      s"topN must be legal for roster size $playerCount: $topN"
    )
    forBracket(TournamentBounds.bracketSize(playerCount), topN)
  }

  def forBracket(bracketSize: Int, topN: Int): Shape = {
    require(
      isPowerOfTwo(bracketSize),
      s"bracket size must be a power of two: $bracketSize"
    )
    require(topN >= 1 && topN <= bracketSize, s"topN out of range: $topN")

    if (topN == bracketSize) {
      val rounds = log2(bracketSize)
      Shape(
        kind = Kind.FullSingleElimination,
        topN = topN,
        bracketSize = bracketSize,
        winnersRounds = 0,
        losersRounds = 0,
        seRounds = rounds,
        singleMatchLosersFinal = false,
        hasGrandFinal = false,
        resetGrandFinalPossible = false
      )
    } else if (topN >= 4) {
      val winners = cutWinnersRounds(bracketSize, topN)
      Shape(
        kind = Kind.CutDoubleElimination,
        topN = topN,
        bracketSize = bracketSize,
        winnersRounds = winners,
        losersRounds = cutLosersRounds(winners),
        seRounds = log2(topN),
        singleMatchLosersFinal = false,
        hasGrandFinal = false,
        resetGrandFinalPossible = false
      )
    } else {
      val winners = log2(bracketSize)
      Shape(
        kind = Kind.ClassicDoubleElimination,
        topN = topN,
        bracketSize = bracketSize,
        winnersRounds = winners,
        losersRounds = (winners - 1) * 2,
        seRounds = 0,
        singleMatchLosersFinal = true,
        hasGrandFinal = true,
        resetGrandFinalPossible = topN == 1
      )
    }
  }

  def losersMatchCount(shape: Shape, round: Int): Int =
    losersMatchCount(
      shape.bracketSize,
      round,
      shape.losersRounds,
      shape.singleMatchLosersFinal
    )

  def losersMatchCount(
      bracketSize: Int,
      round: Int,
      totalRounds: Int,
      singleMatchFinal: Boolean
  ): Int =
    if (singleMatchFinal && round == totalRounds) {
      1
    } else {
      bracketSize >> ((round + 1) / 2 + 1)
    }

  /** WB stops when undefeated remaining equals topN/2 (half the cut). */
  private def cutWinnersRounds(bracketSize: Int, topN: Int): Int =
    log2(bracketSize) - log2(topN) + 1

  /** Classic LB depth for cut WB length (no GF-feeding tail). */
  private def cutLosersRounds(winnersRounds: Int): Int =
    (winnersRounds - 1) * 2

  private def isPowerOfTwo(n: Int): Boolean =
    n > 0 && (n & (n - 1)) == 0

  private def log2(n: Int): Int =
    (math.log(n) / math.log(2)).toInt
}
