package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.handicap.Handicap
import ph.samson.atbp.liga.handicap.HandicapCap
import ph.samson.atbp.liga.handicap.WinProbability
import ph.samson.atbp.liga.model.PlayerRating

/** Pure helpers for Ready-stage handicap win-probability neighborhood display.
  */
object HandicapProbabilityHints {

  final case class ProbabilityRow(
      spot: Int,
      weakerProbability: Double,
      weakerPercent: String
  )

  def neighborhoodSpots(suggested: Int): List[Int] =
    Handicap.probabilityNeighborhoodSpots(suggested)

  def formatWeakerPercent(probability: Double): String =
    s"${(probability * 100.0).round}%"

  def formatSpot(spot: Int): String = s"+$spot"

  def capFor(raceTo: Int): Int = HandicapCap.capFor(raceTo)

  def isOverCap(spot: Int, raceTo: Int): Boolean =
    spot > capFor(raceTo)

  def headerLabel(weakerName: String, raceTo: Int): String = {
    val cap = capFor(raceTo)
    s"Spot to: $weakerName (max $cap for Race to $raceTo)"
  }

  def neighborhoodRows(
      weaker: PlayerRating,
      stronger: PlayerRating,
      raceTo: Int,
      suggested: Int
  ): List[ProbabilityRow] =
    neighborhoodSpots(suggested).map { spot =>
      val probability =
        WinProbability.matchWinProbability(weaker, stronger, raceTo, spot)
      ProbabilityRow(spot, probability, formatWeakerPercent(probability))
    }

  def typedSpotHint(
      weaker: PlayerRating,
      stronger: PlayerRating,
      raceTo: Int,
      suggested: Int,
      typedInput: String
  ): Option[String] =
    typedInput.trim.toIntOption.flatMap { typed =>
      val cap = capFor(raceTo)
      Option.when(
        typed >= 0 &&
          typed <= cap &&
          !neighborhoodSpots(suggested).contains(typed)
      ) {
        val probability =
          WinProbability.matchWinProbability(weaker, stronger, raceTo, typed)
        s"${formatSpot(typed)} → ${formatWeakerPercent(probability)}"
      }
    }
}
