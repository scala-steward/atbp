package ph.samson.atbp.liga.model

import ph.samson.atbp.liga.glicko.Tuning

/** Display label for a frozen tournament rating in bracket views. */
enum PlayerRatingLabel {
  case Rated(value: Double)
  case Unrated
}

object PlayerRatingLabel {

  def fromFrozen(rating: PlayerRating): PlayerRatingLabel = {
    val tuning = Tuning.Default
    if (
      rating.rd == tuning.maxDeviation &&
      rating.wins == 0 &&
      rating.losses == 0
    ) PlayerRatingLabel.Unrated
    else PlayerRatingLabel.Rated(rating.rating)
  }
}
