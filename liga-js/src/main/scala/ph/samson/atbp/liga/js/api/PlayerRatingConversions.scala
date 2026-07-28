package ph.samson.atbp.liga.js.api

import ph.samson.atbp.liga.js.api.Models.PlayerRating
import ph.samson.atbp.liga.model as shared

object PlayerRatingConversions {

  def toShared(rating: PlayerRating): shared.PlayerRating =
    shared.PlayerRating(
      shared.Player(rating.player.name),
      rating.rating,
      rating.rd,
      rating.wins,
      rating.losses
    )
}
