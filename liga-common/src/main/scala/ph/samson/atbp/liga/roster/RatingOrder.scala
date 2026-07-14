package ph.samson.atbp.liga.roster

import ph.samson.atbp.liga.model.PlayerRating

/** Best-first ordering for roster preview and seeding parity. */
object RatingOrder {

  def sortBestFirst(ratings: List[PlayerRating]): List[PlayerRating] =
    ratings.sortWith(compareBestFirst)

  /** True when `a` should rank above `b` (higher seed). */
  def compareBestFirst(a: PlayerRating, b: PlayerRating): Boolean = {
    val byRating = a.rating.compare(b.rating)
    if (byRating > 0) {
      true
    } else if (byRating < 0) {
      false
    } else {
      val byRd = a.rd.compare(b.rd)
      if (byRd < 0) {
        true
      } else if (byRd > 0) {
        false
      } else {
        a.player.name < b.player.name
      }
    }
  }
}
