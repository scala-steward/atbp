package ph.samson.atbp.liga.js.director

/** Pure helpers for Define-step roster soft-remove (no-shows before lock). */
object RosterSoftRemove {

  def activeNames(names: List[String], removed: Set[String]): List[String] =
    names.filterNot(removed.contains)

  /** Names persisted by Save and Lock. Soft-removed rows are dropped; paste is
    * intentionally not a parameter — Apply is the only paste path.
    */
  def commitNames(names: List[String], removed: Set[String]): List[String] =
    activeNames(names, removed)

  def toggle(removed: Set[String], name: String): Set[String] =
    if (removed.contains(name)) removed - name else removed + name

  /** Always occupy the badge grid column so Remove stays in the last column. */
  def guestBadgeText(isGuest: Boolean): String =
    if (isGuest) "guest" else ""
}
