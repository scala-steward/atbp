package ph.samson.atbp.liga.js.audience

/** Pathname helpers for audience list vs spatial bracket surfaces. */
object AudienceRoute {

  val SpatialBracketPath: String = "/audience/bracket"

  def normalizePathname(pathname: String): String =
    if (pathname.length > 1 && pathname.endsWith("/")) pathname.dropRight(1)
    else pathname

  def isSpatialBracket(pathname: String): Boolean =
    normalizePathname(pathname) == SpatialBracketPath
}
