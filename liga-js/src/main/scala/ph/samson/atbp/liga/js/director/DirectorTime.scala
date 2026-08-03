package ph.samson.atbp.liga.js.director

import scala.scalajs.js

/** Browser-local clock formatting (same approach as audience "updated" time).
  */
object DirectorTime {

  def formatDoneTime(isoInstant: String): String =
    new js.Date(isoInstant).toLocaleTimeString()
}
