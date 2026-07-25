package ph.samson.atbp.liga.js

import com.raquo.laminar.api.L.*
import ph.samson.atbp.liga.js.api.Models.*

/** Shared Latest Ratings table for director and audience idle surfaces. */
object LatestRatingsView {

  def apply(latestRatings: LatestRatingsResponse): Div =
    div(
      h2("Latest Ratings"),
      table(
        cls := "leaderboard-table",
        thead(
          tr(
            th("Player"),
            th("Rating"),
            th("Delta")
          )
        ),
        tbody(
          latestRatings.ratings.map { row =>
            tr(
              td(row.player.name),
              td(f"${row.rating}%.0f"),
              td(formatDelta(row.delta))
            )
          }
        )
      )
    )

  private[js] def formatDelta(delta: Double): String = {
    val rounded = Math.round(delta)
    if (rounded == 0L) "0"
    else if (rounded > 0) s"+$rounded"
    else rounded.toString
  }
}
