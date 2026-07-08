package ph.samson.atbp.liga.js.director

import com.raquo.laminar.api.L.*
import ph.samson.atbp.liga.js.api.Models.*

/** Period leaderboard panel shown before a tournament exists. */
object LeaderboardView {

  def apply(
      leaderboard: LeaderboardResponse,
      busy: Signal[Boolean],
      onCreate: Observer[String]
  ): Div = {
    val tournamentName = Var("")

    div(
      cls := "leaderboard-panel",
      h2("Period leaderboard"),
      table(
        cls := "leaderboard-table",
        thead(
          tr(
            th("Player"),
            th("Rating"),
            th("W–L")
          )
        ),
        tbody(
          leaderboard.ratings.map { rating =>
            tr(
              td(rating.player.name),
              td(f"${rating.rating}%.0f"),
              td(s"${rating.wins}–${rating.losses}")
            )
          }
        )
      ),
      div(
        cls := "start-tournament",
        h3("Start today's tournament"),
        label(
          "Tournament name",
          input(
            typ := "text",
            placeholder := "Spring Open",
            controlled(
              value <-- tournamentName.signal,
              onInput.mapToValue --> tournamentName.writer
            )
          )
        ),
        button(
          cls := "primary",
          disabled <-- busy,
          onClick.mapTo(()) --> Observer[Unit] { _ =>
            val name = tournamentName.now().trim
            if (name.nonEmpty) {
              onCreate.onNext(name)
            }
          },
          "Create tournament"
        )
      )
    )
  }
}
