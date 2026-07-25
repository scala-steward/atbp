package ph.samson.atbp.liga.js.director

import com.raquo.laminar.api.L.*
import ph.samson.atbp.liga.js.LatestRatingsView
import ph.samson.atbp.liga.js.api.Models.*

/** Idle director panel: latest ratings plus create-tournament controls. */
object IdleDirectorView {

  def apply(
      latestRatings: LatestRatingsResponse,
      busy: Signal[Boolean],
      onCreate: Observer[String]
  ): Div = {
    val tournamentName = Var("")

    div(
      cls := "leaderboard-panel",
      LatestRatingsView(latestRatings),
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
