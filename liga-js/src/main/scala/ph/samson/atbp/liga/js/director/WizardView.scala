package ph.samson.atbp.liga.js.director

import com.raquo.laminar.api.L.*
import ph.samson.atbp.liga.js.api.Models.*

/** Define → Lock → Race-to → Seed wizard steps. */
object WizardView {

  def apply(
      tournament: TournamentResponse,
      leaderboard: LeaderboardResponse,
      busy: Signal[Boolean],
      onSetPlayers: Observer[List[Player]],
      onLock: Observer[Unit],
      onSetRaceTo: Observer[Map[Int, Int]],
      onSeed: Observer[Unit]
  ): Div = {
    val phase = TournamentPhase.fromApi(tournament.phase)
    phase match {
      case TournamentPhase.Defining =>
        definingStep(tournament, leaderboard, busy, onSetPlayers, onLock)
      case TournamentPhase.Locked =>
        raceToStep(tournament, busy, onSetRaceTo)
      case TournamentPhase.RaceTo =>
        seedStep(tournament, busy, onSeed)
      case other =>
        div(p(s"Unexpected wizard phase: $other"))
    }
  }

  private def definingStep(
      tournament: TournamentResponse,
      leaderboard: LeaderboardResponse,
      busy: Signal[Boolean],
      onSetPlayers: Observer[List[Player]],
      onLock: Observer[Unit]
  ): Div = {
    val selected = Var(tournament.players.map(_.name).toSet)
    val guestName = Var("")

    def currentPlayers: List[Player] =
      selected.now().toList.sorted.map(Player(_))

    div(
      cls := "wizard-panel",
      h2("Define roster"),
      p(tournament.name).amend(cls := "tournament-title"),
      p(
        "Select period players and add guests. Lock when you have 8–64 players."
      ),
      div(
        cls := "player-picker",
        h3("Period players"),
        leaderboard.ratings.map { rating =>
          label(
            input(
              typ := "checkbox",
              checked <-- selected.signal.map(_.contains(rating.player.name)),
              onClick.mapTo(rating.player.name) --> Observer[String] { name =>
                selected.update { current =>
                  if (current.contains(name)) {
                    current - name
                  } else {
                    current + name
                  }
                }
              }
            ),
            span(rating.player.name)
          )
        }
      ),
      div(
        cls := "guest-entry",
        label(
          "Add guest",
          input(
            typ := "text",
            controlled(
              value <-- guestName.signal,
              onInput.mapToValue --> guestName.writer
            )
          )
        ),
        button(
          onClick.mapTo(()) --> Observer[Unit] { _ =>
            val name = guestName.now().trim
            if (name.nonEmpty) {
              selected.update(_ + name)
              guestName.set("")
            }
          },
          "Add"
        )
      ),
      div(
        cls := "roster-summary",
        child.text <-- selected.signal.map(names =>
          s"${names.size} players selected"
        ),
        child <-- selected.signal.map { names =>
          val hint = DirectorGuidance.lockRosterHint(names.size)
          if (hint.nonEmpty) p(cls := "hint", hint) else emptyNode
        }
      ),
      div(
        cls := "wizard-actions",
        button(
          cls := "primary",
          disabled <-- busy,
          onClick.mapTo(()) --> Observer[Unit](_ =>
            onSetPlayers.onNext(currentPlayers)
          ),
          "Save roster"
        ),
        button(
          disabled <-- busy.combineWith(selected.signal).map {
            case (isBusy, names) =>
              isBusy || names.size < 8 || names.size > 64
          },
          onClick.mapTo(()) --> onLock,
          "Lock roster"
        )
      )
    )
  }

  private def raceToStep(
      tournament: TournamentResponse,
      busy: Signal[Boolean],
      onSetRaceTo: Observer[Map[Int, Int]]
  ): Div = {
    val rounds = WizardRounds.requiredKeys(tournament.players.size)
    val raceToValues = Var(rounds.map(r => r -> 7).toMap)

    div(
      cls := "wizard-panel",
      h2("Race-to by round"),
      p(
        "Set race-to for each bracket round (pre-filled to 7). " +
          "The same round number may apply to several bracket sections."
      ),
      ul(
        cls := "race-to-inputs",
        rounds.map { round =>
          li(
            label(
              BracketLayout.raceToRoundLabel(round, tournament.players.size),
              input(
                typ := "number",
                controlled(
                  value <-- raceToValues.signal.map(
                    _.getOrElse(round, 7).toString
                  ),
                  onInput.mapToValue --> Observer[String] { raw =>
                    raw.toIntOption.foreach { value =>
                      raceToValues.update(_.updated(round, value))
                    }
                  }
                )
              )
            )
          )
        }
      ),
      button(
        cls := "primary",
        disabled <-- busy,
        onClick.mapTo(()) --> Observer[Unit](_ =>
          onSetRaceTo.onNext(raceToValues.now())
        ),
        "Save race-to"
      )
    )
  }

  private def seedStep(
      tournament: TournamentResponse,
      busy: Signal[Boolean],
      onSeed: Observer[Unit]
  ): Div =
    div(
      cls := "wizard-panel",
      h2("Seed bracket"),
      p(
        s"${tournament.players.size} players locked. Race-to configured for all rounds."
      ),
      p(cls := "guidance", DirectorGuidance.seedHint),
      ul(
        tournament.players.map(player => li(player.name))
      ),
      button(
        cls := "primary",
        disabled <-- busy,
        onClick.mapTo(()) --> onSeed,
        "Seed bracket"
      )
    )
}
