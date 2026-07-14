package ph.samson.atbp.liga.js.director

import com.raquo.laminar.api.L.*
import ph.samson.atbp.liga.js.api.Models.*
import ph.samson.atbp.liga.roster.RosterEntry
import ph.samson.atbp.liga.roster.RosterPaste

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
    val names = Var(tournament.players.map(_.name))
    val pasteText = Var("")

    val periodByName: Map[String, Double] =
      leaderboard.ratings.map(r => r.player.name -> r.rating).toMap

    val rosterSignal: Signal[List[RosterEntry]] =
      names.signal.map(RosterPaste.resolveRoster(_, periodByName))

    div(
      cls := "wizard-panel",
      h2("Define roster"),
      p(tournament.name).amend(cls := "tournament-title"),
      p(
        "Paste the signup list (one name per line). " +
          "Exact matches keep period ratings; others show as guests. " +
          "Lock when you have 8–64 players."
      ),
      div(
        cls := "roster-paste",
        label(
          "Signup names",
          textArea(
            rows := 8,
            placeholder := "Alice\nBob\nCarol",
            controlled(
              value <-- pasteText.signal,
              onInput.mapToValue --> pasteText.writer
            )
          )
        ),
        button(
          onClick.mapTo(()) --> Observer[Unit] { _ =>
            names.set(RosterPaste.parsePaste(pasteText.now()))
          },
          "Apply paste"
        )
      ),
      div(
        cls := "roster-list",
        h3("Roster"),
        children <-- rosterSignal.map { entries =>
          if (entries.isEmpty) {
            List(p(cls := "hint", "No players yet — paste a signup list."))
          } else {
            entries.map { entry =>
              div(
                cls := (if (entry.guest) "roster-row guest" else "roster-row"),
                span(cls := "roster-name", entry.name),
                span(cls := "roster-rating", f"${entry.rating}%.0f"),
                if (entry.guest) span(cls := "guest-badge", "guest")
                else emptyNode
              )
            }
          }
        }
      ),
      div(
        cls := "roster-summary",
        child.text <-- names.signal.map(ns => s"${ns.size} players selected"),
        child <-- names.signal.map { ns =>
          val hint = DirectorGuidance.lockRosterHint(ns.size)
          if (hint.nonEmpty) p(cls := "hint", hint) else emptyNode
        }
      ),
      div(
        cls := "wizard-actions",
        button(
          cls := "primary",
          disabled <-- busy,
          onClick.mapTo(()) --> Observer[Unit] { _ =>
            onSetPlayers.onNext(names.now().map(Player(_)))
          },
          "Save roster"
        ),
        button(
          disabled <-- busy.combineWith(names.signal).map { case (isBusy, ns) =>
            isBusy || ns.size < 8 || ns.size > 64
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
