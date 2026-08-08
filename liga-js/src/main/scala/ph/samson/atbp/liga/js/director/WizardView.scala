package ph.samson.atbp.liga.js.director

import com.raquo.laminar.api.L.*
import ph.samson.atbp.liga.bracket.BracketPreview
import ph.samson.atbp.liga.bracket.RaceToScopes
import ph.samson.atbp.liga.bracket.RaceToWizard
import ph.samson.atbp.liga.bracket.TournamentBounds
import ph.samson.atbp.liga.js.api.Models.*
import ph.samson.atbp.liga.model.Player as CommonPlayer
import ph.samson.atbp.liga.model.PlayerRating as CommonPlayerRating
import ph.samson.atbp.liga.roster.RosterEntry
import ph.samson.atbp.liga.roster.RosterPaste

/** Define → Lock → Race-to → Seed wizard steps. */
object WizardView {

  def apply(
      tournament: TournamentResponse,
      leaderboard: Signal[Option[LeaderboardResponse]],
      busy: Signal[Boolean],
      onSetPlayers: Observer[List[Player]],
      onSaveAndLock: Observer[List[Player]],
      onSetRaceTo: Observer[RaceToRequest],
      onSeed: Observer[Unit]
  ): Div = {
    val phase = TournamentPhase.fromApi(tournament.phase)
    phase match {
      case TournamentPhase.Defining =>
        definingStep(tournament, leaderboard, busy, onSetPlayers, onSaveAndLock)
      case TournamentPhase.Locked =>
        raceToStep(tournament, busy, onSetRaceTo)
      case TournamentPhase.RaceTo =>
        seedStep(tournament, busy, onSeed)
      case other =>
        div(p(s"Unexpected wizard phase: $other"))
    }
  }

  private def periodRatings(
      leaderboard: LeaderboardResponse
  ): Map[String, CommonPlayerRating] =
    leaderboard.ratings
      .map(r =>
        r.player.name -> CommonPlayerRating(
          player = CommonPlayer(r.player.name),
          rating = r.rating,
          rd = r.rd,
          wins = r.wins,
          losses = r.losses
        )
      )
      .toMap

  private def definingStep(
      tournament: TournamentResponse,
      leaderboard: Signal[Option[LeaderboardResponse]],
      busy: Signal[Boolean],
      onSetPlayers: Observer[List[Player]],
      onSaveAndLock: Observer[List[Player]]
  ): Div =
    div(
      cls := "wizard-panel",
      child <-- leaderboard.map(_.isDefined).distinct.map {
        case false =>
          div(p("Loading leaderboard…"))
        case true =>
          definingRosterForm(
            tournament,
            leaderboard.map(_.map(periodRatings).getOrElse(Map.empty)),
            busy,
            onSetPlayers,
            onSaveAndLock
          )
      }
    )

  private def definingRosterForm(
      tournament: TournamentResponse,
      periodByName: Signal[Map[String, CommonPlayerRating]],
      busy: Signal[Boolean],
      onSetPlayers: Observer[List[Player]],
      onSaveAndLock: Observer[List[Player]]
  ): Div = {
    val initialNames = tournament.players.map(_.name)
    val names = Var(initialNames)
    val pasteText = Var(RosterPaste.formatPaste(initialNames))
    val removedNames = Var(Set.empty[String])

    def applyCleanedPaste(): List[String] = {
      val rosterNames = RosterPaste.parsePaste(pasteText.now())
      names.set(rosterNames)
      pasteText.set(RosterPaste.formatPaste(rosterNames))
      removedNames.set(Set.empty)
      rosterNames
    }

    def activeRosterNames(): List[String] =
      RosterSoftRemove.commitNames(names.now(), removedNames.now())

    val pasteDirty: Signal[Boolean] =
      pasteText.signal
        .combineWith(names.signal)
        .map { case (text, applied) =>
          RosterPaste.parsePaste(text) != applied
        }

    val activeCount: Signal[Int] =
      names.signal.combineWith(removedNames.signal).map {
        case (rosterNames, removed) =>
          RosterSoftRemove.activeNames(rosterNames, removed).size
      }

    val rosterSignal: Signal[List[RosterEntry]] =
      names.signal.combineWith(periodByName).map {
        case (rosterNames, ratings) =>
          RosterPaste.resolveRoster(rosterNames, ratings)
      }

    div(
      h2("Define roster"),
      p(tournament.name).amend(cls := "tournament-title"),
      p(
        "Paste the signup list (one name per line). " +
          "Exact matches keep period ratings; others show as guests. " +
          s"Lock when you have ${TournamentBounds.MinPlayers}–${TournamentBounds.MaxPlayers} players."
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
          disabled <-- pasteText.signal.map(_.trim.isEmpty),
          onClick.mapTo(()) --> Observer[Unit] { _ =>
            val _ = applyCleanedPaste()
          },
          "Apply paste"
        ),
        child <-- pasteDirty.map { dirty =>
          val hints = DirectorGuidance.definePasteAreaHints(dirty)
          if (hints.isEmpty) emptyNode
          else div(hints.map(text => p(cls := "hint", text)))
        }
      ),
      div(
        cls := "roster-list",
        h3("Roster"),
        children <-- rosterSignal.combineWith(removedNames.signal).map {
          case (entries, removed) =>
            if (entries.isEmpty) {
              List(p(cls := "hint", "No players yet — paste a signup list."))
            } else {
              entries.map { entry =>
                val isRemoved = removed.contains(entry.name)
                div(
                  cls := (
                    List("roster-row") ++
                      (if (entry.guest) List("guest") else Nil) ++
                      (if (isRemoved) List("removed") else Nil)
                  ).mkString(" "),
                  span(cls := "roster-name", entry.name),
                  span(cls := "roster-rating", f"${entry.rating}%.0f"),
                  span(
                    cls := "guest-badge",
                    RosterSoftRemove.guestBadgeText(entry.guest)
                  ),
                  button(
                    cls := "roster-remove",
                    onClick.mapTo(()) --> Observer[Unit] { _ =>
                      removedNames.update(
                        RosterSoftRemove.toggle(_, entry.name)
                      )
                    },
                    if (isRemoved) "Restore" else "Remove"
                  )
                )
              }
            }
        }
      ),
      div(
        cls := "roster-summary",
        child.text <-- activeCount.map(n => s"$n players in roster"),
        child <-- activeCount.combineWith(pasteDirty).map {
          case (count, dirty) =>
            val hints = DirectorGuidance.defineSummaryHints(count, dirty)
            if (hints.isEmpty) emptyNode
            else div(hints.map(text => p(cls := "hint", text)))
        }
      ),
      div(
        cls := "wizard-actions",
        button(
          cls := "primary",
          disabled <-- busy,
          onClick.mapTo(()) --> Observer[Unit] { _ =>
            onSetPlayers.onNext(activeRosterNames().map(Player(_)))
          },
          "Save roster"
        ),
        button(
          disabled <-- busy.combineWith(activeCount).map {
            case (isBusy, count) =>
              isBusy || !TournamentBounds.validPlayerCount(count)
          },
          onClick.mapTo(()) --> Observer[Unit] { _ =>
            onSaveAndLock.onNext(activeRosterNames().map(Player(_)))
          },
          "Lock roster"
        )
      )
    )
  }

  private def raceToStep(
      tournament: TournamentResponse,
      busy: Signal[Boolean],
      onSetRaceTo: Observer[RaceToRequest]
  ): Div = {
    val playerCount = tournament.players.size
    val legalTopNs = RaceToWizardStep.legalTopNOptions(playerCount)
    val wizardState = Var(RaceToWizardStep.initialState(tournament))

    def scopesFor(topN: Int, section: RaceToScopes.Section): List[String] =
      RaceToWizardStep
        .requiredScopes(playerCount, topN)
        .filter(scope => RaceToScopes.scopeLabel(scope).section == section)

    def renderPreview(topN: Int): Div = {
      val preview = RaceToWizardStep.preview(playerCount, topN)
      div(
        cls := "bracket-preview",
        h3("Bracket preview"),
        p(
          cls := "hint",
          s"${preview.playerCount} players · ${preview.bracketSize}-slot bracket"
        ),
        preview.sections.map(renderPreviewSection)
      )
    }

    def renderPreviewSection(
        sectionPreview: BracketPreview.SectionPreview
    ): Div =
      div(
        cls := "preview-section",
        h4(sectionPreview.section.label),
        ul(
          cls := "preview-rounds",
          sectionPreview.rounds.map { round =>
            li(RaceToWizardStep.formatRoundSummary(round))
          }
        ),
        RaceToWizardStep.resetGrandFinalHint(sectionPreview) match {
          case Some(hint) => p(cls := "hint", hint)
          case None       => emptyNode
        }
      )

    def renderSection(topN: Int, section: RaceToScopes.Section): Node = {
      val scopes = scopesFor(topN, section)
      if (scopes.isEmpty) {
        emptyNode
      } else {
        div(
          cls := "race-to-section",
          h3(section.label),
          ul(
            cls := "race-to-inputs",
            scopes.map { scope =>
              val scopeLabel = RaceToScopes.scopeLabel(scope)
              li(
                label(
                  scopeLabel.roundLabel,
                  input(
                    typ := "number",
                    controlled(
                      value <-- wizardState.signal.map(
                        _.raceToByScope.getOrElse(scope, 7).toString
                      ),
                      onInput.mapToValue --> Observer[String] { raw =>
                        raw.toIntOption.foreach { value =>
                          wizardState.update { state =>
                            RaceToWizard.applyEdit(
                              state,
                              scope,
                              value,
                              playerCount
                            )
                          }
                        }
                      }
                    )
                  ),
                  if (scope == "gf") {
                    p(
                      cls := "hint",
                      "usually longer than finals — set explicitly."
                    )
                  } else {
                    emptyNode
                  }
                )
              )
            }
          )
        )
      }
    }

    div(
      cls := "wizard-panel",
      h2("Race-to by bracket section"),
      div(
        cls := "top-n-control",
        label(
          "Double elimination until Top N",
          select(
            value <-- wizardState.signal.map(_.topN.toString),
            onChange.mapToValue --> Observer[String] { raw =>
              raw.toIntOption.foreach { newTopN =>
                wizardState.update { state =>
                  RaceToWizardStep.changeTopN(state, newTopN, playerCount)
                }
              }
            },
            legalTopNs.map { n =>
              option(value := n.toString, s"Top $n")
            }
          )
        )
      ),
      child <-- wizardState.signal.map(state => renderPreview(state.topN)),
      p(
        "Set race-to for each bracket section. " +
          "Editing a round cascades through later rounds in that section."
      ),
      children <-- wizardState.signal.map { state =>
        RaceToWizardStep
          .visibleSections(playerCount, state.topN)
          .map(section => renderSection(state.topN, section))
      },
      button(
        cls := "primary",
        disabled <-- busy,
        onClick.mapTo(()) --> Observer[Unit](_ => {
          onSetRaceTo.onNext(RaceToWizardStep.saveRequest(wizardState.now()))
        }),
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
        s"${tournament.players.size} players locked. Race-to configured for all bracket sections."
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
