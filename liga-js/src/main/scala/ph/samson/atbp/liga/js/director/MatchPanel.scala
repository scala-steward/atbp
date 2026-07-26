package ph.samson.atbp.liga.js.director

import com.raquo.laminar.api.L.*
import ph.samson.atbp.liga.js.api.Models.*
import ph.samson.atbp.liga.model as shared

/** Match control panel: ready, handicap, start, and result entry. */
object MatchPanel {

  def apply(
      tournament: TournamentResponse,
      matchDef: BracketMatch,
      busy: Signal[Boolean],
      onReady: Observer[Unit],
      onApplyHandicap: Observer[Int],
      onStart: Observer[Unit],
      onResult: Observer[(Int, Int)]
  ): Div = {
    val resolvedRaceTo =
      BracketLayout.resolveMatchRaceTo(matchDef, tournament.raceToByScope)
    val handicapInput = Var(
      matchDef.handicapApplied
        .orElse(matchDef.handicapSuggested)
        .map(_.toString)
        .orElse {
          resolvedRaceTo.flatMap(rt =>
            MatchHandicapPreview
              .fromMatch(tournament, matchDef, rt)
              .map(_.suggestedHandicap.toString)
          )
        }
        .getOrElse("0")
    )
    val scoreAInput = Var(
      matchDef.result.map(_.scoreA.toString).getOrElse("0")
    )
    val scoreBInput = Var(
      matchDef.result.map(_.scoreB.toString).getOrElse("0")
    )
    val validationError = Var("")

    div(
      cls := "match-panel",
      h2(
        BracketLayout.matchLabel(
          matchDef.id,
          tournament.bracket.map(_.size).getOrElse(8)
        )
      ),
      p(cls := "match-id", matchDef.id),
      p(
        s"${BracketLayout.playerLabel(matchDef.playerA)} vs ${BracketLayout.playerLabel(matchDef.playerB)}"
      ),
      p(
        cls := "match-state",
        s"State: ${BracketLayout.stateLabel(matchDef.state)}"
      ),
      raceToDisplay(matchDef, tournament.raceToByScope),
      p(
        cls := "guidance",
        DirectorGuidance.matchStepHint(matchDef, resolvedRaceTo)
      ),
      matchControls(
        matchDef,
        tournament,
        resolvedRaceTo,
        handicapInput,
        scoreAInput,
        scoreBInput,
        validationError,
        busy,
        onReady,
        onApplyHandicap,
        onStart,
        onResult
      )
    )
  }

  private def raceToDisplay(
      matchDef: BracketMatch,
      raceToByScope: Map[String, Int]
  ): Node =
    RaceToLabels.matchRaceToLabel(matchDef, raceToByScope) match {
      case Right(label) => p(cls := "race-to", label)
      case Left(hint)   => p(cls := "race-to-error", hint)
    }

  /** Started/Completed applied-handicap line; loud when weaker cannot resolve.
    */
  private def appliedHandicapStatusLine(
      tournament: TournamentResponse,
      matchDef: BracketMatch,
      completed: Boolean
  ): Node = {
    val display = AppliedHandicapLabels.forMatch(
      BracketHandicapContext.fromTournament(tournament),
      matchDef
    )
    AppliedHandicapLabels.panelStatusMessage(display, completed) match {
      case None       => emptyNode
      case Some(text) =>
        if (AppliedHandicapLabels.panelStatusIsError(display)) {
          p(cls := "race-to-error", text)
        } else {
          p(text)
        }
    }
  }

  private def missingRaceToControls(matchDef: BracketMatch): Div =
    div(
      p(
        cls := "hint race-to-error",
        DirectorGuidance.missingRaceToHint(matchDef.id)
      )
    )

  private def matchControls(
      matchDef: BracketMatch,
      tournament: TournamentResponse,
      resolvedRaceTo: Option[Int],
      handicapInput: Var[String],
      scoreAInput: Var[String],
      scoreBInput: Var[String],
      validationError: Var[String],
      busy: Signal[Boolean],
      onReady: Observer[Unit],
      onApplyHandicap: Observer[Int],
      onStart: Observer[Unit],
      onResult: Observer[(Int, Int)]
  ): Div =
    (matchDef.state, resolvedRaceTo) match {
      case (BracketMatchState.Pending, _) =>
        div(p("Waiting for players."))

      case (BracketMatchState.Ready | BracketMatchState.Started, None) =>
        missingRaceToControls(matchDef)

      case (BracketMatchState.Ready, Some(raceTo)) =>
        val preview =
          MatchHandicapPreview.fromMatch(tournament, matchDef, raceTo)
        ReadyHandicapPolicy.surface(matchDef, preview) match {
          case ReadyHandicapPolicy.Surface.Preview(handicapPreview) =>
            div(
              probabilityNeighborhood(
                handicapPreview.weaker,
                handicapPreview.stronger,
                raceTo,
                handicapPreview.suggestedHandicap
              ),
              button(
                cls := "primary",
                disabled <-- busy,
                onClick.mapTo(()) --> onReady,
                "Ready match"
              )
            )
          case ReadyHandicapPolicy.Surface.PreviewWaiting =>
            div(p(ReadyHandicapPolicy.previewWaitingMessage))
          case ReadyHandicapPolicy.Surface.Adjust(suggested, maybePreview) =>
            div(
              maybePreview match {
                case Some(handicapPreview) =>
                  probabilityNeighborhood(
                    handicapPreview.weaker,
                    handicapPreview.stronger,
                    raceTo,
                    suggested
                  )
                case None =>
                  p(ReadyHandicapPolicy.previewWaitingMessage)
              },
              div(
                label(
                  "Handicap (games spotted to weaker player): ",
                  input(
                    typ := "number",
                    value <-- handicapInput,
                    onInput.mapToValue --> handicapInput
                  )
                ),
                maybePreview match {
                  case Some(handicapPreview) =>
                    child <-- handicapInput.signal.map { input =>
                      HandicapProbabilityHints.typedSpotHint(
                        handicapPreview.weaker,
                        handicapPreview.stronger,
                        raceTo,
                        suggested,
                        input
                      ) match {
                        case Some(hint) =>
                          p(cls := "hint typed-spot-hint", hint)
                        case None => emptyNode
                      }
                    }
                  case None => emptyNode
                },
                div(
                  cls := "actions",
                  button(
                    disabled <-- busy,
                    onClick --> onApplyHandicap.contramap { _ =>
                      handicapInput.now().toIntOption.getOrElse(0)
                    },
                    "Apply handicap"
                  ),
                  button(
                    cls := "primary",
                    disabled <-- busy.combineWith(handicapInput.signal).map {
                      case (isBusy, _) =>
                        isBusy || matchDef.handicapApplied.isEmpty
                    },
                    onClick.mapTo(()) --> onStart,
                    "Start match"
                  )
                ),
                Option.when(matchDef.handicapApplied.isEmpty)(
                  p(cls := "hint", "Apply handicap before starting.")
                )
              )
            )
        }

      case (BracketMatchState.Started, Some(raceTo)) =>
        div(
          appliedHandicapStatusLine(tournament, matchDef, completed = false),
          p(
            cls := "guidance",
            DirectorGuidance.scoreboardScoreHint(raceTo)
          ),
          div(
            cls := "score-entry",
            label(
              s"${BracketLayout.playerLabel(matchDef.playerA)}: ",
              input(
                typ := "number",
                value <-- scoreAInput,
                onInput.mapToValue --> scoreAInput
              )
            ),
            label(
              s"${BracketLayout.playerLabel(matchDef.playerB)}: ",
              input(
                typ := "number",
                value <-- scoreBInput,
                onInput.mapToValue --> scoreBInput
              )
            )
          ),
          child <-- validationError.signal.map { msg =>
            if (msg.nonEmpty) div(cls := "validation-error", msg)
            else emptyNode
          },
          button(
            cls := "primary",
            disabled <-- busy,
            onClick.mapTo(()) --> Observer[Unit] { _ =>
              val scoreA = scoreAInput.now().toIntOption
              val scoreB = scoreBInput.now().toIntOption
              (scoreA, scoreB) match {
                case (Some(a), Some(b)) if a == b =>
                  validationError.set(
                    "Scores cannot tie — one player must win."
                  )
                case (Some(a), Some(b)) if a < 0 || b < 0 =>
                  validationError.set("Scores must be zero or greater.")
                case (Some(a), Some(b)) =>
                  validationError.set("")
                  onResult.onNext((a, b))
                case _ =>
                  validationError.set(
                    "Enter valid scores for both players."
                  )
              }
            },
            "Record result"
          )
        )

      case (BracketMatchState.Completed, _) =>
        val handicapLine =
          appliedHandicapStatusLine(tournament, matchDef, completed = true)
        if (matchDef.isBye) {
          div(
            p("Bye — auto-advance"),
            handicapLine
          )
        } else {
          matchDef.result match {
            case Some(result) =>
              div(
                p(s"Final score: ${result.scoreA}–${result.scoreB}"),
                handicapLine
              )
            case None =>
              div(p("Match completed."), handicapLine)
          }
        }
    }

  private def probabilityNeighborhood(
      weaker: shared.PlayerRating,
      stronger: shared.PlayerRating,
      raceTo: Int,
      suggested: Int
  ): Div = {
    val rows =
      HandicapProbabilityHints.neighborhoodRows(
        weaker,
        stronger,
        raceTo,
        suggested
      )
    div(
      cls := "handicap-probability",
      p(
        cls := "handicap-probability-header",
        HandicapProbabilityHints.headerLabel(weaker.player.name, raceTo)
      ),
      div(
        cls := "handicap-probability-rows",
        rows.map { row =>
          val rowClass = List(
            Some("handicap-probability-row"),
            Option.when(row.spot == suggested)("suggested"),
            Option.when(HandicapProbabilityHints.isOverCap(row.spot, raceTo))(
              "over-cap"
            )
          ).flatten.mkString(" ")
          div(
            cls := rowClass,
            span(
              cls := "handicap-spot",
              HandicapProbabilityHints.formatSpot(row.spot)
            ),
            span(cls := "handicap-percent", row.weakerPercent)
          )
        }
      )
    )
  }

}
