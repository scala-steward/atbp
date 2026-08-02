package ph.samson.atbp.liga.js.director

import com.raquo.laminar.api.L.*
import ph.samson.atbp.liga.js.api.Models.*
import ph.samson.atbp.liga.model as shared
import ph.samson.atbp.liga.model.MatchSide

/** Match control panel: ready, handicap, start, result, and forfeit. */
object MatchPanel {

  def apply(
      tournament: TournamentResponse,
      matchDef: BracketMatch,
      busy: Signal[Boolean],
      onReady: Observer[Unit],
      onApplyHandicap: Observer[Int],
      onStart: Observer[Unit],
      onResult: Observer[(Int, Int)],
      onForfeit: Observer[(String, String)]
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
    val appliedHandicapDisplay = AppliedHandicapLabels.forMatch(
      BracketHandicapContext.fromTournament(tournament),
      matchDef
    )
    val scoreFloors =
      ScoreEntryDefaults.floorsFromDisplay(appliedHandicapDisplay)
    val (initialScoreA, initialScoreB) =
      ScoreEntryDefaults.initialScoreStrings(
        appliedHandicapDisplay,
        matchDef.result
      )
    val scoreAInput = Var(initialScoreA)
    val scoreBInput = Var(initialScoreB)
    val forfeitingSide = Var("A")
    val forfeitReason = Var("")
    val validationError = Var("")
    val forfeitValidationError = Var("")

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
        scoreFloors,
        forfeitingSide,
        forfeitReason,
        validationError,
        forfeitValidationError,
        busy,
        onReady,
        onApplyHandicap,
        onStart,
        onResult,
        onForfeit
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
      scoreFloors: ScoreEntryDefaults.ScoreFloors,
      forfeitingSide: Var[String],
      forfeitReason: Var[String],
      validationError: Var[String],
      forfeitValidationError: Var[String],
      busy: Signal[Boolean],
      onReady: Observer[Unit],
      onApplyHandicap: Observer[Int],
      onStart: Observer[Unit],
      onResult: Observer[(Int, Int)],
      onForfeit: Observer[(String, String)]
  ): Div = {
    def withForfeit(body: Node): Div =
      div(
        body,
        forfeitControls(
          matchDef,
          forfeitingSide,
          forfeitReason,
          forfeitValidationError,
          busy,
          onForfeit
        )
      )

    matchDef.state match {
      case BracketMatchState.Pending =>
        div(p("Waiting for players."))

      case BracketMatchState.Ready =>
        withForfeit(
          resolvedRaceTo match {
            case None =>
              missingRaceToControls(matchDef)
            case Some(raceTo) =>
              readyControls(
                matchDef,
                tournament,
                raceTo,
                handicapInput,
                busy,
                onReady,
                onApplyHandicap,
                onStart
              )
          }
        )

      case BracketMatchState.Started =>
        withForfeit(
          resolvedRaceTo match {
            case None =>
              missingRaceToControls(matchDef)
            case Some(raceTo) =>
              startedControls(
                tournament,
                matchDef,
                raceTo,
                scoreAInput,
                scoreBInput,
                scoreFloors,
                validationError,
                busy,
                onResult
              )
          }
        )

      case BracketMatchState.Completed =>
        completedSummary(tournament, matchDef)
    }
  }

  private def completedSummary(
      tournament: TournamentResponse,
      matchDef: BracketMatch
  ): Div = {
    val handicapLine =
      appliedHandicapStatusLine(tournament, matchDef, completed = true)
    (matchDef.isBye, matchDef.forfeit, matchDef.result) match {
      case (true, _, _) =>
        div(
          p("Bye — auto-advance"),
          handicapLine
        )
      case (_, Some(info), _) =>
        div(
          p(s"Forfeit: ${info.reason}"),
          p(
            s"Forfeiting side: ${forfeitSideLabel(matchDef, info.forfeitingSide)}"
          ),
          handicapLine
        )
      case (_, None, Some(result)) =>
        div(
          p(s"Final score: ${result.scoreA}–${result.scoreB}"),
          handicapLine
        )
      case _ =>
        div(p("Match completed."), handicapLine)
    }
  }

  private def readyControls(
      matchDef: BracketMatch,
      tournament: TournamentResponse,
      raceTo: Int,
      handicapInput: Var[String],
      busy: Signal[Boolean],
      onReady: Observer[Unit],
      onApplyHandicap: Observer[Int],
      onStart: Observer[Unit]
  ): Div = {
    val preview = MatchHandicapPreview.fromMatch(tournament, matchDef, raceTo)
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
  }

  private def clampScoreOnBlur(
      scoreInput: Var[String],
      floor: Int
  ): Observer[Unit] =
    Observer[Unit] { _ =>
      scoreInput.set(
        ScoreEntryDefaults.clampOnBlur(scoreInput.now(), floor)
      )
    }

  private def startedControls(
      tournament: TournamentResponse,
      matchDef: BracketMatch,
      raceTo: Int,
      scoreAInput: Var[String],
      scoreBInput: Var[String],
      scoreFloors: ScoreEntryDefaults.ScoreFloors,
      validationError: Var[String],
      busy: Signal[Boolean],
      onResult: Observer[(Int, Int)]
  ): Div =
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
            onInput.mapToValue --> scoreAInput,
            onBlur.mapTo(()) --> clampScoreOnBlur(
              scoreAInput,
              scoreFloors.scoreA
            )
          )
        ),
        label(
          s"${BracketLayout.playerLabel(matchDef.playerB)}: ",
          input(
            typ := "number",
            value <-- scoreBInput,
            onInput.mapToValue --> scoreBInput,
            onBlur.mapTo(()) --> clampScoreOnBlur(
              scoreBInput,
              scoreFloors.scoreB
            )
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
          val (clampedA, clampedB) = ScoreEntryDefaults.clampPair(
            scoreAInput.now(),
            scoreBInput.now(),
            scoreFloors
          )
          scoreAInput.set(clampedA)
          scoreBInput.set(clampedB)
          val scoreA = clampedA.toIntOption
          val scoreB = clampedB.toIntOption
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

  private def forfeitSideLabel(
      matchDef: BracketMatch,
      side: String
  ): String =
    MatchSide.parse(side) match {
      case Some(MatchSide.A) => BracketLayout.playerLabel(matchDef.playerA)
      case Some(MatchSide.B) => BracketLayout.playerLabel(matchDef.playerB)
      case None              => side
    }

  private def forfeitControls(
      matchDef: BracketMatch,
      forfeitingSide: Var[String],
      forfeitReason: Var[String],
      forfeitValidationError: Var[String],
      busy: Signal[Boolean],
      onForfeit: Observer[(String, String)]
  ): Div = {
    val forfeitOpen = Var(false)
    div(
      cls := "forfeit-entry",
      child <-- forfeitOpen.signal.map {
        case false =>
          button(
            cls := "forfeit-open",
            disabled <-- busy,
            onClick.mapTo(true) --> forfeitOpen,
            "Record forfeit"
          )
        case true =>
          div(
            cls := "forfeit-form",
            h3("Forfeit"),
            p(
              cls := "hint",
              "Completes the match without scores. The non-forfeiting player advances."
            ),
            label(
              "Who forfeits: ",
              select(
                value <-- forfeitingSide,
                onChange.mapToValue --> forfeitingSide,
                option(
                  value := "A",
                  BracketLayout.playerLabel(matchDef.playerA)
                ),
                option(
                  value := "B",
                  BracketLayout.playerLabel(matchDef.playerB)
                )
              )
            ),
            label(
              "Reason (required): ",
              input(
                typ := "text",
                placeholder := "e.g. no-show",
                value <-- forfeitReason,
                onInput.mapToValue --> forfeitReason
              )
            ),
            child <-- forfeitValidationError.signal.map { msg =>
              if (msg.nonEmpty) div(cls := "validation-error", msg)
              else emptyNode
            },
            div(
              cls := "actions",
              button(
                disabled <-- busy.combineWith(forfeitReason.signal).map {
                  case (isBusy, reason) => isBusy || reason.trim.isEmpty
                },
                onClick.mapTo(()) --> Observer[Unit] { _ =>
                  ForfeitSubmitPolicy.validate(
                    forfeitingSide.now(),
                    forfeitReason.now()
                  ) match {
                    case ForfeitSubmitPolicy.Outcome.BlankReason =>
                      forfeitValidationError.set(
                        ForfeitSubmitPolicy.blankReasonMessage
                      )
                    case ForfeitSubmitPolicy.Outcome.Ready(submit) =>
                      forfeitValidationError.set("")
                      onForfeit.onNext((submit.side, submit.reason))
                  }
                },
                "Submit forfeit"
              ),
              button(
                disabled <-- busy,
                onClick.mapTo(false) --> forfeitOpen,
                "Cancel"
              )
            )
          )
      }
    )
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
