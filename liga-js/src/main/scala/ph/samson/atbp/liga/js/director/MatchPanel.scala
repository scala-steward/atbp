package ph.samson.atbp.liga.js.director

import com.raquo.laminar.api.L.*
import ph.samson.atbp.liga.handicap.Handicap
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
            previewHandicap(tournament, matchDef, rt).map(_.handicap.toString)
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

      case (BracketMatchState.Ready, Some(raceTo))
          if matchDef.handicapSuggested.isEmpty =>
        previewHandicap(tournament, matchDef, raceTo) match {
          case Some(preview) =>
            div(
              p(
                DirectorGuidance.handicapSpotLabel(
                  preview.weakerPlayer.name,
                  raceTo,
                  DirectorGuidance.handicapCap(raceTo)
                )
              ),
              p(s"Preview spot: ${preview.handicap}"),
              button(
                cls := "primary",
                disabled <-- busy,
                onClick.mapTo(()) --> onReady,
                "Ready match"
              )
            )
          case None =>
            div(p("Waiting for player ratings to compute a preview."))
        }

      case (BracketMatchState.Ready, Some(raceTo)) =>
        val cap = DirectorGuidance.handicapCap(raceTo)
        val weakerName = weakerPlayerName(tournament, matchDef, raceTo)
        div(
          weakerName
            .map(name =>
              p(DirectorGuidance.handicapSpotLabel(name, raceTo, cap))
            )
            .getOrElse(emptyNode),
          label(
            "Handicap (games spotted to weaker player): ",
            input(
              typ := "number",
              value <-- handicapInput,
              onInput.mapToValue --> handicapInput
            )
          ),
          matchDef.handicapSuggested.map { suggested =>
            p(s"Server suggested: $suggested")
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

      case (BracketMatchState.Started, Some(raceTo)) =>
        div(
          matchDef.handicapApplied
            .flatMap { h =>
              weakerPlayerName(tournament, matchDef, raceTo).map { name =>
                p(s"Handicap applied: $h spot to $name")
              }
            }
            .getOrElse(emptyNode),
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
        if (matchDef.isBye) {
          div(
            p("Bye — auto-advance"),
            matchDef.handicapApplied.map(h => p(s"Handicap was $h"))
          )
        } else {
          matchDef.result match {
            case Some(result) =>
              div(
                p(s"Final score: ${result.scoreA}–${result.scoreB}"),
                matchDef.handicapApplied.map(h => p(s"Handicap was $h"))
              )
            case None =>
              div(p("Match completed."))
          }
        }
    }

  private def weakerPlayerName(
      tournament: TournamentResponse,
      matchDef: BracketMatch,
      raceTo: Int
  ): Option[String] =
    previewHandicap(tournament, matchDef, raceTo)
      .map(_.weakerPlayer.name)
      .filter(_.nonEmpty)

  private def previewHandicap(
      tournament: TournamentResponse,
      matchDef: BracketMatch,
      raceTo: Int
  ): Option[HandicapSuggestion] = {
    val ratingA = matchDef.playerA.flatMap(p =>
      tournament.frozenRatings.find(_.player.name == p.name)
    )
    val ratingB = matchDef.playerB.flatMap(p =>
      tournament.frozenRatings.find(_.player.name == p.name)
    )
    for {
      a <- ratingA
      b <- ratingB
    } yield toJsSuggestion(
      Handicap.suggest(toSharedRating(a), toSharedRating(b), raceTo)
    )
  }

  private def toSharedRating(rating: PlayerRating): shared.PlayerRating =
    shared.PlayerRating(
      shared.Player(rating.player.name),
      rating.rating,
      rating.rd,
      rating.wins,
      rating.losses
    )

  private def toJsSuggestion(
      suggestion: shared.HandicapSuggestion
  ): HandicapSuggestion =
    HandicapSuggestion(
      Player(suggestion.weakerPlayer.name),
      suggestion.handicap,
      suggestion.raceTo
    )
}
