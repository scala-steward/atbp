package ph.samson.atbp.liga.js.director

import com.raquo.laminar.api.L.*
import ph.samson.atbp.liga.js.api.Models.*
import ph.samson.atbp.liga.js.glicko.HandicapPreview

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
    val handicapInput = Var(
      matchDef.handicapApplied
        .orElse(matchDef.handicapSuggested)
        .map(_.toString)
        .getOrElse(previewHandicap(tournament, matchDef).toString)
    )
    val scoreAInput = Var(
      matchDef.result.map(_.scoreA.toString).getOrElse("0")
    )
    val scoreBInput = Var(
      matchDef.result.map(_.scoreB.toString).getOrElse("0")
    )

    div(
      cls := "match-panel",
      h2(matchDef.id),
      p(
        s"${BracketLayout.playerLabel(matchDef.playerA)} vs ${BracketLayout.playerLabel(matchDef.playerB)}"
      ),
      p(
        cls := "match-state",
        s"State: ${BracketLayout.stateLabel(matchDef.state)}"
      ),
      matchDef.raceTo.map(rt => p(s"Race to $rt")),
      matchControls(
        matchDef,
        tournament,
        handicapInput,
        scoreAInput,
        scoreBInput,
        busy,
        onReady,
        onApplyHandicap,
        onStart,
        onResult
      )
    )
  }

  private def matchControls(
      matchDef: BracketMatch,
      tournament: TournamentResponse,
      handicapInput: Var[String],
      scoreAInput: Var[String],
      scoreBInput: Var[String],
      busy: Signal[Boolean],
      onReady: Observer[Unit],
      onApplyHandicap: Observer[Int],
      onStart: Observer[Unit],
      onResult: Observer[(Int, Int)]
  ): Div =
    matchDef.state match {
      case BracketMatchState.Pending =>
        div(p("Waiting for players."))

      case BracketMatchState.Ready if matchDef.handicapSuggested.isEmpty =>
        val preview = previewHandicap(tournament, matchDef)
        div(
          p(s"Client preview spot: $preview"),
          button(
            cls := "primary",
            disabled <-- busy,
            onClick.mapTo(()) --> onReady,
            "Ready match"
          )
        )

      case BracketMatchState.Ready =>
        div(
          label(
            "Handicap (weaker player spot): ",
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
              disabled <-- busy,
              onClick.mapTo(()) --> onStart,
              "Start match"
            )
          )
        )

      case BracketMatchState.Started =>
        div(
          matchDef.handicapApplied.map { h =>
            p(s"Handicap applied: $h")
          },
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
          button(
            cls := "primary",
            disabled <-- busy,
            onClick --> onResult.contramap { _ =>
              val scoreA = scoreAInput.now().toIntOption.getOrElse(0)
              val scoreB = scoreBInput.now().toIntOption.getOrElse(0)
              (scoreA, scoreB)
            },
            "Record result"
          )
        )

      case BracketMatchState.Completed =>
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

  private def previewHandicap(
      tournament: TournamentResponse,
      matchDef: BracketMatch
  ): Int = {
    val raceTo = matchDef.raceTo.orElse(
      tournament.bracket.flatMap { bracket =>
        BracketLayout.defaultRaceTo(
          matchDef.id,
          bracket.size,
          tournament.roundRaceTo
        )
      }
    )
    val ratingA = matchDef.playerA.flatMap(p =>
      tournament.frozenRatings.find(_.player.name == p.name)
    )
    val ratingB = matchDef.playerB.flatMap(p =>
      tournament.frozenRatings.find(_.player.name == p.name)
    )
    (for {
      a <- ratingA
      b <- ratingB
      rt <- raceTo
    } yield HandicapPreview.suggest(a, b, rt).handicap).getOrElse(0)
  }
}
