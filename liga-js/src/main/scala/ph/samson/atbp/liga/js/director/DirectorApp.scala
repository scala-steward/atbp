package ph.samson.atbp.liga.js.director

import com.raquo.laminar.api.L.*
import ph.samson.atbp.liga.js.api.ApiClient
import ph.samson.atbp.liga.js.api.Models.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Failure
import scala.util.Success

/** Root Laminar app for the tournament director console. */
object DirectorApp {

  def apply(client: ApiClient): Div = {
    val tournament = Var[Option[TournamentResponse]](None)
    val selectedMatchId = Var[Option[String]](None)
    val statusMessage = Var("")
    val busy = Var(false)

    def refresh(): Unit = {
      busy.set(true)
      client.getTournament.onComplete {
        case Success(value) =>
          tournament.set(Some(value))
          busy.set(false)
          statusMessage.set("")
        case Failure(err) =>
          busy.set(false)
          statusMessage.set(err.getMessage)
      }
    }

    def runAction(action: => Future[TournamentResponse]): Unit = {
      busy.set(true)
      action.onComplete {
        case Success(value) =>
          tournament.set(Some(value))
          busy.set(false)
          statusMessage.set("")
        case Failure(err) =>
          busy.set(false)
          statusMessage.set(err.getMessage)
      }
    }

    val selectedMatch =
      tournament.signal.combineWith(selectedMatchId).map {
        case (Some(t), Some(id)) =>
          t.bracket.flatMap(_.matches.find(_.id == id))
        case _ => None
      }

    div(
      cls := "director-app",
      onMountCallback { _ =>
        refresh()
      },
      div(
        cls := "header",
        h1("Liga Director"),
        child <-- tournament.signal.map { maybeTournament =>
          span(
            cls := "tournament-name",
            maybeTournament.map(_.name).getOrElse("Loading…")
          )
        },
        button(
          cls := "refresh",
          disabled <-- busy,
          onClick.mapTo(()) --> Observer[Unit](_ => refresh()),
          "Refresh"
        )
      ),
      child <-- statusMessage.signal.map { msg =>
        if (msg.nonEmpty) div(cls := "error", msg) else emptyNode
      },
      child <-- tournament.signal.map {
        case None =>
          div(p("Loading tournament…"))
        case Some(t) if t.bracket.isEmpty =>
          setupPanel(
            t,
            busy.signal,
            Observer[Unit](_ =>
              runAction(client.seed(DirectorDefaults.defaultRoundRaceTo))
            )
          )
        case Some(t) =>
          mainLayout(
            t,
            selectedMatch,
            busy.signal,
            selectedMatchId,
            client,
            runAction
          )
      },
      styleTag(directorStyles)
    )
  }

  private def mainLayout(
      tournament: TournamentResponse,
      selectedMatch: Signal[Option[BracketMatch]],
      busy: Signal[Boolean],
      selectedMatchId: Var[Option[String]],
      client: ApiClient,
      runAction: (=> Future[TournamentResponse]) => Unit
  ): Div =
    div(
      cls := "main-layout",
      div(
        cls := "bracket-column",
        BracketView(
          tournament.bracket.get,
          selectedMatchId.signal,
          Observer[String](id => selectedMatchId.set(Some(id)))
        )
      ),
      div(
        cls := "panel-column",
        child <-- selectedMatch.map {
          case Some(matchDef) =>
            MatchPanel(
              tournament,
              matchDef,
              busy,
              Observer[Unit](_ => runAction(client.ready(matchDef.id))),
              Observer[Int](handicap =>
                runAction(client.applyHandicap(matchDef.id, handicap))
              ),
              Observer[Unit](_ => runAction(client.start(matchDef.id))),
              Observer[(Int, Int)] { case (scoreA, scoreB) =>
                runAction(client.recordResult(matchDef.id, scoreA, scoreB))
              }
            )
          case None =>
            div(
              cls := "match-panel empty",
              p("Select a match from the bracket.")
            )
        }
      )
    )

  private def setupPanel(
      tournament: TournamentResponse,
      busy: Signal[Boolean],
      onSeed: Observer[Unit]
  ): Div =
    div(
      cls := "setup-panel",
      h2("Tournament setup"),
      p(s"${tournament.players.size} players loaded from period data."),
      ul(
        tournament.players.map(p => li(p.name))
      ),
      p("Seed the bracket to begin (race-to 7 for all rounds)."),
      button(
        cls := "primary",
        disabled <-- busy,
        onClick.mapTo(()) --> onSeed,
        "Seed bracket"
      )
    )

  private val directorStyles: String =
    """
      |.director-app { font-family: system-ui, sans-serif; margin: 1rem 2rem; max-width: 1200px; }
      |.header { display: flex; align-items: baseline; gap: 1rem; margin-bottom: 1rem; }
      |.tournament-name { color: #555; }
      |.error { color: #b00020; margin-bottom: 1rem; }
      |.main-layout { display: grid; grid-template-columns: 1fr 320px; gap: 1.5rem; }
      |.bracket-section { margin-bottom: 1rem; }
      |.round-matches { display: flex; flex-direction: column; gap: 0.35rem; }
      |.match-row {
      |  display: grid; grid-template-columns: 5rem 1fr auto auto; gap: 0.5rem;
      |  padding: 0.4rem 0.6rem; border: 1px solid #ddd; border-radius: 4px;
      |  cursor: pointer; background: #fafafa;
      |}
      |.match-row.selected { border-color: #1565c0; background: #e3f2fd; }
      |.match-row.actionable { border-left: 3px solid #2e7d32; }
      |.match-state { text-transform: uppercase; font-size: 0.75rem; color: #666; }
      |.match-panel { border: 1px solid #ccc; border-radius: 6px; padding: 1rem; background: #fff; }
      |.match-panel.empty { color: #666; }
      |.actions { display: flex; gap: 0.5rem; margin-top: 0.75rem; }
      |.score-entry { display: flex; flex-direction: column; gap: 0.5rem; margin: 0.75rem 0; }
      |button { padding: 0.4rem 0.8rem; cursor: pointer; }
      |button.primary { background: #1565c0; color: #fff; border: 1px solid #0d47a1; }
      |button:disabled { opacity: 0.5; cursor: not-allowed; }
      |input[type=number] { width: 4rem; }
      |""".stripMargin
}
