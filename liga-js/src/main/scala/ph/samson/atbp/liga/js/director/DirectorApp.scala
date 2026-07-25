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
    val leaderboard = Var[Option[LeaderboardResponse]](None)
    val latestRatings = Var[Option[LatestRatingsResponse]](None)
    val selectedMatchId = Var[Option[String]](None)
    val statusMessage = Var("")
    val busy = Var(false)

    def selectFirstActionable(tournament: TournamentResponse): Unit =
      if (selectedMatchId.now().isEmpty) {
        tournament.bracket
          .flatMap(_.matches.find(BracketLayout.isActionable))
          .foreach(matchDef => selectedMatchId.set(Some(matchDef.id)))
      }

    def refresh(): Unit = {
      busy.set(true)
      val loaded = client.getTournament.flatMap { t =>
        val phase = TournamentPhase.fromApi(t.phase)
        val leaderboardF =
          if (DirectorIdlePolicy.needsLeaderboard(phase)) {
            client.getLeaderboard.map(Some(_))
          } else {
            Future.successful(None)
          }
        val latestRatingsF =
          if (DirectorIdlePolicy.needsLatestRatings(phase)) {
            client.getLatestRatings.map(Some(_))
          } else {
            Future.successful(None)
          }
        for {
          lb <- leaderboardF
          lr <- latestRatingsF
        } yield (t, lb, lr)
      }
      loaded.onComplete {
        case Success((t, lb, lr)) =>
          tournament.set(Some(t))
          leaderboard.set(lb)
          latestRatings.set(lr)
          selectFirstActionable(t)
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
          selectFirstActionable(value)
          busy.set(false)
          statusMessage.set("")
          client.getLeaderboard.onComplete {
            case Success(lb) => leaderboard.set(Some(lb))
            case _           => ()
          }
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
            maybeTournament
              .map(_.name)
              .filter(_.nonEmpty)
              .getOrElse("No tournament")
          )
        },
        button(
          cls := "refresh",
          disabled <-- busy,
          onClick.mapTo(()) --> Observer[Unit](_ => refresh()),
          "Refresh"
        )
      ),
      child <-- statusMessage.signal
        .combineWith(tournament.signal, selectedMatchId.signal)
        .map { case (msg, maybeTournament, maybeMatchId) =>
          if (msg.nonEmpty) {
            div(
              cls := "error",
              DirectorGuidance.friendlyApiErrorForSelectedMatch(
                msg,
                maybeTournament,
                maybeMatchId
              )
            )
          } else {
            emptyNode
          }
        },
      child <-- tournament.signal
        .combineWith(leaderboard.signal, latestRatings.signal)
        .map { case (maybeTournament, maybeLeaderboard, maybeLatestRatings) =>
          DirectorIdlePolicy.view(
            maybeTournament,
            maybeLeaderboard,
            maybeLatestRatings
          ) match {
            case DirectorIdlePolicy.View.LoadingTournament =>
              div(p("Loading…"))
            case DirectorIdlePolicy.View.LoadingLeaderboard =>
              div(p("Loading leaderboard…"))
            case DirectorIdlePolicy.View.LoadingLatestRatings =>
              div(p("Loading latest ratings…"))
            case DirectorIdlePolicy.View.Idle(latest) =>
              IdleDirectorView(
                latest,
                busy.signal,
                Observer[String](name =>
                  runAction(client.createTournament(name))
                )
              )
            case DirectorIdlePolicy.View.Wizard(t, lb) =>
              WizardView(
                t,
                lb,
                busy.signal,
                Observer[List[Player]](players =>
                  runAction(client.setPlayers(players))
                ),
                Observer[List[Player]] { players =>
                  runAction(
                    client
                      .setPlayers(players)
                      .flatMap(_ => client.lockPlayers())
                  )
                },
                Observer[Map[String, Int]](raceToByScope =>
                  runAction(client.setRaceTo(raceToByScope))
                ),
                Observer[Unit](_ => runAction(client.seed()))
              )
            case DirectorIdlePolicy.View.Live(t) =>
              div(
                completionBar(t, busy.signal, runAction, client),
                mainLayout(
                  t,
                  selectedMatch,
                  busy.signal,
                  selectedMatchId,
                  client,
                  runAction
                )
              )
          }
        },
      p(cls := "footer-note", DirectorGuidance.localhostNote)
    )
  }

  private def completionBar(
      tournament: TournamentResponse,
      busy: Signal[Boolean],
      runAction: (=> Future[TournamentResponse]) => Unit,
      client: ApiClient
  ): Node =
    if (tournament.completed) {
      div(
        cls := "completed-banner",
        p("Tournament complete — ratings period written to data directory.")
      )
    } else if (
      tournament.bracket
        .exists(b => BracketLayout.allMatchesCompleted(b.matches))
    ) {
      div(
        cls := "complete-panel",
        p("All bracket matches are finished."),
        button(
          cls := "primary",
          disabled <-- busy,
          onClick.mapTo(()) --> Observer[Unit](_ =>
            runAction(client.completeTournament())
          ),
          "Complete tournament"
        )
      )
    } else {
      emptyNode
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
          tournament.raceToByScope,
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
              p("Select a match from the bracket."),
              p(cls := "guidance", DirectorGuidance.matchWorkflowOverview),
              p(
                cls := "hint",
                "Green border = match needs director action. " +
                  "After byes, play the remaining winners-round-1 match first."
              )
            )
        }
      )
    )
}
