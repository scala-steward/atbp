package ph.samson.atbp.cli

import better.files.File
import ph.samson.atbp.liga.cli.HandicapRenderer
import ph.samson.atbp.liga.cli.LeaderboardRenderer
import ph.samson.atbp.liga.io.PeriodLoader
import ph.samson.atbp.liga.serve.BindConfig
import ph.samson.atbp.liga.serve.ServeConfig
import ph.samson.atbp.liga.serve.ServeContext
import ph.samson.atbp.liga.serve.Server as LigaServer
import ph.samson.atbp.liga.tournament.Resume
import zio.Console
import zio.ZIO
import zio.cli.Args
import zio.cli.Command
import zio.cli.Exists.Either
import zio.cli.HelpDoc.*
import zio.cli.Options

case class Liga(action: Liga.Action) extends ToolCommand {
  override def run(conf: Conf): ZIO[Any, Throwable, Unit] = action.run(conf)
}

object Liga {

  sealed trait Action extends ToolCommand

  case class Leaderboard(data: File) extends Action {
    override def run(conf: Conf): ZIO[Any, Throwable, Unit] =
      ZIO.logSpan("Liga.leaderboard") {
        for {
          ratings <- PeriodLoader.loadAll(data)
          _ <- Console.printLine(LeaderboardRenderer.render(ratings))
        } yield ()
      }
  }

  case class Handicap(
      playerA: String,
      playerB: String,
      raceTo: Int,
      data: File
  ) extends Action {
    override def run(conf: Conf): ZIO[Any, Throwable, Unit] =
      ZIO.logSpan("Liga.handicap") {
        for {
          ratings <- PeriodLoader.loadAll(data)
          suggestion <- ZIO.fromEither(
            HandicapRenderer.suggest(ratings, playerA, playerB, raceTo)
          )
          _ <- Console.printLine(HandicapRenderer.render(suggestion))
        } yield ()
      }
  }

  case class Serve(
      data: File,
      port: Option[Int],
      lan: Boolean,
      newTournament: Option[String]
  ) extends Action {
    override def run(conf: Conf): ZIO[Any, Throwable, Unit] =
      ZIO.logSpan("Liga.serve") {
        for {
          tournamentDir <- Resume.resolve(data, newTournament)
          config <- ServeConfig.load(data)
          bind = BindConfig.from(
            ServeConfig.withOverrides(config, None, port),
            lan = lan
          )
          ctx = ServeContext(data, tournamentDir)
          _ <- Console.printLine(
            s"Liga serve on http://${bind.bindHost}:${bind.port} " +
              s"(tournament: ${tournamentDir.name})"
          )
          _ <- LigaServer.run(bind, ctx)
        } yield ()
      }
  }

  private def dataDir(dataDir: Option[java.nio.file.Path]): File =
    dataDir.map(path => File(path)).getOrElse(File.currentWorkingDirectory)

  private val data =
    Options
      .directory("data", Either)
      .optional ?? "Period data directory (default: current working directory)"

  private val raceTo =
    Options.integer("race") ?? "Race-to-N for the match"

  private val port =
    Options.integer("port").optional ?? "HTTP port (default: 5442)"

  private val lan =
    Options.boolean("lan").withDefault(false) ??
      "Expose audience routes on LAN (0.0.0.0)"

  private val newTournament =
    Options.text("new").optional ?? "Start a new tournament with this name"

  private val playerA = Args.text("player-a")
  private val playerB = Args.text("player-b")

  private val leaderboardCommand =
    Command("leaderboard", data).map { dir =>
      Leaderboard(dataDir(dir))
    }

  private val handicapCommand =
    Command("handicap", data ++ raceTo, playerA ++ playerB)
      .withHelp(
        p(
          "Options (--race, --data) must appear before player names: " +
            "liga handicap --race 7 [--data dir] <player-a> <player-b>"
        )
      )
      .map { case ((dir, race), (a, b)) =>
        Handicap(a, b, race.toInt, dataDir(dir))
      }

  private val serveCommand =
    Command("serve", data ++ port ++ lan ++ newTournament).map {
      case (dir, portOpt, lanEnabled, newName) =>
        Serve(dataDir(dir), portOpt.map(_.toInt), lanEnabled, newName)
    }

  private val withSubcommands =
    Command("liga")
      .subcommands(leaderboardCommand, handicapCommand, serveCommand)
      .map(Liga.apply)

  private val defaultLeaderboard =
    Command("liga", data).map { dir =>
      Liga(Leaderboard(dataDir(dir)))
    }

  val command: Command[Liga] =
    withSubcommands
      .orElse(defaultLeaderboard)
      .withHelp(
        blocks(
          h2("Liga"),
          p("Billiards club ratings, handicaps, and tournaments.")
        )
      )
}
