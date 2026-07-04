package ph.samson.atbp.cli

import better.files.File
import ph.samson.atbp.liga.cli.HandicapRenderer
import ph.samson.atbp.liga.cli.LeaderboardRenderer
import ph.samson.atbp.liga.io.PeriodLoader
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

  private def dataDir(dataDir: Option[java.nio.file.Path]): File =
    dataDir.map(path => File(path)).getOrElse(File.currentWorkingDirectory)

  private val data =
    Options
      .directory("data", Either)
      .optional ?? "Period data directory (default: current working directory)"

  private val raceTo =
    Options.integer("race") ?? "Race-to-N for the match"

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

  private val withSubcommands =
    Command("liga")
      .subcommands(leaderboardCommand, handicapCommand)
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
