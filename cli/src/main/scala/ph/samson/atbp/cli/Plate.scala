package ph.samson.atbp.cli

import better.files.File
import ph.samson.atbp.cli.Plate.Action
import ph.samson.atbp.jira.Client
import ph.samson.atbp.plate.Inspector
import ph.samson.atbp.plate.Labeler
import ph.samson.atbp.plate.RadarScanner
import ph.samson.atbp.plate.Sorter
import zio.ZIO
import zio.cli.Args
import zio.cli.Command
import zio.cli.Exists.Either
import zio.cli.Exists.Yes
import zio.cli.HelpDoc.blocks
import zio.cli.HelpDoc.h2
import zio.cli.HelpDoc.p
import zio.cli.Options
import zio.http.ZClient

case class Plate(action: Action) extends ToolCommand {
  override def run(conf: Conf): ZIO[Any, Throwable, Unit] = action.run(conf)
}

object Plate {
  import ph.samson.atbp.cli.Plate.Check.Status

  private val source = Args.file("source", Yes)
  private val target = Options.file("target", Either).optional

  sealed trait Action extends ToolCommand

  private case class Label(source: File, value: String) extends Action {
    override def run(conf: Conf): ZIO[Any, Throwable, Unit] = {
      conf.jiraConf match {
        case None       => ZIO.fail(new Exception("No jira config."))
        case Some(jira) =>
          doRun().provide(ZClient.default, Client.layer(jira), Labeler.layer())
      }
    }

    def doRun() = ZIO.logSpan("label") {
      for {
        labeler <- ZIO.service[Labeler]
        _ <- labeler.label(source, value)
      } yield ()
    }
  }

  private object Label {
    val value = Options.text("value")
    val command = Command("label", Label.value, source).map { (v, s) =>
      Label(s, v)
    }
  }

  private case class Check(source: File, target: Option[File], status: Status)
      extends Action {
    override def run(conf: Conf): ZIO[Any, Throwable, Unit] = {
      conf.jiraConf match {
        case None       => ZIO.fail(new Exception("No jira config."))
        case Some(jira) =>
          val check = for {
            inspector <- ZIO.service[Inspector]
            result <- status match {
              case Check.Cooking => inspector.cooking(source, target)
              case Check.Stale   => inspector.stale(source, target)
              case Check.Done    => inspector.done(source, target)
            }
            _ <- ZIO.logInfo(s"check result: $result")
          } yield ()

          check.provide(ZClient.default, Client.layer(jira), Inspector.layer())
      }
    }
  }

  private object Check {
    sealed trait Status
    case object Cooking extends Status
    case object Stale extends Status
    case object Done extends Status

    val status: Options[Status] = Options.enumeration("status")(
      "cooking" -> Cooking,
      "stale" -> Stale,
      "done" -> Done
    )

    val command = Command("check", status ++ target, source).map {
      case ((status, target), source) =>
        Check(source, target.map(File(_)), status)
    }
  }

  private case class Radar(
      source: File,
      target: Option[File],
      exclude: List[String]
  ) extends Action {
    override def run(conf: Conf): ZIO[Any, Throwable, Unit] = {
      conf.jiraConf match {
        case None       => ZIO.fail(new Exception("No jira config."))
        case Some(jira) =>
          val scan = for {
            scanner <- ZIO.service[RadarScanner]
            result <- scanner.scan(source, target, exclude)
            _ <- ZIO.logInfo(s"radar result: $result")
          } yield ()

          scan.provide(
            ZClient.default,
            Client.layer(jira),
            RadarScanner.layer()
          )
      }
    }
  }

  private object Radar {
    val exclude = Options
      .text("exclude")
      .map(_.split(',').toList.map(_.trim))
      .withDefault(Nil) ?? "Projects to exclude"

    val command = Command("radar", target ++ exclude, source).map {
      case ((target, exclude), source) =>
        Radar(source, target.map(File(_)), exclude)
    }
  }

  private case class Sort(source: File) extends Action {
    override def run(conf: Conf): ZIO[Any, Throwable, Unit] = {
      conf.jiraConf match {
        case None       => ZIO.fail(new Exception("No jira config."))
        case Some(jira) =>
          doRun().provide(ZClient.default, Client.layer(jira), Sorter.layer())
      }
    }

    def doRun() = ZIO.logSpan("sort") {
      for {
        sorter <- ZIO.service[Sorter]
        result <- sorter.sort(source)
        _ <- ZIO.logInfo(s"sorted: $result")
      } yield ()
    }
  }

  private object Sort {
    val command = Command("sort", source)
      .map(s => Sort(s))
      .withHelp("Sort Jira tickets according to their order in the Plate")
  }

  val command: Command[Plate] =
    Command("plate")
      .withHelp(
        blocks(
          h2("Plate Tools"),
          p("Tools for managing The Plate.")
        )
      )
      .subcommands(Label.command, Check.command, Radar.command, Sort.command)
      .map(Plate.apply)
}
