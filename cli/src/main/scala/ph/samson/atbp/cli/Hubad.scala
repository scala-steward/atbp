package ph.samson.atbp.cli

import better.files.File
import ph.samson.atbp.hubad.RemovePassword
import zio.ZIO
import zio.cli.Args
import zio.cli.Command
import zio.cli.Exists.Yes
import zio.cli.HelpDoc.*
import zio.cli.Options

case class Hubad(source: File, target: Option[File]) extends ToolCommand {
  override def run(conf: Conf): zio.ZIO[Any, Throwable, Unit] = {
    val outPath = target match {
      case Some(value) => if (value.isDirectory) value / source.name else value
      case None        =>
        source.sibling(s"${source.nameWithoutExtension(false)}.hubad.pdf")
    }
    for {
      pdf <- RemovePassword.tryPattern(
        source,
        "" :: conf.hubad.map(_.passwords).getOrElse(Nil)*
      )
      out <- ZIO.attemptBlockingIO {
        pdf.setAllSecurityToBeRemoved(true)
        pdf.save(outPath.toJava)
      }
      _ <- ZIO.logInfo(s"Password removed from $source to $outPath")
    } yield ()
  }
}

object Hubad {
  private val source = Args.file("source", Yes)
  private val target = Options.file("target", Yes).optional

  val command: Command[Hubad] = Command("hubad", target, source)
    .withHelp(
      blocks(
        h2("Remove password from a PDF file"),
        p("Read a given source FILE and remove the password from it.")
      )
    )
    .map { case (target, source) => Hubad(source, target.map(File.apply)) }
}
