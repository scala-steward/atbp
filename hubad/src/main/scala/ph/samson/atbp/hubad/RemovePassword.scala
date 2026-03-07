package ph.samson.atbp.hubad

import better.files.File
import dk.brics.automaton.RegExp
import org.apache.pdfbox.Loader
import org.apache.pdfbox.pdmodel.PDDocument
import zio.Task
import zio.ZIO

import scala.jdk.CollectionConverters.*

object RemovePassword {

  def tryPattern(file: File, patterns: String*): Task[PDDocument] =
    ZIO.logSpan("tryPattern") {
      for {
        bytes <- ZIO.attemptBlockingIO(file.byteArray)
        patterns <- ZIO.logSpan("generate") {
          ZIO
            .succeed(patterns.flatMap(generate).toList)
            .tap(result => ZIO.logInfo(s"generated ${result.length} passwords"))
        }
        doc <-
          patterns match {
            case head :: next =>
              ZIO.raceAll(
                tryPassword(bytes, head),
                next.map(password => tryPassword(bytes, password))
              )
            case Nil =>
              ZIO.fail(
                new IllegalArgumentException(
                  s"No passwords generated for ${patterns.mkString(", ")}"
                )
              )
          }
        _ <- ZIO.logInfo(s"loaded $file")
      } yield doc
    }

  def tryPassword(
      bytes: Array[Byte],
      password: String
  ): Task[PDDocument] = ZIO.logSpan("tryPassword") {
    for {
      result <- ZIO.attempt(Loader.loadPDF(bytes, password))
      _ <- ZIO.logDebug(s"found password $password")
    } yield result
  }

  def generate(pattern: String): List[String] = {
    val regExp = new RegExp(pattern)
    val automaton = regExp.toAutomaton
    if (automaton.isFinite) {
      automaton.getFiniteStrings.asScala.toList
    } else {
      Nil
    }
  }

}
