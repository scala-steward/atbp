package ph.samson.atbp.plate

import better.files.File
import ph.samson.atbp.jira.Client
import ph.samson.atbp.jira.model.Changelog
import ph.samson.atbp.jira.model.Comment
import ph.samson.atbp.jira.model.Issue
import ph.samson.atbp.plate.JiraOps.*
import zio.Clock
import zio.Task
import zio.ZIO
import zio.ZLayer

import java.time.LocalTime
import java.time.ZoneId
import java.time.temporal.ChronoUnit.DAYS
import scala.util.control.NoStackTrace

trait Inspector {
  def cooking(source: File, target: Option[File]): Task[File]
  def stale(source: File, target: Option[File]): Task[File]
  def done(source: File, target: Option[File]): Task[File]
}

object Inspector {

  val JiraLink = """.*\(https://.*/browse/([A-Z]+-\d+)\).*""".r
  val CookingProgressDays = 14
  val StaleProgressDays = 28

  private def includeLine(
      check: String => Task[Boolean]
  )(line: String): Task[Boolean] = {
    line match {
      case ""                                 => ZIO.succeed(true)
      case heading if heading.startsWith("#") => ZIO.succeed(true)
      case JiraLink(key)                      => check(key)
      case _                                  => ZIO.succeed(false)
    }
  }

  private class LiveImpl(client: Client) extends Inspector {

    /** Extract relevant lines from the given source file and write to a sibling
      * file named with the given suffix.
      *
      * @param source
      *   source file
      * @param suffix
      *   suffix for naming the target file
      * @param check
      *   criteria for deciding if a Jira key should be included
      * @return
      *   target file
      */
    private def extract(source: File, target: File)(
        check: String => Task[Boolean]
    ): Task[File] =
      for {
        sourceLines <- ZIO.attemptBlockingIO(source.lines.toList)
        targetLines <- ZIO.filterPar(sourceLines)(includeLine(check))
        outFile <- ZIO.attemptBlockingIO {
          target.overwrite(prune(targetLines).mkString("\n"))
        }
      } yield outFile

    private def sibling(source: File, suffix: String) = {
      val name = source.`extension`() match {
        case Some(ext) =>
          source.nameWithoutExtension(includeAll = false) + suffix + ext
        case None => source.name + suffix
      }
      source.sibling(name)
    }

    override def done(source: File, target: Option[File]): Task[File] =
      ZIO.logSpan("done") {
        extract(source, target.getOrElse(sibling(source, ".done"))) { key =>
          for {
            issue <- client.getIssue(key)
            descendants <- client.getDescendants(key)
          } yield issue.isDone && descendants.forall(_.isDone)
        }
      }

    override def cooking(source: File, target: Option[File]): Task[File] =
      ZIO.logSpan("cooking") {
        extract(source, target.getOrElse(sibling(source, ".cooking"))) { key =>
          for {
            issue <- client.getIssue(key)
            result <- ZIO.succeed(!issue.isDone) && (
              hasProgress(issue, CookingProgressDays)
                || anyDescendantHasProgress(issue, CookingProgressDays)
            )
          } yield result
        }
      }

    private def hasProgress(issue: Issue, sinceDays: Int): Task[Boolean] = {
      for {
        now <- Clock.instant
        progressLimit = now
          .atZone(ZoneId.systemDefault())
          .minus(sinceDays, DAYS)
          .`with`(LocalTime.MIDNIGHT)
        (changelogs, comments) <- client
          .getChangelogs(issue.key)
          .zipPar(client.getComments(issue.key))
      } yield {
        val hasNewChanges = changelogs.exists {
          case Changelog(_, _, created, items, _) =>
            created.isAfter(progressLimit) &&
            !items.forall { item =>
              // changes we don't count as progress
              item.field == "Rank"
              || item.field == "labels"
            }
        }

        val hasNewComments = comments.exists {
          case Comment(_, _, _, _, created, updated, _) =>
            created.isAfter(progressLimit)
            || updated.isAfter(progressLimit)
        }

        hasNewChanges || hasNewComments
      }
    }

    private def anyDescendantHasProgress(
        issue: Issue,
        sinceDays: Int
    ): Task[Boolean] = {
      def checkProgress(issue: Issue) =
        hasProgress(issue, sinceDays).filterOrFail(_.self)(NoProgress)
      ZIO.logSpan("anyDescendantHasProgress") {
        for {
          descendants <- client.getDescendants(issue.key)
          progress = descendants match {
            case Nil          => ZIO.succeed(false)
            case head :: next =>
              checkProgress(head).raceAll(next.map(checkProgress))
          }
          result <- progress.catchSome { case NoProgress =>
            ZIO.succeed(false)
          }
        } yield {
          result
        }
      }
    }

    /** A set of issues, considered together, is determined to be "In Progress"
      * if some of them are In Progress or some are Done while there are others
      * still not Done.
      *
      * @param issues
      * @return
      */
    private def aggregateInProgress(issues: List[Issue]) = {
      val someInProgress = issues.exists(_.inProgress)
      val partialDone = issues.exists(_.isDone) && issues.exists(!_.isDone)
      someInProgress || partialDone
    }

    override def stale(source: File, target: Option[File]): Task[File] =
      ZIO.logSpan("stale") {
        extract(source, target.getOrElse(sibling(source, ".stale"))) { key =>
          for {
            issue <- client.getIssue(key)
            result <-
              if (issue.isDone) {
                ZIO.succeed(false)
              } else if (issue.inProgress) {
                (
                  hasProgress(issue, StaleProgressDays)
                    || anyDescendantHasProgress(issue, StaleProgressDays)
                ).negate
              } else {
                allDescendantsStale(issue, StaleProgressDays)
              }
          } yield result
        }
      }

    def allDescendantsStale(issue: Issue, sinceDays: Int): Task[Boolean] =
      ZIO.logSpan("allDescendantsStale") {
        for {
          descendants <- client.getDescendants(issue.key)
          inProgress = aggregateInProgress(descendants)
          hasProgress <- anyDescendantHasProgress(issue, sinceDays)
        } yield {
          inProgress && !hasProgress
        }
      }

    /** Remove empty sections and extra blank lines.
      *
      * @param lines
      *   output lines
      * @return
      *   pruned output
      */
    def prune(lines: List[String]): List[String] = {

      def level(section: String) = section.takeWhile(_ == '#').length

      /** Prune a section
        *
        * @return
        *   tuple of (pruned section, unprocessed lines)
        */
      def pruneSection(
          start: String,
          in: List[String],
          entries: List[String]
      ): (List[String], List[String]) = {
        in match {
          case Nil =>
            entries match { // this is the last section
              case Nil                                     => (Nil, Nil)
              case nonEmpty if nonEmpty.exists(_.nonEmpty) =>
                (nonEmpty :+ start, Nil)
              case _ => (Nil, Nil) // entries are just blanks
            }
          case line :: rest =>
            line.trim match {
              case "" =>
                entries match {
                  case "" :: _ =>
                    // ignore consecutive blank lines
                    pruneSection(
                      start,
                      rest,
                      entries
                    )
                  case _ =>
                    // keep the first blank in
                    pruneSection(start, rest, "" :: entries)
                }
              case section if section.startsWith("#") =>
                if (level(section) > level(start)) {
                  // entering subsection
                  val (subsection, remaining) = pruneSection(section, rest, Nil)
                  pruneSection(start, remaining, subsection ++ entries)
                } else {
                  // end of section
                  entries match {
                    case Nil => (Nil, section :: rest)
                    case nonEmpty if nonEmpty.exists(_.nonEmpty) =>
                      (nonEmpty :+ start, section :: rest)
                    case _ => (Nil, section :: rest) // entries are just blanks
                  }
                }
              case _ => pruneSection(start, rest, line :: entries)
            }
        }
      }

      def doPrune(in: List[String], out: List[String]): List[String] = {
        in match {
          case Nil          => out
          case line :: rest =>
            line.trim match {
              case "" =>
                out match {
                  case Nil => doPrune(rest, out) // ignore leading blank lines
                  case "" :: _ =>
                    doPrune(rest, out) // ignore consecutive blanks
                  case _ => doPrune(rest, "" :: out) // keep first blank
                }
              case section if section.startsWith("#") =>
                val (subsection, remaining) = pruneSection(section, rest, Nil)
                doPrune(remaining, subsection ++ out)
              case nonblank => doPrune(rest, nonblank :: out)
            }
        }
      }

      doPrune(lines, Nil).reverse
    }
  }

  private case object NoProgress extends Exception with NoStackTrace

  def layer() = ZLayer {
    for {
      client <- ZIO.service[Client]
    } yield LiveImpl(client): Inspector
  }
}
