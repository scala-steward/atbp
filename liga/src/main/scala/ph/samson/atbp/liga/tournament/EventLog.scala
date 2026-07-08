package ph.samson.atbp.liga.tournament

import better.files.File
import ph.samson.atbp.liga.tournament.events.TournamentEvent
import zio.Task
import zio.ZIO

/** Append-only JSON event log for one tournament directory. */
object EventLog {

  final case class EventFileExists(path: String)
      extends Exception(s"Event file already exists: $path")

  final case class InvalidSeq(expected: Int, actual: Int)
      extends Exception(s"Event seq must be $expected, got $actual")

  def read(dir: File): Task[List[TournamentEvent]] =
    ZIO.attemptBlocking {
      if (!dir.exists) Nil
      else
        dir.list
          .filter(_.extension(includeDot = false).contains("json"))
          .flatMap(parseFile)
          .toList
          .sortBy(_.seq)
    }

  def append(dir: File, event: TournamentEvent): Task[Unit] =
    for {
      _ <- ZIO.attemptBlocking(
        dir.createDirectoryIfNotExists(createParents = true)
      )
      existing <- read(dir)
      expectedSeq = existing.map(_.seq).maxOption.getOrElse(0) + 1
      _ <- ZIO.when(event.seq != expectedSeq) {
        ZIO.fail(InvalidSeq(expectedSeq, event.seq))
      }
      filename = filenameFor(event)
      target = dir / filename
      _ <- ZIO.when(target.exists) {
        ZIO.fail(EventFileExists(target.pathAsString))
      }
      _ <- ZIO.attemptBlocking {
        dir.createChild(filename).overwrite(EventCodec.encode(event))
      }
    } yield ()

  def filenameFor(event: TournamentEvent): String =
    f"${event.seq}%06d-${suffixFor(event)}.json"

  private def suffixFor(event: TournamentEvent): String =
    event match {
      case _: TournamentEvent.Created             => "created"
      case _: TournamentEvent.PlayersSet          => "players-set"
      case _: TournamentEvent.PlayersLocked       => "players-locked"
      case _: TournamentEvent.RoundRaceToSet      => "round-race-to"
      case _: TournamentEvent.BracketSeeded       => "seeded"
      case _: TournamentEvent.MatchReady          => "match-ready"
      case _: TournamentEvent.HandicapApplied     => "handicap"
      case _: TournamentEvent.MatchStarted        => "started"
      case _: TournamentEvent.MatchResult         => "result"
      case _: TournamentEvent.TournamentCompleted => "completed"
    }

  private def parseFile(file: File): Option[TournamentEvent] =
    EventCodec.decode(file.contentAsString).toOption
}
