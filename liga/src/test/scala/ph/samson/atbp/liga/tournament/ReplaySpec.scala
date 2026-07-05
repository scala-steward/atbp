package ph.samson.atbp.liga.tournament

import better.files.File
import ph.samson.atbp.liga.bracket.BracketGen
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.tournament.events.TournamentEvent
import zio.*
import zio.test.*

import java.time.Instant

object ReplaySpec extends ZIOSpecDefault {

  private val at = Instant.parse("2026-03-15T18:00:00Z")

  private def rating(name: String, r: Double): PlayerRating =
    PlayerRating(Player(name), r, rd = 100, wins = 0, losses = 0)

  private def eightPlayerRatings: List[PlayerRating] =
    (1 to 8).map(i => rating(s"P$i", 1700 - i * 10)).toList

  private def withTempDir[R, A](f: File => ZIO[R, Throwable, A]): ZIO[R, Throwable, A] =
    ZIO.acquireReleaseWith(
      ZIO.attemptBlocking(File.newTemporaryDirectory("liga-replay"))
    )(dir => ZIO.attemptBlocking(dir.delete()).ignore)(f)

  private def appendAll(dir: File, events: List[TournamentEvent]): Task[Unit] =
    ZIO.foreachDiscard(events)(event => EventLog.append(dir, event))

  def spec = suite("Replay")(
    test("EventLog writes zero-padded filenames and never overwrites") {
      withTempDir { dir =>
        val created = TournamentEvent.Created(
          seq = 1,
          at = at,
          payload = TournamentCreatedPayload(
            name = "Spring Open",
            players = eightPlayerRatings.map(_.player)
          )
        )
        val seeded = TournamentEvent.BracketSeeded(
          seq = 2,
          at = at,
          payload = BracketSeededPayload(
            frozenRatings = eightPlayerRatings,
            bracket = BracketGen.generate(eightPlayerRatings)
          )
        )
        for {
          _ <- EventLog.append(dir, created)
          _ <- EventLog.append(dir, seeded)
          files = dir.list.toList.sortBy(_.name)
          loaded <- EventLog.read(dir)
          duplicate <- EventLog.append(dir, created).either
        } yield assertTrue(
          files.map(_.name) == List("000001-created.json", "000002-seeded.json"),
          loaded.size == 2,
          duplicate.isLeft
        )
      }
    },
    test("replay from fixture dir reproduces bracket after seeding") {
      val dir = File(getClass.getResource("/tournaments/eight-player-seeded"))
      for {
        state <- Replay.replayDir(dir)
      } yield assertTrue(
        state.name == "Spring Open",
        state.players.size == 8,
        state.frozenRatings.size == 8,
        state.bracket.exists(_.size == 8),
        !Replay.isComplete(state),
        state.bracket.exists(_.matches.exists(_.id == "wb-1-1"))
      )
    },
    test("replay folds match lifecycle events onto bracket") {
      withTempDir { dir =>
        val bracket = BracketGen.generate(eightPlayerRatings)
        val events = List(
          TournamentEvent.Created(
            seq = 1,
            at = at,
            payload = TournamentCreatedPayload(
              name = "Spring Open",
              players = eightPlayerRatings.map(_.player)
            )
          ),
          TournamentEvent.BracketSeeded(
            seq = 2,
            at = at,
            payload = BracketSeededPayload(
              frozenRatings = eightPlayerRatings,
              bracket = bracket
            )
          ),
          TournamentEvent.MatchReady(
            seq = 3,
            at = at,
            payload = MatchReadyPayload(matchId = "wb-1-1", handicapSuggested = 2)
          ),
          TournamentEvent.HandicapApplied(
            seq = 4,
            at = at,
            payload = HandicapAppliedPayload(matchId = "wb-1-1", handicapApplied = 3)
          ),
          TournamentEvent.MatchStarted(
            seq = 5,
            at = at,
            payload = MatchStartedPayload(matchId = "wb-1-1")
          ),
          TournamentEvent.MatchResult(
            seq = 6,
            at = at,
            payload = MatchResultPayload(matchId = "wb-1-1", scoreA = 7, scoreB = 4)
          )
        )
        for {
          _ <- appendAll(dir, events)
          state <- Replay.replayDir(dir)
          matchDef = state.bracket.flatMap(_.matches.find(_.id == "wb-1-1")).get
        } yield assertTrue(
          matchDef.state == BracketMatchState.Completed,
          matchDef.handicapSuggested.contains(2),
          matchDef.handicapApplied.contains(3),
          matchDef.result.contains(MatchResult(7, 4))
        )
      }
    },
    test("partial replay without TournamentCompleted is incomplete") {
      val dir = File(getClass.getResource("/tournaments/eight-player-partial"))
      for {
        state <- Replay.replayDir(dir)
      } yield assertTrue(
        !Replay.isComplete(state),
        state.bracket.exists(_.matches.exists(_.id == "wb-1-1")),
        state.bracket
          .flatMap(_.matches.find(_.id == "wb-1-1"))
          .exists(_.state == BracketMatchState.Started)
      )
    },
    test("replay marks complete when TournamentCompleted is present") {
      val dir = File(getClass.getResource("/tournaments/eight-player-complete"))
      for {
        state <- Replay.replayDir(dir)
      } yield assertTrue(
        Replay.isComplete(state),
        state.completed
      )
    },
    test("replay rejects non-monotonic seq values") {
      val events = List(
        TournamentEvent.Created(
          seq = 1,
          at = at,
          payload = TournamentCreatedPayload("Open", Nil)
        ),
        TournamentEvent.MatchStarted(
          seq = 1,
          at = at,
          payload = MatchStartedPayload("wb-1-1")
        )
      )
      assertTrue(Replay.replay(events).isLeft)
    },
    test("EventLog read sorts by seq not filename") {
      withTempDir { dir =>
        val first = TournamentEvent.Created(
          seq = 1,
          at = at,
          payload = TournamentCreatedPayload(
            name = "Spring Open",
            players = eightPlayerRatings.map(_.player)
          )
        )
        val second = TournamentEvent.BracketSeeded(
          seq = 2,
          at = at,
          payload = BracketSeededPayload(
            frozenRatings = eightPlayerRatings,
            bracket = BracketGen.generate(eightPlayerRatings)
          )
        )
        for {
          _ <- ZIO.attemptBlocking {
            dir.createChild("000002-seeded.json").write(EventCodec.encode(second))
            dir.createChild("000001-created.json").write(EventCodec.encode(first))
          }
          loaded <- EventLog.read(dir)
        } yield assertTrue(loaded.map(_.seq) == List(1, 2))
      }
    }
  )
}
