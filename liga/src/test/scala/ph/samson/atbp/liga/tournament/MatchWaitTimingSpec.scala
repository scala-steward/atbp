package ph.samson.atbp.liga.tournament

import ph.samson.atbp.liga.bracket.BracketGen
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.testsupport.RaceToTestSupport
import ph.samson.atbp.liga.testsupport.TournamentTestFixtures.*
import ph.samson.atbp.liga.tournament.events.TournamentEvent
import zio.test.*

import java.time.Instant

object MatchWaitTimingSpec extends ZIOSpecDefault {

  private val seedAt = at
  private val readyAt = at.plusSeconds(3600)
  private val resultAt = at.plusSeconds(7200)

  private def timingOf(
      events: List[TournamentEvent],
      matchId: String
  ): MatchWaitTiming.Timing =
    MatchWaitTiming.project(events).toOption.get(matchId)

  private def customSeededEvents(
      ratings: List[PlayerRating],
      bracket: Bracket
  ): List[TournamentEvent] = {
    val players = ratings.map(_.player)
    val raceToEvents =
      RaceToTestSupport.raceToSetEvents(players.size, startSeq = 4, at = at)
    List(
      TournamentEvent.Created(
        seq = 1,
        at = at,
        payload = TournamentCreatedPayload(name = "Test", players = Nil)
      ),
      TournamentEvent.PlayersSet(
        seq = 2,
        at = at,
        payload = PlayersSetPayload(players = players)
      ),
      TournamentEvent.PlayersLocked(
        seq = 3,
        at = at,
        payload = PlayersLockedPayload()
      )
    ) ++ raceToEvents :+ TournamentEvent.BracketSeeded(
      seq = 4 + raceToEvents.size,
      at = seedAt,
      payload = BracketSeededPayload(
        frozenRatings = ratings,
        bracket = bracket
      )
    )
  }

  private def resultOnMatch(
      base: TournamentState,
      matchId: String,
      resultSeq: Int,
      resultAt: Instant,
      scoreA: Int,
      scoreB: Int
  ): List[TournamentEvent] = {
    val seeded = seededEvents(base)
    val afterSeed = Replay.replay(seeded).toOption.get
    val readyEvents =
      Tournament.ready(afterSeed, matchId, resultSeq, at).toOption.get
    val withReady = seeded ++ readyEvents
    val afterReady = Replay.replay(withReady).toOption.get
    val nextSeq = resultSeq + readyEvents.size
    val withHandicap =
      if (
        readyEvents.exists {
          case _: TournamentEvent.HandicapApplied => true
          case _                                  => false
        }
      ) {
        withReady
      } else {
        val handicap =
          Tournament
            .applyHandicap(afterReady, matchId, handicap = 2, nextSeq, at)
            .toOption
            .get
        withReady :+ handicap
      }
    val afterHandicap = Replay.replay(withHandicap).toOption.get
    val handicapSeq = withHandicap.map(_.seq).max + 1
    val started =
      Tournament.start(afterHandicap, matchId, handicapSeq, at).toOption.get
    val withStarted = withHandicap :+ started
    val afterStart = Replay.replay(withStarted).toOption.get
    val result =
      Tournament
        .recordResult(
          afterStart,
          matchId,
          scoreA = scoreA,
          scoreB = scoreB,
          handicapSeq + 1,
          resultAt
        )
        .toOption
        .get
    withStarted :+ result
  }

  def spec = suite("MatchWaitTiming")(
    test("projectWithState state matches Replay.replay") {
      val events = resultOnMatch(
        seededState(),
        matchId = "wb-1-1",
        resultSeq = 13,
        resultAt = resultAt,
        scoreA = 7,
        scoreB = 4
      )
      val projected = MatchWaitTiming.projectWithState(events).toOption.get
      val replayed = Replay.replay(events).toOption.get
      assertTrue(
        projected._1 == replayed,
        projected._2("wb-1-1").completedAt.contains(resultAt)
      )
    },
    test("R1 both-seeded Ready wait is BracketSeeded.at, not MatchReady.at") {
      val state = seededState()
      val events =
        seededEvents(state) :+
          TournamentEvent.MatchReady(
            seq = 13,
            at = readyAt,
            payload =
              MatchReadyPayload(matchId = "wb-1-1", handicapSuggested = 2)
          )
      val timing = timingOf(events, "wb-1-1")
      assertTrue(
        timing.waitStartedAt.contains(seedAt),
        timing.waitStartedAt != Some(readyAt)
      )
    },
    test("feeder result arrival uses MatchResult.at for downstream fill") {
      val base = seededState()
      val events = resultOnMatch(
        base,
        matchId = "wb-1-4",
        resultSeq = 13,
        resultAt = resultAt,
        scoreA = 7,
        scoreB = 4
      )
      val after = Replay.replay(events).toOption.get
      val wb22 = matchOf(after, "wb-2-2")
      val timing = timingOf(events, "wb-2-2")
      assertTrue(
        wb22.state == BracketMatchState.Pending,
        wb22.playerA.isDefined != wb22.playerB.isDefined,
        timing.waitStartedAt.contains(resultAt)
      )
    },
    test("both sides filled wait is earlier side arrival") {
      val base = seededState()
      val firstResultAt = at.plusSeconds(1000)
      val secondResultAt = at.plusSeconds(2000)
      val events1 =
        resultOnMatch(
          base,
          "wb-1-1",
          resultSeq = 13,
          resultAt = firstResultAt,
          scoreA = 7,
          scoreB = 4
        )
      val after1 = Replay.replay(events1).toOption.get
      val ready2Events =
        Tournament.ready(after1, "wb-1-2", seq = 17, at).toOption.get
      val withReady2 = events1 ++ ready2Events
      val afterReady2 = Replay.replay(withReady2).toOption.get
      val nextSeq = 17 + ready2Events.size
      val withHandicap2 =
        if (
          ready2Events.exists {
            case _: TournamentEvent.HandicapApplied => true
            case _                                  => false
          }
        ) {
          withReady2
        } else {
          val handicap2 =
            Tournament
              .applyHandicap(afterReady2, "wb-1-2", handicap = 2, nextSeq, at)
              .toOption
              .get
          withReady2 :+ handicap2
        }
      val afterHandicap2 = Replay.replay(withHandicap2).toOption.get
      val handicapSeq2 = withHandicap2.map(_.seq).max + 1
      val started2 =
        Tournament
          .start(afterHandicap2, "wb-1-2", handicapSeq2, at)
          .toOption
          .get
      val withStarted2 = withHandicap2 :+ started2
      val afterStart2 = Replay.replay(withStarted2).toOption.get
      val result2 =
        Tournament
          .recordResult(
            afterStart2,
            "wb-1-2",
            scoreA = 7,
            scoreB = 4,
            handicapSeq2 + 1,
            secondResultAt
          )
          .toOption
          .get
      val events = withStarted2 :+ result2
      val timing = timingOf(events, "wb-2-1")
      assertTrue(
        timing.waitStartedAt.contains(firstResultAt),
        timing.newPlayerRestSince.contains(secondResultAt)
      )
    },
    test("R1 both-seeded Ready has no newPlayerRestSince") {
      val state = seededState()
      val events =
        seededEvents(state) :+
          TournamentEvent.MatchReady(
            seq = 13,
            at = readyAt,
            payload =
              MatchReadyPayload(matchId = "wb-1-1", handicapSuggested = 2)
          )
      val timing = timingOf(events, "wb-1-1")
      assertTrue(timing.newPlayerRestSince.isEmpty)
    },
    test("forfeit feeder arrival uses MatchForfeit.at") {
      val base = seededState()
      val forfeitAt = at.plusSeconds(5000)
      val seeded = seededEvents(base)
      val forfeit =
        Tournament
          .recordForfeit(
            readyMatch(base),
            matchId = "wb-1-1",
            forfeitingSide = "A",
            reason = "no-show",
            seq = 13,
            at = forfeitAt
          )
          .toOption
          .get
      val events = seeded :+ forfeit
      val after = Replay.replay(events).toOption.get
      val winner = matchOf(readyMatch(base), "wb-1-1").playerB.get
      val advancedMatch =
        after.bracket.get.matches.find { matchDef =>
          matchDef.id != "wb-1-1" &&
          (matchDef.playerA.contains(winner) || matchDef.playerB.contains(
            winner
          ))
        }.get
      val timing = timingOf(events, advancedMatch.id)
      assertTrue(timing.waitStartedAt.contains(forfeitAt))
    },
    test("forfeit completion sets completedAt on forfeited match") {
      val base = seededState()
      val forfeitAt = at.plusSeconds(5000)
      val forfeit =
        Tournament
          .recordForfeit(
            readyMatch(base),
            matchId = "wb-1-1",
            forfeitingSide = "A",
            reason = "no-show",
            seq = 13,
            at = forfeitAt
          )
          .toOption
          .get
      val events = seededEvents(base) :+ forfeit
      val timing = timingOf(events, "wb-1-1")
      assertTrue(timing.completedAt.contains(forfeitAt))
    },
    test("structural bye at seed has no completedAt") {
      val ratings =
        List(rating("P1", 1700), rating("P2", 1690), rating("P3", 1680))
      val bracket = BracketGen.generate(ratings)
      val events = customSeededEvents(ratings, bracket)
      val byeTiming = timingOf(events, "wb-1-1")
      val propagatedTiming = timingOf(events, "wb-2-1")
      assertTrue(
        byeTiming.completedAt.isEmpty,
        propagatedTiming.waitStartedAt.contains(seedAt)
      )
    },
    test("score completion sets completedAt") {
      val base = seededState()
      val doneAt = at.plusSeconds(9000)
      val events =
        resultOnMatch(
          base,
          "wb-1-1",
          resultSeq = 13,
          resultAt = doneAt,
          scoreA = 7,
          scoreB = 4
        )
      val timing = timingOf(events, "wb-1-1")
      assertTrue(timing.completedAt.contains(doneAt))
    },
    test("MatchStarted sets startedAt to event at") {
      val base = seededState()
      val startedAt = at.plusSeconds(4000)
      val seeded = seededEvents(base)
      val afterSeed = Replay.replay(seeded).toOption.get
      val readyEvents =
        Tournament.ready(afterSeed, "wb-1-1", seq = 13, at).toOption.get
      val withReady = seeded ++ readyEvents
      val afterReady = Replay.replay(withReady).toOption.get
      val nextSeq = 13 + readyEvents.size
      val withHandicap =
        if (
          readyEvents.exists {
            case _: TournamentEvent.HandicapApplied => true
            case _                                  => false
          }
        ) {
          withReady
        } else {
          val handicap =
            Tournament
              .applyHandicap(afterReady, "wb-1-1", handicap = 2, nextSeq, at)
              .toOption
              .get
          withReady :+ handicap
        }
      val afterHandicap = Replay.replay(withHandicap).toOption.get
      val handicapSeq = withHandicap.map(_.seq).max + 1
      val started =
        Tournament
          .start(afterHandicap, "wb-1-1", handicapSeq, startedAt)
          .toOption
          .get
      val events = withHandicap :+ started
      val timing = timingOf(events, "wb-1-1")
      assertTrue(timing.startedAt.contains(startedAt))
    },
    test("never-started match has no startedAt") {
      val state = seededState()
      val events =
        seededEvents(state) :+
          TournamentEvent.MatchReady(
            seq = 13,
            at = readyAt,
            payload =
              MatchReadyPayload(matchId = "wb-1-1", handicapSuggested = 2)
          )
      val timing = timingOf(events, "wb-1-1")
      assertTrue(timing.startedAt.isEmpty)
    },
    test("completed match retains startedAt from MatchStarted") {
      val base = seededState()
      val doneAt = at.plusSeconds(9000)
      val events =
        resultOnMatch(
          base,
          "wb-1-1",
          resultSeq = 13,
          resultAt = doneAt,
          scoreA = 7,
          scoreB = 4
        )
      val timing = timingOf(events, "wb-1-1")
      assertTrue(
        timing.startedAt.contains(at),
        timing.completedAt.contains(doneAt),
        timing.waitStartedAt.contains(seedAt)
      )
    }
  )

  private def readyMatch(state: TournamentState): TournamentState =
    withMatch(state, "wb-1-1") {
      _.copy(
        state = BracketMatchState.Ready,
        handicapSuggested = Some(2)
      )
    }
}
