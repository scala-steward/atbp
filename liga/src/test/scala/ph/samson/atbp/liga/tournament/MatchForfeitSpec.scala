package ph.samson.atbp.liga.tournament

import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.testsupport.TournamentTestFixtures.*
import ph.samson.atbp.liga.tournament.events.TournamentEvent
import zio.test.*

object MatchForfeitSpec extends ZIOSpecDefault {

  private def readyMatch(state: TournamentState): TournamentState =
    withMatch(state, "wb-1-1") {
      _.copy(
        state = BracketMatchState.Ready,
        handicapSuggested = Some(2)
      )
    }

  private def startedMatch(state: TournamentState): TournamentState =
    withMatch(state, "wb-1-1") {
      _.copy(
        state = BracketMatchState.Started,
        handicapSuggested = Some(2),
        handicapApplied = Some(2)
      )
    }

  private def lifecycleToStarted(
      seeded: List[TournamentEvent],
      afterSeed: TournamentState
  ): (List[TournamentEvent], TournamentState) = {
    val readyEvents =
      Tournament.ready(afterSeed, "wb-1-1", seq = 13, at).toOption.get
    val afterReady = Replay.replay(seeded ++ readyEvents).toOption.get
    val handicap = Tournament
      .applyHandicap(afterReady, "wb-1-1", handicap = 2, seq = 14, at)
      .toOption
      .get
    val afterHandicap =
      Replay.replay(seeded ++ readyEvents :+ handicap).toOption.get
    val started =
      Tournament.start(afterHandicap, "wb-1-1", seq = 15, at).toOption.get
    val events = seeded ++ readyEvents :+ handicap :+ started
    val afterStart = Replay.replay(events).toOption.get
    (events, afterStart)
  }

  private val commandSpec = suite("MatchForfeit command")(
    test("recordForfeit accepts Ready match") {
      val state = readyMatch(seededState())
      val result = Tournament.recordForfeit(
        state,
        matchId = "wb-1-1",
        forfeitingSide = "A",
        reason = "no-show",
        seq = 20,
        at = at
      )
      assertTrue(
        result == Right(
          TournamentEvent.MatchForfeit(
            seq = 20,
            at = at,
            payload = MatchForfeitPayload(
              matchId = "wb-1-1",
              forfeitingSide = "A",
              reason = "no-show"
            )
          )
        ),
        matchOf(state, "wb-1-1").state == BracketMatchState.Ready
      )
    },
    test("recordForfeit accepts Started match") {
      val state = startedMatch(seededState())
      val result = Tournament.recordForfeit(
        state,
        matchId = "wb-1-1",
        forfeitingSide = "B",
        reason = "illness",
        seq = 21,
        at = at
      )
      assertTrue(result.isRight)
    },
    test("recordForfeit trims reason") {
      val state = readyMatch(seededState())
      val result = Tournament.recordForfeit(
        state,
        matchId = "wb-1-1",
        forfeitingSide = "A",
        reason = "  left early  ",
        seq = 22,
        at = at
      )
      assertTrue(result.toOption.get.payload.reason == "left early")
    },
    test("recordForfeit rejects Pending match") {
      val state = seededState()
      assertTrue(
        Tournament
          .recordForfeit(
            state,
            "wb-2-1",
            forfeitingSide = "A",
            reason = "no-show",
            seq = 23,
            at = at
          )
          .isLeft
      )
    },
    test("recordForfeit rejects Completed match") {
      val state = withMatch(seededState(), "wb-1-1") {
        _.copy(
          state = BracketMatchState.Completed,
          result = Some(MatchResult(7, 4))
        )
      }
      assertTrue(
        Tournament
          .recordForfeit(
            state,
            "wb-1-1",
            forfeitingSide = "A",
            reason = "no-show",
            seq = 24,
            at = at
          )
          .isLeft
      )
    },
    test("recordForfeit rejects blank and whitespace reason") {
      val state = readyMatch(seededState())
      assertTrue(
        Tournament
          .recordForfeit(state, "wb-1-1", "A", "", seq = 25, at = at)
          .isLeft,
        Tournament
          .recordForfeit(state, "wb-1-1", "A", "   ", seq = 26, at = at)
          .isLeft
      )
    },
    test("recordForfeit rejects invalid side") {
      val state = readyMatch(seededState())
      assertTrue(
        Tournament
          .recordForfeit(state, "wb-1-1", "C", "no-show", seq = 27, at = at)
          .isLeft
      )
    },
    test("recordForfeit rejects missing player") {
      val state = withMatch(readyMatch(seededState()), "wb-1-1") {
        _.copy(playerB = None)
      }
      assertTrue(
        Tournament
          .recordForfeit(state, "wb-1-1", "A", "no-show", seq = 28, at = at)
          .isLeft
      )
    },
    test("recordForfeit rejects completed tournament") {
      val state = readyMatch(seededState()).copy(completed = true)
      assertTrue(
        Tournament
          .recordForfeit(state, "wb-1-1", "A", "no-show", seq = 29, at = at)
          .isLeft
      )
    }
  )

  private val replaySpec = suite("MatchForfeit replay")(
    test(
      "replay applies Ready forfeit: clears result, sets forfeit, places loser"
    ) {
      val base = seededState()
      val before = matchOf(readyMatch(base), "wb-1-1")
      val playerA = before.playerA.get
      val playerB = before.playerB.get
      val seeded = seededEvents(base)
      val forfeit = Tournament
        .recordForfeit(
          readyMatch(base),
          "wb-1-1",
          forfeitingSide = "A",
          reason = "no-show",
          seq = 13,
          at = at
        )
        .toOption
        .get
      val after = Replay.replay(seeded :+ forfeit).toOption.get
      val completed = matchOf(after, "wb-1-1")
      val advanced = matchOf(after, "wb-2-1")
      val loserSlot = after.bracket.get.matches
        .filter(_.id.startsWith("lb-"))
        .find(m => m.playerA.contains(playerA) || m.playerB.contains(playerA))
      assertTrue(
        completed.state == BracketMatchState.Completed,
        completed.result.isEmpty,
        !completed.isBye,
        completed.forfeit.contains(
          MatchForfeitInfo(forfeitingSide = "A", reason = "no-show")
        ),
        advanced.playerA.contains(playerB) || advanced.playerB.contains(
          playerB
        ),
        loserSlot.isDefined
      )
    },
    test("replay applies Started forfeit the same way") {
      val base = seededState()
      val seeded = seededEvents(base)
      val afterSeed = Replay.replay(seeded).toOption.get
      val (events, afterStart) = lifecycleToStarted(seeded, afterSeed)
      val forfeitEvt = Tournament
        .recordForfeit(
          afterStart,
          "wb-1-1",
          forfeitingSide = "B",
          reason = "illness",
          seq = 16,
          at = at
        )
        .toOption
        .get
      val finalState = Replay.replay(events :+ forfeitEvt).toOption.get
      val completed = matchOf(finalState, "wb-1-1")
      assertTrue(
        completed.state == BracketMatchState.Completed,
        completed.result.isEmpty,
        !completed.isBye,
        completed.forfeit.contains(
          MatchForfeitInfo(forfeitingSide = "B", reason = "illness")
        )
      )
    },
    test("replay rejects forfeit on Completed match") {
      val base = seededState()
      val seeded = seededEvents(base)
      val afterSeed = Replay.replay(seeded).toOption.get
      val (events, afterStart) = lifecycleToStarted(seeded, afterSeed)
      val result = Tournament
        .recordResult(afterStart, "wb-1-1", 7, 4, seq = 16, at)
        .toOption
        .get
      val illegal = TournamentEvent.MatchForfeit(
        seq = 17,
        at = at,
        payload = MatchForfeitPayload("wb-1-1", "A", "too late")
      )
      assertTrue(
        Replay.replay(events :+ result :+ illegal).isLeft
      )
    }
  )

  def spec = suite("MatchForfeit")(commandSpec, replaySpec)
}
