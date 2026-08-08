package ph.samson.atbp.liga.tournament

import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.testsupport.RaceToTestSupport
import ph.samson.atbp.liga.testsupport.TournamentTestFixtures.*
import ph.samson.atbp.liga.tournament.events.TournamentEvent
import zio.test.*

object TournamentSpec extends ZIOSpecDefault {

  def spec = suite("Tournament")(
    suite("ready")(
      test("MatchReady computes handicap suggestion from frozen ratings") {
        val state = seededState()
        val result = Tournament.ready(state, "wb-1-1", seq = 3, at)
        val ready = result.toOption.get.collectFirst {
          case event: TournamentEvent.MatchReady => event
        }.get
        assertTrue(
          result.isRight,
          result.toOption.get.length == 1,
          ready.payload.matchId == "wb-1-1",
          ready.payload.handicapSuggested >= 0
        )
      },
      test("ready with one unrated participant suggests 0 and auto-applies") {
        val state = withUnratedFrozenRating(
          seededState(),
          matchOf(seededState(), "wb-1-1").playerA.get
        )
        val result = Tournament.ready(state, "wb-1-1", seq = 13, at)
        val events = result.toOption.get
        val ready = events.collectFirst {
          case event: TournamentEvent.MatchReady => event
        }.get
        val applied = events.collectFirst {
          case event: TournamentEvent.HandicapApplied => event
        }.get
        val afterReady =
          Replay.replay(seededEvents(state) ++ events).toOption.get
        val matchDef = matchOf(afterReady, "wb-1-1")
        assertTrue(
          result.isRight,
          events.length == 2,
          ready.payload.handicapSuggested == 0,
          applied.payload.handicapApplied == 0,
          matchDef.handicapSuggested.contains(0),
          matchDef.handicapApplied.contains(0)
        )
      },
      test("ready with both unrated participants suggests 0 and auto-applies") {
        val base = seededState()
        val matchDef = matchOf(base, "wb-1-1")
        val state = withUnratedFrozenRating(
          withUnratedFrozenRating(base, matchDef.playerA.get),
          matchDef.playerB.get
        )
        val result = Tournament.ready(state, "wb-1-1", seq = 13, at)
        val events = result.toOption.get
        val ready = events.collectFirst {
          case event: TournamentEvent.MatchReady => event
        }.get
        val applied = events.collectFirst {
          case event: TournamentEvent.HandicapApplied => event
        }.get
        assertTrue(
          result.isRight,
          events.length == 2,
          ready.payload.handicapSuggested == 0,
          applied.payload.handicapApplied == 0
        )
      },
      test("start succeeds after unrated ready without separate apply") {
        val state = withUnratedFrozenRating(
          seededState(),
          matchOf(seededState(), "wb-1-1").playerA.get
        )
        val seeded = seededEvents(state)
        val readyEvents =
          Tournament.ready(state, "wb-1-1", seq = 13, at).toOption.get
        val afterReady = Replay.replay(seeded ++ readyEvents).toOption.get
        val started =
          Tournament.start(afterReady, "wb-1-1", seq = 15, at).toOption.get
        val finalState =
          Replay.replay(seeded ++ readyEvents :+ started).toOption.get
        assertTrue(
          matchOf(finalState, "wb-1-1").state == BracketMatchState.Started
        )
      },
      test("ready rejects pending matches without both players") {
        val state = seededState()
        val pending =
          state.bracket.get.matches
            .find(_.state == BracketMatchState.Pending)
            .get
        assertTrue(Tournament.ready(state, pending.id, seq = 3, at).isLeft)
      },
      test("ready rejects started matches") {
        val state = withMatch(seededState(), "wb-1-1") {
          _.copy(
            state = BracketMatchState.Started,
            handicapSuggested = Some(2),
            handicapApplied = Some(2)
          )
        }
        assertTrue(Tournament.ready(state, "wb-1-1", seq = 3, at).isLeft)
      }
    ),
    suite("handicap")(
      test("HandicapApplied can differ from suggested") {
        val readyState = withMatch(seededState(), "wb-1-1") {
          _.copy(
            state = BracketMatchState.Ready,
            handicapSuggested = Some(2)
          )
        }
        val result =
          Tournament.applyHandicap(
            readyState,
            "wb-1-1",
            handicap = 3,
            seq = 4,
            at
          )
        assertTrue(
          result.isRight,
          result.toOption.get.payload.handicapApplied == 3
        )
      },
      test("handicap rejects values above race-to cap") {
        val readyState = withMatch(seededState(), "wb-1-1") {
          _.copy(
            state = BracketMatchState.Ready,
            handicapSuggested = Some(2)
          )
        }
        val result =
          Tournament.applyHandicap(
            readyState,
            "wb-1-1",
            handicap = 6,
            seq = 4,
            at
          )
        assertTrue(
          result.isLeft,
          result.left.toOption.get.message
            .contains("handicap must be at most 5")
        )
      },
      test("handicap rejects negative values") {
        val readyState = withMatch(seededState(), "wb-1-1") {
          _.copy(
            state = BracketMatchState.Ready,
            handicapSuggested = Some(2)
          )
        }
        assertTrue(
          Tournament
            .applyHandicap(readyState, "wb-1-1", handicap = -1, seq = 4, at)
            .isLeft
        )
      },
      test("handicap rejects started matches") {
        val state = withMatch(seededState(), "wb-1-1") {
          _.copy(
            state = BracketMatchState.Started,
            handicapSuggested = Some(2),
            handicapApplied = Some(2)
          )
        }
        assertTrue(
          Tournament
            .applyHandicap(state, "wb-1-1", handicap = 3, seq = 4, at)
            .isLeft
        )
      },
      test("handicap rejects non-zero when either player is unrated") {
        val state = withUnratedFrozenRating(
          withMatch(seededState(), "wb-1-1") {
            _.copy(
              state = BracketMatchState.Ready,
              handicapSuggested = Some(0)
            )
          },
          matchOf(seededState(), "wb-1-1").playerA.get
        )
        val result =
          Tournament.applyHandicap(state, "wb-1-1", handicap = 2, seq = 4, at)
        assertTrue(
          result.isLeft,
          result.left.toOption.get.message.contains("unrated")
        )
      },
      test("handicap allows zero when either player is unrated") {
        val state = withUnratedFrozenRating(
          withMatch(seededState(), "wb-1-1") {
            _.copy(
              state = BracketMatchState.Ready,
              handicapSuggested = Some(0)
            )
          },
          matchOf(seededState(), "wb-1-1").playerA.get
        )
        val result =
          Tournament.applyHandicap(state, "wb-1-1", handicap = 0, seq = 4, at)
        assertTrue(
          result.isRight,
          result.toOption.get.payload.handicapApplied == 0
        )
      }
    ),
    suite("start")(
      test("MatchStarted requires handicap to be applied first") {
        assertTrue(
          Tournament.start(seededState(), "wb-1-1", seq = 5, at).isLeft
        )
      },
      test("start succeeds after handicap applied") {
        val state = withMatch(seededState(), "wb-1-1") {
          _.copy(
            state = BracketMatchState.Ready,
            handicapSuggested = Some(2),
            handicapApplied = Some(2)
          )
        }
        assertTrue(Tournament.start(state, "wb-1-1", seq = 5, at).isRight)
      }
    ),
    suite("result")(
      test("MatchResult advances bracket and marks next matches ready") {
        val base = seededState()
        val state = withMatch(base, "wb-1-1") {
          _.copy(
            state = BracketMatchState.Started,
            handicapSuggested = Some(2),
            handicapApplied = Some(2)
          )
        }
        val folded = for {
          event <- Tournament.recordResult(
            state,
            "wb-1-1",
            scoreA = 7,
            scoreB = 4,
            seq = 16,
            at
          )
          next <- Replay.replay(
            seededEvents(base) :+
              TournamentEvent.MatchReady(
                seq = 13,
                at = at,
                payload =
                  MatchReadyPayload(matchId = "wb-1-1", handicapSuggested = 2)
              ) :+
              TournamentEvent.HandicapApplied(
                seq = 14,
                at = at,
                payload = HandicapAppliedPayload(
                  matchId = "wb-1-1",
                  handicapApplied = 2
                )
              ) :+
              TournamentEvent.MatchStarted(
                seq = 15,
                at = at,
                payload = MatchStartedPayload(matchId = "wb-1-1")
              ) :+
              event
          )
        } yield next
        val after = folded.toOption.get
        assertTrue(
          folded.isRight,
          matchOf(after, "wb-1-1").state == BracketMatchState.Completed,
          matchOf(after, "wb-2-1").playerA.contains(Player("P1"))
        )
      },
      test("result rejects winner score below race-to") {
        val state = withMatch(seededState(), "wb-1-1") {
          _.copy(
            state = BracketMatchState.Started,
            handicapSuggested = Some(2),
            handicapApplied = Some(2)
          )
        }
        assertTrue(
          Tournament
            .recordResult(state, "wb-1-1", scoreA = 6, scoreB = 4, seq = 6, at)
            .isLeft
        )
      },
      test("result rejects winner score above race-to") {
        val state = withMatch(seededState(), "wb-1-1") {
          _.copy(
            state = BracketMatchState.Started,
            handicapSuggested = Some(2),
            handicapApplied = Some(2)
          )
        }
        assertTrue(
          Tournament
            .recordResult(
              state,
              "wb-1-1",
              scoreA = 999,
              scoreB = 0,
              seq = 6,
              at
            )
            .isLeft
        )
      },
      test("result rejects tied scores at race-to") {
        val state = withMatch(seededState(), "wb-1-1") {
          _.copy(
            state = BracketMatchState.Started,
            handicapSuggested = Some(2),
            handicapApplied = Some(2)
          )
        }
        assertTrue(
          Tournament
            .recordResult(state, "wb-1-1", scoreA = 7, scoreB = 7, seq = 6, at)
            .isLeft
        )
      },
      test("result rejects matches that have not started") {
        val state = withMatch(seededState(), "wb-1-1") {
          _.copy(
            state = BracketMatchState.Ready,
            handicapSuggested = Some(2),
            handicapApplied = Some(2)
          )
        }
        assertTrue(
          Tournament
            .recordResult(state, "wb-1-1", scoreA = 7, scoreB = 4, seq = 6, at)
            .isLeft
        )
      }
    ),
    suite("full lifecycle")(
      test("ready → handicap → start → result replays cleanly") {
        val state = seededState()
        val seeded = seededEvents(state)
        val readyEvents =
          Tournament.ready(state, "wb-1-1", seq = 13, at).toOption.get
        val afterReady = Replay.replay(seeded ++ readyEvents).toOption.get
        val handicap =
          Tournament
            .applyHandicap(afterReady, "wb-1-1", handicap = 3, seq = 14, at)
            .toOption
            .get
        val afterHandicap =
          Replay.replay(seeded ++ readyEvents :+ handicap).toOption.get
        val started =
          Tournament.start(afterHandicap, "wb-1-1", seq = 15, at).toOption.get
        val afterStart =
          Replay
            .replay(seeded ++ readyEvents :+ handicap :+ started)
            .toOption
            .get
        val result = Tournament
          .recordResult(
            afterStart,
            "wb-1-1",
            scoreA = 7,
            scoreB = 4,
            seq = 16,
            at
          )
          .toOption
          .get
        val finalState =
          Replay
            .replay(seeded ++ readyEvents :+ handicap :+ started :+ result)
            .toOption
            .get
        val readyEvent = readyEvents.collectFirst {
          case event: TournamentEvent.MatchReady => event
        }.get
        val matchDef = matchOf(finalState, "wb-1-1")
        assertTrue(
          matchDef.state == BracketMatchState.Completed,
          matchDef.handicapSuggested.contains(
            readyEvent.payload.handicapSuggested
          ),
          matchDef.handicapApplied.contains(3),
          matchDef.result.contains(MatchResult(7, 4))
        )
      },
      test("illegal transitions fail during replay") {
        val state = seededState()
        assertTrue(
          Replay
            .replay(
              seededEvents(state) :+
                TournamentEvent.HandicapApplied(
                  seq = 9,
                  at = at,
                  payload = HandicapAppliedPayload(
                    matchId = "wb-1-1",
                    handicapApplied = 2
                  )
                )
            )
            .isLeft,
          Replay
            .replay(
              seededEvents(state) :+
                TournamentEvent.MatchStarted(
                  seq = 9,
                  at = at,
                  payload = MatchStartedPayload(matchId = "wb-1-1")
                )
            )
            .isLeft
        )
      }
    ),
    suite("wizard")(
      test("create produces TournamentCreated with empty players") {
        val result = Tournament.create("Spring Open", seq = 1, at)
        assertTrue(
          result.isRight,
          result.toOption.get.payload.name == "Spring Open",
          result.toOption.get.payload.players.isEmpty
        )
      },
      test("create rejects blank tournament name") {
        assertTrue(Tournament.create("  ", seq = 1, at).isLeft)
      },
      test("setPlayers rejects duplicate names") {
        val state = TournamentState(name = "Open", players = Nil)
        val players = List(Player("Alice"), Player("Alice"))
        val result = Tournament.setPlayers(state, players, seq = 2, at)
        assertTrue(
          result.isLeft,
          result.left.toOption.get.message.contains("duplicate player names")
        )
      },
      test("setPlayers allows names that differ only by case") {
        val state = TournamentState(name = "Open", players = Nil)
        val players = List(Player("Alice"), Player("alice"))
        assertTrue(
          Tournament.setPlayers(state, players, seq = 2, at).isRight
        )
      },
      test("setPlayers rejects when roster is locked") {
        val state = TournamentState(
          name = "Open",
          players = (1 to 8).map(i => Player(s"P$i")).toList,
          playersLocked = true
        )
        assertTrue(
          Tournament
            .setPlayers(state, List(Player("Guest")), seq = 2, at)
            .isLeft
        )
      },
      test("setPlayers rejects after bracket is seeded") {
        val state = seededState()
        assertTrue(
          Tournament
            .setPlayers(state, List(Player("Guest")), seq = 3, at)
            .isLeft
        )
      },
      test("lockPlayers rejects invalid player count") {
        val state = TournamentState(
          name = "Open",
          players = List(Player("Alice"), Player("Bob"))
        )
        assertTrue(Tournament.lockPlayers(state, seq = 2, at).isLeft)
      },
      test("lockPlayers succeeds with 3 players") {
        val state = TournamentState(
          name = "Open",
          players = List(Player("Alice"), Player("Bob"), Player("Carol"))
        )
        assertTrue(Tournament.lockPlayers(state, seq = 2, at).isRight)
      },
      test("lockPlayers succeeds with 8 players") {
        val state = TournamentState(
          name = "Open",
          players = (1 to 8).map(i => Player(s"P$i")).toList
        )
        assertTrue(Tournament.lockPlayers(state, seq = 2, at).isRight)
      },
      test("setFormat rejects incomplete scope map") {
        val state = TournamentState(
          name = "Open",
          players = (1 to 8).map(i => Player(s"P$i")).toList,
          playersLocked = true
        )
        val incomplete = RaceToTestSupport.uniformRaceTo(8) - "gf"
        val result =
          Tournament.setFormat(state, topN = 2, incomplete, seq = 3, at)
        assertTrue(
          result.isLeft,
          result.left.toOption.get.message
            .contains("every scope is configured")
        )
      },
      test("setFormat rejects illegal topN") {
        val state = TournamentState(
          name = "Open",
          players = (1 to 8).map(i => Player(s"P$i")).toList,
          playersLocked = true
        )
        val result =
          Tournament.setFormat(
            state,
            topN = 3,
            RaceToTestSupport.uniformRaceTo(8),
            seq = 3,
            at
          )
        assertTrue(
          result.isLeft,
          result.left.toOption.get.message.contains("topN must be legal")
        )
      },
      test("setFormat rejects race-to below 2") {
        val state = TournamentState(
          name = "Open",
          players = (1 to 8).map(i => Player(s"P$i")).toList,
          playersLocked = true
        )
        val invalid =
          RaceToTestSupport.uniformRaceTo(8).updated("wb-1", 1)
        val result =
          Tournament.setFormat(state, topN = 2, invalid, seq = 3, at)
        assertTrue(
          result.isLeft,
          result.left.toOption.get.message
            .contains("race-to must be at least 2")
        )
      },
      test("setFormat emits one FormatSet event") {
        val state = TournamentState(
          name = "Open",
          players = (1 to 8).map(i => Player(s"P$i")).toList,
          playersLocked = true
        )
        val raceToByScope =
          RaceToTestSupport.uniformRaceTo(8).updated("wb-2", 5)
        val result =
          Tournament.setFormat(state, topN = 2, raceToByScope, seq = 3, at)
        assertTrue(
          result.isRight,
          result.toOption.get.payload.topN == 2,
          result.toOption.get.payload.raceToByScope.get("wb-2").contains(5)
        )
      },
      test("setFormat rejects after bracket is seeded") {
        assertTrue(
          Tournament
            .setFormat(
              seededState(),
              topN = 2,
              RaceToTestSupport.uniformRaceTo(8),
              seq = 3,
              at
            )
            .isLeft
        )
      }
    ),
    suite("resolveRaceTo")(
      test("uses per-section values from raceToByScope") {
        val raceTo = RaceToTestSupport.differentiatedRaceTo(8)
        val state = seededState(raceTo)
        assertTrue(
          MatchLifecycle.resolveRaceTo(state, "wb-1-1") == Right(7),
          MatchLifecycle.resolveRaceTo(state, "lb-1-1") == Right(5),
          MatchLifecycle.resolveRaceTo(state, "gf-1") == Right(9)
        )
      }
    ),
    suite("result with differentiated race-to")(
      test("enforces losers-bracket race-to on result") {
        val state = withMatch(
          seededState(RaceToTestSupport.differentiatedRaceTo(8)),
          "lb-1-1"
        ) {
          _.copy(
            state = BracketMatchState.Started,
            handicapSuggested = Some(0),
            handicapApplied = Some(0)
          )
        }
        assertTrue(
          Tournament
            .recordResult(state, "lb-1-1", scoreA = 5, scoreB = 3, seq = 6, at)
            .isRight,
          Tournament
            .recordResult(state, "lb-1-1", scoreA = 7, scoreB = 3, seq = 6, at)
            .isLeft
        )
      }
    )
  )
}
