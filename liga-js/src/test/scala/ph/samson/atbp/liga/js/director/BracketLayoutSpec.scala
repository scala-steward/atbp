package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.js.api.Models.*
import zio.test.*

object BracketLayoutSpec extends ZIOSpecDefault {

  def spec = suite("BracketLayout")(
    test("bracketRound for gf-1 matches server winners-round semantics") {
      assertTrue(
        BracketLayout.bracketRound("gf-1", bracketSize = 8) == Some(3),
        BracketLayout.bracketRound("gf-1", bracketSize = 16) == Some(4),
        BracketLayout.bracketRound("wb-2-1", bracketSize = 8) == Some(2),
        BracketLayout.bracketRound("lb-3-2", bracketSize = 8) == Some(3)
      )
    },
    test("roundOf delegates to bracketRound for grand final") {
      assertTrue(
        BracketLayout.roundOf("gf-1", bracketSize = 8) == 3,
        BracketLayout.roundOf("wb-1-1", bracketSize = 8) == 1
      )
    },
    test("groupLabel omits round number for grand final") {
      assertTrue(
        BracketLayout.groupLabel(BracketLayout.Section.GrandFinal, 3) ==
          "Grand Final",
        BracketLayout.groupLabel(BracketLayout.Section.Winners, 2) ==
          "Winners — round 2",
        BracketLayout.groupLabel(BracketLayout.Section.Losers, 4) ==
          "Losers — round 4"
      )
    },
    test("matchLabel delegates to groupLabel") {
      assertTrue(
        BracketLayout.matchLabel("gf-1", bracketSize = 8) == "Grand Final",
        BracketLayout.matchLabel("wb-2-1", bracketSize = 8) ==
          "Winners — round 2"
      )
    },
    test("groupMatches uses bracket size for grand final round grouping") {
      val gf = BracketMatch(
        id = "gf-1",
        playerA = Some(Player("P1")),
        playerB = Some(Player("P2")),
        state = BracketMatchState.Ready
      )
      val groups =
        BracketLayout.groupMatches(matches = List(gf), bracketSize = 8)
      assertTrue(
        groups.length == 1,
        groups.head.section == BracketLayout.Section.GrandFinal,
        groups.head.round == 3
      )
    },
    test("groupMatches omits empty Pending matches with neither player") {
      val hidden = BracketMatch(
        id = "wb-1-1",
        playerA = None,
        playerB = None,
        state = BracketMatchState.Pending
      )
      val shown = BracketMatch(
        id = "wb-1-2",
        playerA = Some(Player("P1")),
        playerB = None,
        state = BracketMatchState.Pending
      )
      val groups =
        BracketLayout.groupMatches(
          matches = List(hidden, shown),
          bracketSize = 8
        )
      assertTrue(
        groups.length == 1,
        groups.head.matches.map(_.id) == List("wb-1-2")
      )
    },
    test(
      "groupMatches sorts matches Ready then Pending then Started then Completed"
    ) {
      val player = Some(Player("P1"))
      val ready = BracketMatch(
        id = "wb-2-4",
        playerA = player,
        playerB = None,
        state = BracketMatchState.Ready
      )
      val pending = BracketMatch(
        id = "wb-2-1",
        playerA = player,
        playerB = None,
        state = BracketMatchState.Pending
      )
      val started =
        pending.copy(id = "wb-2-3", state = BracketMatchState.Started)
      val completed =
        pending.copy(id = "wb-2-2", state = BracketMatchState.Completed)
      val groups = BracketLayout.groupMatches(
        matches = List(completed, started, pending, ready),
        bracketSize = 8
      )
      assertTrue(
        groups.length == 1,
        groups.head.matches
          .map(_.id) == List("wb-2-4", "wb-2-1", "wb-2-3", "wb-2-2")
      )
    },
    test(
      "groupMatches keeps ascending id order within the same status bucket"
    ) {
      val player = Some(Player("P1"))
      val first = BracketMatch(
        id = "wb-2-1",
        playerA = player,
        playerB = None,
        state = BracketMatchState.Ready
      )
      val second = first.copy(id = "wb-2-2")
      val groups =
        BracketLayout.groupMatches(
          matches = List(second, first),
          bracketSize = 8
        )
      assertTrue(groups.head.matches.map(_.id) == List("wb-2-1", "wb-2-2"))
    },
    test(
      "groupMatches sorts by numeric match index within the same status bucket"
    ) {
      val player = Some(Player("P1"))
      val second = BracketMatch(
        id = "wb-1-2",
        playerA = player,
        playerB = Some(Player("P2")),
        state = BracketMatchState.Ready
      )
      val tenth = second.copy(id = "wb-1-10")
      val groups =
        BracketLayout.groupMatches(
          matches = List(tenth, second),
          bracketSize = 64
        )
      assertTrue(groups.head.matches.map(_.id) == List("wb-1-2", "wb-1-10"))
    },
    test("groupMatches places unfinished rounds above fully completed rounds") {
      val player = Some(Player("P1"))
      val unfinished = BracketMatch(
        id = "wb-2-1",
        playerA = player,
        playerB = Some(Player("P2")),
        state = BracketMatchState.Started
      )
      val finished = BracketMatch(
        id = "wb-3-1",
        playerA = player,
        playerB = Some(Player("P2")),
        state = BracketMatchState.Completed
      )
      val groups = BracketLayout.groupMatches(
        matches = List(finished, unfinished),
        bracketSize = 8
      )
      assertTrue(
        groups.map(g => (g.section, g.round)) ==
          List(
            (BracketLayout.Section.Winners, 2),
            (BracketLayout.Section.Winners, 3)
          )
      )
    },
    test(
      "groupMatches places unfinished band above section stack order"
    ) {
      val player = Some(Player("P1"))
      val opponent = Some(Player("P2"))
      val unfinishedWinners = BracketMatch(
        id = "wb-1-1",
        playerA = player,
        playerB = opponent,
        state = BracketMatchState.Ready
      )
      val finishedGrandFinal = BracketMatch(
        id = "gf-1",
        playerA = player,
        playerB = opponent,
        state = BracketMatchState.Completed
      )
      val groups = BracketLayout.groupMatches(
        matches = List(finishedGrandFinal, unfinishedWinners),
        bracketSize = 8
      )
      assertTrue(
        groups.map(g => (g.section, g.round)) ==
          List(
            (BracketLayout.Section.Winners, 1),
            (BracketLayout.Section.GrandFinal, 3)
          )
      )
    },
    test(
      "groupMatches stacks Grand Final above Losers above Winners in a band"
    ) {
      val player = Some(Player("P1"))
      val wb = BracketMatch(
        id = "wb-1-1",
        playerA = player,
        playerB = Some(Player("P2")),
        state = BracketMatchState.Ready
      )
      val lb = BracketMatch(
        id = "lb-1-1",
        playerA = player,
        playerB = Some(Player("P2")),
        state = BracketMatchState.Ready
      )
      val gf = BracketMatch(
        id = "gf-1",
        playerA = player,
        playerB = Some(Player("P2")),
        state = BracketMatchState.Ready
      )
      val groups =
        BracketLayout.groupMatches(matches = List(wb, lb, gf), bracketSize = 8)
      assertTrue(
        groups.map(_.section) == List(
          BracketLayout.Section.GrandFinal,
          BracketLayout.Section.Losers,
          BracketLayout.Section.Winners
        )
      )
    },
    test("groupMatches places later rounds above earlier rounds in a section") {
      val player = Some(Player("P1"))
      val round1 = BracketMatch(
        id = "wb-1-1",
        playerA = player,
        playerB = Some(Player("P2")),
        state = BracketMatchState.Ready
      )
      val round2 = round1.copy(id = "wb-2-1")
      val groups =
        BracketLayout.groupMatches(
          matches = List(round1, round2),
          bracketSize = 8
        )
      assertTrue(groups.map(_.round) == List(2, 1))
    },
    test("groupMatches omits groups where every match is hidden") {
      val hiddenOnly = BracketMatch(
        id = "wb-1-1",
        playerA = None,
        playerB = None,
        state = BracketMatchState.Pending
      )
      val groups =
        BracketLayout.groupMatches(matches = List(hiddenOnly), bracketSize = 8)
      assertTrue(groups.isEmpty)
    },
    test(
      "groupMatches keeps round unfinished when visible Completed plus hidden empty Pending"
    ) {
      val player = Some(Player("P1"))
      val opponent = Some(Player("P2"))
      val completedVisible = BracketMatch(
        id = "wb-2-1",
        playerA = player,
        playerB = opponent,
        state = BracketMatchState.Completed
      )
      val hiddenEmptyPending = BracketMatch(
        id = "wb-2-2",
        playerA = None,
        playerB = None,
        state = BracketMatchState.Pending
      )
      val finishedLater = BracketMatch(
        id = "wb-3-1",
        playerA = player,
        playerB = opponent,
        state = BracketMatchState.Completed
      )
      val groups = BracketLayout.groupMatches(
        matches = List(finishedLater, hiddenEmptyPending, completedVisible),
        bracketSize = 8
      )
      assertTrue(
        groups.length == 2,
        groups.map(g => (g.section, g.round)) ==
          List(
            (BracketLayout.Section.Winners, 2),
            (BracketLayout.Section.Winners, 3)
          ),
        groups.head.matches.map(_.id) == List("wb-2-1")
      )
    },
    test(
      "groupMatches treats rounds with only hidden empty Pendings as unfinished but omits them"
    ) {
      val hiddenOnly = BracketMatch(
        id = "wb-1-1",
        playerA = None,
        playerB = None,
        state = BracketMatchState.Pending
      )
      val visibleUnfinished = BracketMatch(
        id = "wb-2-1",
        playerA = Some(Player("P1")),
        playerB = Some(Player("P2")),
        state = BracketMatchState.Started
      )
      val finishedLater = BracketMatch(
        id = "wb-3-1",
        playerA = Some(Player("P1")),
        playerB = Some(Player("P2")),
        state = BracketMatchState.Completed
      )
      val groups = BracketLayout.groupMatches(
        matches = List(finishedLater, hiddenOnly, visibleUnfinished),
        bracketSize = 8
      )
      assertTrue(
        groups.length == 2,
        groups.map(g => (g.section, g.round)) ==
          List(
            (BracketLayout.Section.Winners, 2),
            (BracketLayout.Section.Winners, 3)
          )
      )
    },
    test("resultLabel shows bye for completed bye matches") {
      val byeMatch = BracketMatch(
        id = "wb-1-1",
        playerA = Some(Player("P1")),
        playerB = None,
        state = BracketMatchState.Completed,
        result = Some(MatchResult(scoreA = 1, scoreB = 0)),
        isBye = true
      )
      assertTrue(BracketLayout.resultLabel(byeMatch) == Some("bye"))
    },
    test("resultLabel shows score for normal completed matches") {
      val matchDef = BracketMatch(
        id = "wb-1-2",
        playerA = Some(Player("P2")),
        playerB = Some(Player("P3")),
        state = BracketMatchState.Completed,
        result = Some(MatchResult(scoreA = 7, scoreB = 4))
      )
      assertTrue(BracketLayout.resultLabel(matchDef) == Some("7–4"))
    },
    test("resultLabel shows forfeit reason for completed forfeits") {
      val matchDef = BracketMatch(
        id = "wb-1-1",
        playerA = Some(Player("P1")),
        playerB = Some(Player("P2")),
        state = BracketMatchState.Completed,
        forfeit =
          Some(MatchForfeitInfo(forfeitingSide = "A", reason = "no-show"))
      )
      assertTrue(
        BracketLayout.resultLabel(matchDef) == Some("forfeit: no-show")
      )
    },
    test("resultLabel is empty for pending matches") {
      val matchDef = BracketMatch(
        id = "wb-2-1",
        playerA = Some(Player("P1")),
        playerB = None,
        state = BracketMatchState.Pending
      )
      assertTrue(BracketLayout.resultLabel(matchDef) == None)
    },
    test("resultLabel ignores isBye on non-completed matches") {
      val flaggedPending = BracketMatch(
        id = "wb-2-1",
        playerA = Some(Player("P1")),
        playerB = None,
        state = BracketMatchState.Pending,
        isBye = true
      )
      assertTrue(BracketLayout.resultLabel(flaggedPending) == None)
    },
    test("resultLabel shows bye for ghost structural byes without a result") {
      val ghostBye = BracketMatch(
        id = "lb-1-2",
        playerA = None,
        playerB = None,
        state = BracketMatchState.Completed,
        isBye = true
      )
      assertTrue(BracketLayout.resultLabel(ghostBye) == Some("bye"))
    },
    test("raceToLabel formats Race to N") {
      assertTrue(
        BracketLayout.raceToLabel(7) == "Race to 7",
        BracketLayout.raceToLabel(9) == "Race to 9"
      )
    },
    test("resolveRoundRaceTo resolves mixed WB/LB/GF scopes") {
      val raceToByScope = Map(
        "wb-1" -> 7,
        "lb-2" -> 5,
        "gf" -> 9
      )
      assertTrue(
        BracketLayout.resolveRoundRaceTo(
          BracketLayout.Section.Winners,
          1,
          raceToByScope
        ) == Some(7),
        BracketLayout.resolveRoundRaceTo(
          BracketLayout.Section.Losers,
          2,
          raceToByScope
        ) == Some(5),
        BracketLayout.resolveRoundRaceTo(
          BracketLayout.Section.GrandFinal,
          3,
          raceToByScope
        ) == Some(9)
      )
    },
    test("resolveRoundRaceTo uses gf key for grand final not wb round number") {
      val raceToByScope = Map("gf" -> 9, "wb-3" -> 7)
      assertTrue(
        BracketLayout.resolveRoundRaceTo(
          BracketLayout.Section.GrandFinal,
          3,
          raceToByScope
        ) == Some(9)
      )
    },
    test("resolveRoundRaceTo returns None when scope is missing") {
      assertTrue(
        BracketLayout.resolveRoundRaceTo(
          BracketLayout.Section.Winners,
          2,
          Map("wb-1" -> 7)
        ) == None
      )
    },
    test("roundRaceToScope returns Right when scope resolves") {
      val raceToByScope = Map("wb-2" -> 7)
      assertTrue(
        BracketLayout.roundRaceToScope(
          BracketLayout.Section.Winners,
          2,
          raceToByScope
        ) == Right(7)
      )
    },
    test("roundRaceToScope returns Left scope key when unresolved") {
      val result = BracketLayout.roundRaceToScope(
        BracketLayout.Section.Losers,
        3,
        Map.empty
      )
      assertTrue(result == Left("lb-3"))
    },
    test(
      "matchRaceToScope returns Left scope key when race-to cannot resolve"
    ) {
      val matchDef = BracketMatch(
        id = "wb-2-1",
        playerA = Some(Player("P1")),
        playerB = Some(Player("P2")),
        state = BracketMatchState.Ready
      )
      assertTrue(
        BracketLayout.matchRaceToScope(matchDef, Map.empty) == Left("wb-2")
      )
    },
    test("resolveMatchRaceTo prefers matchDef.raceTo over scope default") {
      val matchDef = BracketMatch(
        id = "wb-1-1",
        playerA = Some(Player("P1")),
        playerB = Some(Player("P2")),
        state = BracketMatchState.Ready,
        raceTo = Some(11)
      )
      assertTrue(
        BracketLayout.resolveMatchRaceTo(matchDef, Map("wb-1" -> 7)) ==
          Some(11)
      )
    },
    test("resolveMatchRaceTo falls back to raceToByScope") {
      val matchDef = BracketMatch(
        id = "wb-1-1",
        playerA = Some(Player("P1")),
        playerB = Some(Player("P2")),
        state = BracketMatchState.Ready
      )
      assertTrue(
        BracketLayout.resolveMatchRaceTo(matchDef, Map("wb-1" -> 7)) ==
          Some(7)
      )
    },
    test("scopeRaceTo looks up race-to from scope map without defaults") {
      assertTrue(
        BracketLayout.scopeRaceTo("wb-2-1", Map("wb-2" -> 9)) == Some(9),
        BracketLayout.scopeRaceTo("wb-2-1", Map.empty).isEmpty
      )
    },
    test("winnerSide returns A when completed non-bye scoreA > scoreB") {
      val matchDef = completedMatch(scoreA = 7, scoreB = 4)
      assertTrue(
        BracketLayout.winnerSide(matchDef) == Some(
          BracketLayout.MatchWinnerSide.A
        )
      )
    },
    test("winnerSide returns B when completed non-bye scoreB > scoreA") {
      val matchDef = completedMatch(scoreA = 4, scoreB = 7)
      assertTrue(
        BracketLayout.winnerSide(matchDef) == Some(
          BracketLayout.MatchWinnerSide.B
        )
      )
    },
    test("winnerSide returns None for completed tie") {
      val matchDef = completedMatch(scoreA = 5, scoreB = 5)
      assertTrue(BracketLayout.winnerSide(matchDef) == None)
    },
    test("winnerSide returns None for pending matches") {
      val matchDef = BracketMatch(
        id = "wb-1-1",
        playerA = Some(Player("P1")),
        playerB = Some(Player("P2")),
        state = BracketMatchState.Pending
      )
      assertTrue(BracketLayout.winnerSide(matchDef) == None)
    },
    test("winnerSide returns None for started matches") {
      val matchDef = BracketMatch(
        id = "wb-1-1",
        playerA = Some(Player("P1")),
        playerB = Some(Player("P2")),
        state = BracketMatchState.Started,
        result = Some(MatchResult(scoreA = 3, scoreB = 1))
      )
      assertTrue(BracketLayout.winnerSide(matchDef) == None)
    },
    test("winnerSide returns None when completed but no result") {
      val matchDef = BracketMatch(
        id = "wb-1-1",
        playerA = Some(Player("P1")),
        playerB = Some(Player("P2")),
        state = BracketMatchState.Completed
      )
      assertTrue(BracketLayout.winnerSide(matchDef) == None)
    },
    test("winnerSide returns non-forfeiting side for completed forfeit") {
      val matchDef = BracketMatch(
        id = "wb-1-1",
        playerA = Some(Player("P1")),
        playerB = Some(Player("P2")),
        state = BracketMatchState.Completed,
        forfeit =
          Some(MatchForfeitInfo(forfeitingSide = "A", reason = "no-show"))
      )
      assertTrue(
        BracketLayout.winnerSide(matchDef) == Some(
          BracketLayout.MatchWinnerSide.B
        )
      )
    },
    test("winnerSide returns None for completed bye matches") {
      val byeMatch = BracketMatch(
        id = "wb-1-1",
        playerA = Some(Player("P1")),
        playerB = None,
        state = BracketMatchState.Completed,
        result = Some(MatchResult(scoreA = 1, scoreB = 0)),
        isBye = true
      )
      assertTrue(BracketLayout.winnerSide(byeMatch) == None)
    },
    suite("directorGroupMatches")(
      test("places all Ready matches in the strip sorted longest wait first") {
        val older = "2026-03-15T18:00:00Z"
        val newer = "2026-03-15T19:00:00Z"
        val readyLong =
          BracketMatch(
            id = "wb-1-1",
            playerA = Some(Player("P1")),
            playerB = Some(Player("P2")),
            state = BracketMatchState.Ready,
            waitStartedAt = Some(older)
          )
        val readyShort =
          readyLong.copy(id = "wb-1-2", waitStartedAt = Some(newer))
        val pending =
          readyLong.copy(
            id = "wb-2-1",
            state = BracketMatchState.Pending,
            waitStartedAt = Some(older)
          )
        val sections =
          BracketLayout.directorGroupMatches(
            matches = List(readyShort, readyLong, pending),
            bracketSize = 8
          )
        assertTrue(
          sections.liveStrip.isEmpty,
          sections.readyStrip.map(_.id) == List("wb-1-1", "wb-1-2"),
          sections.groups.flatMap(_.matches).exists(_.id == "wb-2-1"),
          !sections.groups
            .flatMap(_.matches)
            .exists(_.state == BracketMatchState.Ready)
        )
      },
      test("lower list matches groupMatches on non-Live non-Ready subset") {
        val ready =
          BracketMatch(
            id = "wb-1-1",
            playerA = Some(Player("P1")),
            playerB = Some(Player("P2")),
            state = BracketMatchState.Ready
          )
        val started =
          ready.copy(id = "wb-1-2", state = BracketMatchState.Started)
        val pending =
          ready.copy(id = "wb-2-1", state = BracketMatchState.Pending)
        val all = List(ready, started, pending)
        val sections = BracketLayout.directorGroupMatches(all, bracketSize = 8)
        val grouped =
          BracketLayout.groupMatches(List(pending), bracketSize = 8)
        assertTrue(
          sections.liveStrip.map(_.id) == List("wb-1-2"),
          sections.readyStrip.map(_.id) == List("wb-1-1"),
          sections.groups == grouped,
          !sections.groups
            .flatMap(_.matches)
            .exists(m =>
              m.state == BracketMatchState.Started ||
                m.state == BracketMatchState.Ready
            )
        )
      },
      test(
        "places all Started matches in live strip sorted by round then seed"
      ) {
        val laterRound =
          BracketMatch(
            id = "wb-2-1",
            playerA = Some(Player("P1")),
            playerB = Some(Player("P2")),
            state = BracketMatchState.Started
          )
        val higherSeed =
          laterRound.copy(id = "wb-1-2")
        val lowerSeed =
          laterRound.copy(id = "wb-1-1")
        val ready =
          laterRound.copy(id = "wb-1-3", state = BracketMatchState.Ready)
        val sections =
          BracketLayout.directorGroupMatches(
            matches = List(laterRound, higherSeed, lowerSeed, ready),
            bracketSize = 8
          )
        assertTrue(
          sections.liveStrip.map(_.id) == List("wb-1-1", "wb-1-2", "wb-2-1"),
          sections.readyStrip.map(_.id) == List("wb-1-3"),
          !sections.groups
            .flatMap(_.matches)
            .exists(_.state == BracketMatchState.Started)
        )
      },
      test("equal wait orders earlier rounds before later rounds") {
        val sameWait = "2026-03-15T18:00:00Z"
        val laterRound =
          BracketMatch(
            id = "wb-2-1",
            playerA = Some(Player("P1")),
            playerB = Some(Player("P2")),
            state = BracketMatchState.Ready,
            waitStartedAt = Some(sameWait)
          )
        val earlierRound =
          laterRound.copy(id = "wb-1-2")
        val sections =
          BracketLayout.directorGroupMatches(
            matches = List(laterRound, earlierRound),
            bracketSize = 8
          )
        assertTrue(
          sections.readyStrip.map(_.id) == List("wb-1-2", "wb-2-1")
        )
      },
      test("equal wait and round orders by seed index") {
        val sameWait = "2026-03-15T18:00:00Z"
        val higherSeed =
          BracketMatch(
            id = "wb-1-2",
            playerA = Some(Player("P1")),
            playerB = Some(Player("P2")),
            state = BracketMatchState.Ready,
            waitStartedAt = Some(sameWait)
          )
        val lowerSeed =
          higherSeed.copy(id = "wb-1-1")
        val sections =
          BracketLayout.directorGroupMatches(
            matches = List(higherSeed, lowerSeed),
            bracketSize = 8
          )
        assertTrue(
          sections.readyStrip.map(_.id) == List("wb-1-1", "wb-1-2")
        )
      },
      test("missing waitStartedAt sorts after known waits") {
        val knownWait = "2026-03-15T18:00:00Z"
        val withWait =
          BracketMatch(
            id = "wb-1-1",
            playerA = Some(Player("P1")),
            playerB = Some(Player("P2")),
            state = BracketMatchState.Ready,
            waitStartedAt = Some(knownWait)
          )
        val missingWait =
          withWait.copy(id = "wb-1-2", waitStartedAt = None)
        val sections =
          BracketLayout.directorGroupMatches(
            matches = List(missingWait, withWait),
            bracketSize = 8
          )
        assertTrue(
          sections.readyStrip.map(_.id) == List("wb-1-1", "wb-1-2")
        )
      }
    ),
    suite("elapsed formatting")(
      test("formatElapsedSeconds follows HH:mm floor rules") {
        assertTrue(
          BracketLayout.formatElapsedSeconds(0) == "00:00",
          BracketLayout.formatElapsedSeconds(59) == "00:00",
          BracketLayout.formatElapsedSeconds(60) == "00:01",
          BracketLayout.formatElapsedSeconds(3600) == "01:00"
        )
      },
      test(
        "formatElapsedSince omits malformed ISO and clamps negative elapsed"
      ) {
        val now = java.time.Instant.parse("2026-03-15T19:00:00Z")
        assertTrue(
          BracketLayout.formatElapsedSince("not-an-instant", now).isEmpty,
          BracketLayout
            .formatElapsedSince("2026-03-15T20:00:00Z", now)
            .contains("00:00")
        )
      },
      test("elapsedChipText omits malformed waitStartedAt") {
        val now = java.time.Instant.parse("2026-03-15T19:00:00Z")
        val ready =
          BracketMatch(
            id = "wb-1-1",
            playerA = Some(Player("P1")),
            playerB = Some(Player("P2")),
            state = BracketMatchState.Ready,
            waitStartedAt = Some("bogus")
          )
        assertTrue(BracketLayout.elapsedChipText(ready, now).isEmpty)
      },
      test("malformed waitStartedAt sorts after known waits") {
        val withWait =
          BracketMatch(
            id = "wb-1-1",
            playerA = Some(Player("P1")),
            playerB = Some(Player("P2")),
            state = BracketMatchState.Ready,
            waitStartedAt = Some("2026-03-15T18:00:00Z")
          )
        val badWait =
          withWait.copy(id = "wb-1-2", waitStartedAt = Some("bogus"))
        val sections =
          BracketLayout.directorGroupMatches(
            matches = List(badWait, withWait),
            bracketSize = 8
          )
        assertTrue(
          sections.readyStrip.map(_.id) == List("wb-1-1", "wb-1-2")
        )
      },
      test("doneChipText uses toLocaleTimeString like audience updated time") {
        val instant = "2026-03-15T18:34:00Z"
        val done =
          BracketMatch(
            id = "wb-1-1",
            playerA = Some(Player("P1")),
            playerB = Some(Player("P2")),
            state = BracketMatchState.Completed,
            completedAt = Some(instant)
          )
        assertTrue(
          BracketLayout.doneChipText(done) ==
            Some(DirectorTime.formatDoneTime(instant))
        )
      },
      test(
        "timingChipTexts shows wait and new-player rest for Ready"
      ) {
        val now = java.time.Instant.parse("2026-03-15T19:00:00Z")
        val ready =
          BracketMatch(
            id = "wb-2-1",
            playerA = Some(Player("P1")),
            playerB = Some(Player("P2")),
            state = BracketMatchState.Ready,
            waitStartedAt = Some("2026-03-15T18:00:00Z"),
            newPlayerRestSince = Some("2026-03-15T18:30:00Z")
          )
        val chips = BracketLayout.timingChipTexts(ready, now)
        assertTrue(
          chips.length == 2,
          chips.head == "01:00",
          chips(1) == "00:30"
        )
      },
      test(
        "elapsedChipText shows wait for Ready and cooling Pending-with-one"
      ) {
        val now = java.time.Instant.parse("2026-03-15T19:00:00Z")
        val ready =
          BracketMatch(
            id = "wb-1-1",
            playerA = Some(Player("P1")),
            playerB = Some(Player("P2")),
            state = BracketMatchState.Ready,
            waitStartedAt = Some("2026-03-15T18:00:00Z")
          )
        val cooling =
          BracketMatch(
            id = "wb-2-1",
            playerA = Some(Player("P3")),
            playerB = None,
            state = BracketMatchState.Pending,
            waitStartedAt = Some("2026-03-15T18:30:00Z")
          )
        assertTrue(
          BracketLayout.elapsedChipText(ready, now).contains("01:00"),
          BracketLayout.elapsedChipText(cooling, now).contains("00:30")
        )
      },
      test("timingChipTexts omits bye Done and missing Instants") {
        val now = java.time.Instant.parse("2026-03-15T19:00:00Z")
        val byeDone =
          BracketMatch(
            id = "wb-1-1",
            playerA = Some(Player("P1")),
            playerB = None,
            state = BracketMatchState.Completed,
            isBye = true,
            completedAt = Some("2026-03-15T18:00:00Z")
          )
        val noInstant =
          BracketMatch(
            id = "wb-1-2",
            playerA = Some(Player("P2")),
            playerB = Some(Player("P3")),
            state = BracketMatchState.Ready
          )
        val done =
          BracketMatch(
            id = "wb-1-3",
            playerA = Some(Player("P4")),
            playerB = Some(Player("P5")),
            state = BracketMatchState.Completed,
            completedAt = Some("2026-03-15T18:34:00Z")
          )
        assertTrue(
          BracketLayout.timingChipTexts(byeDone, now).isEmpty,
          BracketLayout.timingChipTexts(noInstant, now).isEmpty,
          BracketLayout.timingChipTexts(done, now).nonEmpty
        )
      },
      test("startedChipText uses formatDoneTime for Started matches") {
        val instant = "2026-03-15T18:34:00Z"
        val started =
          BracketMatch(
            id = "wb-1-1",
            playerA = Some(Player("P1")),
            playerB = Some(Player("P2")),
            state = BracketMatchState.Started,
            startedAt = Some(instant)
          )
        assertTrue(
          BracketLayout.startedChipText(started) ==
            Some(DirectorTime.formatDoneTime(instant))
        )
      },
      test("timingChipTexts shows started time for Started and omits missing") {
        val now = java.time.Instant.parse("2026-03-15T19:00:00Z")
        val instant = "2026-03-15T18:34:00Z"
        val started =
          BracketMatch(
            id = "wb-1-1",
            playerA = Some(Player("P1")),
            playerB = Some(Player("P2")),
            state = BracketMatchState.Started,
            startedAt = Some(instant)
          )
        val missingStartedAt =
          started.copy(id = "wb-1-2", startedAt = None)
        val malformedStartedAt =
          started.copy(id = "wb-1-3", startedAt = Some("bogus"))
        assertTrue(
          BracketLayout.timingChipTexts(started, now) ==
            List(DirectorTime.formatDoneTime(instant)),
          BracketLayout.timingChipTexts(missingStartedAt, now).isEmpty,
          BracketLayout.timingChipTexts(malformedStartedAt, now).isEmpty
        )
      }
    )
  )

  private def completedMatch(scoreA: Int, scoreB: Int): BracketMatch =
    BracketMatch(
      id = "wb-1-2",
      playerA = Some(Player("P1")),
      playerB = Some(Player("P2")),
      state = BracketMatchState.Completed,
      result = Some(MatchResult(scoreA = scoreA, scoreB = scoreB))
    )
}
