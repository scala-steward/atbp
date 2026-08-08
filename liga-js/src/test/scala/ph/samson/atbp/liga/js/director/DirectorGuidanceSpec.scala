package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.bracket.TournamentBounds
import ph.samson.atbp.liga.glicko.Tuning
import ph.samson.atbp.liga.js.api.Models.*
import zio.test.*

object DirectorGuidanceSpec extends ZIOSpecDefault {

  def spec = suite("DirectorGuidance")(
    suite("lockRosterHint")(
      test("warns when roster is below minimum") {
        assertTrue(
          DirectorGuidance.lockRosterHint(2) ==
            s"Need at least ${TournamentBounds.MinPlayers} players to lock (currently 2)."
        )
      },
      test("returns empty hint for valid roster size") {
        assertTrue(
          DirectorGuidance.lockRosterHint(3).isEmpty,
          DirectorGuidance.lockRosterHint(64).isEmpty
        )
      },
      test("warns when roster exceeds maximum") {
        assertTrue(
          DirectorGuidance.lockRosterHint(65) ==
            s"At most ${TournamentBounds.MaxPlayers} players allowed (currently 65)."
        )
      }
    ),
    test("applyPasteHint tells director to Apply paste, not Lock") {
      assertTrue(
        DirectorGuidance.applyPasteHint.contains("Apply paste"),
        !DirectorGuidance.applyPasteHint.toLowerCase.contains("lock")
      )
    },
    suite("define roster hint placement")(
      test("summary keeps below-min lock hint when paste is dirty") {
        val hints = DirectorGuidance.defineSummaryHints(
          activeCount = 0,
          pasteDirty = true
        )
        assertTrue(
          hints.exists(_.contains("Need at least")),
          !hints.contains(DirectorGuidance.lockSavesHint)
        )
      },
      test("summary includes lock-saves only when paste is clean") {
        assertTrue(
          DirectorGuidance
            .defineSummaryHints(activeCount = 4, pasteDirty = false)
            .contains(DirectorGuidance.lockSavesHint),
          !DirectorGuidance
            .defineSummaryHints(activeCount = 4, pasteDirty = true)
            .contains(DirectorGuidance.lockSavesHint)
        )
      },
      test("paste area only shows Apply hint when dirty") {
        assertTrue(
          DirectorGuidance.definePasteAreaHints(pasteDirty = true) ==
            List(DirectorGuidance.applyPasteHint),
          DirectorGuidance.definePasteAreaHints(pasteDirty = false).isEmpty
        )
      }
    ),
    test("friendlyApiError translates player count validation") {
      assertTrue(
        DirectorGuidance.friendlyApiError(
          TournamentBounds.invalidPlayerCountMessage(2)
        ) ==
          s"Roster must have ${TournamentBounds.MinPlayers}–${TournamentBounds.MaxPlayers} players"
      )
    },
    test("matchStepHint omits workflow steps when race-to is unresolved") {
      val matchDef = BracketMatch(
        id = "wb-2-1",
        playerA = Some(Player("P1")),
        playerB = Some(Player("P2")),
        state = BracketMatchState.Ready
      )
      val hint = DirectorGuidance.matchStepHint(
        matchDef,
        resolvedRaceTo = None,
        frozenRatings = Nil
      )
      assertTrue(
        !hint.contains("click Ready"),
        !hint.contains("Step 1"),
        hint.contains("file a bug"),
        hint.contains("wb-2")
      )
    },
    test("missingRaceToHint for match id uses bug-filing copy") {
      val hint = DirectorGuidance.missingRaceToHint("wb-2-1")
      assertTrue(
        hint.contains("file a bug"),
        hint.contains("wb-2")
      )
    },
    test("matchStepHint keeps workflow steps when race-to resolves") {
      val matchDef = BracketMatch(
        id = "wb-2-1",
        playerA = Some(Player("P1")),
        playerB = Some(Player("P2")),
        state = BracketMatchState.Ready
      )
      assertTrue(
        DirectorGuidance
          .matchStepHint(
            matchDef,
            resolvedRaceTo = Some(7),
            frozenRatings = Nil
          )
          .contains("Step 1")
      )
    },
    test("missingRaceToBugHint tells user to file a bug") {
      val hint = DirectorGuidance.missingRaceToBugHint("wb-2")
      assertTrue(
        hint.contains("file a bug"),
        hint.contains("wb-2")
      )
    },
    test("scoreboardScoreHint uses the given race-to in its example") {
      val hint = DirectorGuidance.scoreboardScoreHint(raceTo = 9)
      assertTrue(
        hint.contains("9"),
        !hint.contains("7–5")
      )
    },
    test("friendlyApiError includes cap and race-to for handicap errors") {
      val friendly = DirectorGuidance.friendlyApiError(
        "handicap must be at most 5 for race-to 7"
      )
      assertTrue(
        friendly.contains("5"),
        friendly.contains("Race to 7")
      )
    },
    test("friendlyApiError includes race-to for winner score errors") {
      val friendly = DirectorGuidance.friendlyApiError(
        "winner score must be 9"
      )
      assertTrue(
        friendly.contains("9"),
        friendly.contains("Race to 9")
      )
    },
    test("friendlyApiError includes race-to for loser score errors") {
      val friendly = DirectorGuidance.friendlyApiError(
        "loser score must be less than 9"
      )
      assertTrue(
        friendly.contains("9"),
        friendly.contains("Race to 9")
      )
    },
    test("friendlyApiErrorForSelectedMatch uses selected match race-to") {
      val matchDef = BracketMatch(
        id = "wb-1-1",
        playerA = Some(Player("P1")),
        playerB = Some(Player("P2")),
        state = BracketMatchState.Started
      )
      val tournament = TournamentResponse(
        name = "T",
        players = Nil,
        completed = false,
        phase = "active",
        topN = 2,
        raceToByScope = Map("wb-1" -> 9),
        bracket = Some(Bracket(size = 8, matches = List(matchDef))),
        frozenRatings = Nil
      )
      val friendly = DirectorGuidance.friendlyApiErrorForSelectedMatch(
        "winner score must be",
        Some(tournament),
        Some("wb-1-1")
      )
      assertTrue(
        friendly.contains("9"),
        friendly.contains("Race to 9")
      )
    },
    test("friendlyApiError uses optional raceTo when raw omits numbers") {
      val friendly = DirectorGuidance.friendlyApiError(
        "winner score must be",
        raceTo = Some(11)
      )
      assertTrue(
        friendly.contains("11"),
        friendly.contains("Race to 11")
      )
    },
    test(
      "friendlyApiError prefers race-to parsed from raw over client context"
    ) {
      val winner = DirectorGuidance.friendlyApiError(
        "winner score must be 9",
        raceTo = Some(5)
      )
      val loser = DirectorGuidance.friendlyApiError(
        "loser score must be less than 9",
        raceTo = Some(5)
      )
      val handicap = DirectorGuidance.friendlyApiError(
        "handicap must be at most 3 for race-to 9",
        raceTo = Some(5)
      )
      assertTrue(
        winner.contains("9"),
        !winner.contains("Race to 5"),
        loser.contains("9"),
        !loser.contains("Race to 5"),
        handicap.contains("Race to 9"),
        !handicap.contains("Race to 5")
      )
    },
    test("matchStepHint uses 3 steps for unrated matches") {
      val tuning = Tuning.Default
      val guest = Player("Guest")
      val bob = Player("Bob")
      val frozen = List(
        PlayerRating(
          guest,
          tuning.initRating,
          tuning.maxDeviation,
          wins = 0,
          losses = 0
        ),
        PlayerRating(bob, 1450, 90, wins = 0, losses = 0)
      )
      val beforeReady = BracketMatch(
        id = "wb-1-1",
        playerA = Some(guest),
        playerB = Some(bob),
        state = BracketMatchState.Ready
      )
      val afterReady = beforeReady.copy(
        handicapSuggested = Some(0),
        handicapApplied = Some(0)
      )
      val started = afterReady.copy(state = BracketMatchState.Started)
      val beforeHint =
        DirectorGuidance.matchStepHint(beforeReady, Some(7), frozen)
      val afterHint =
        DirectorGuidance.matchStepHint(afterReady, Some(7), frozen)
      val startedHint =
        DirectorGuidance.matchStepHint(started, Some(7), frozen)
      assertTrue(
        beforeHint.contains("Step 1 of 3"),
        afterHint.contains("Step 2 of 3"),
        afterHint.contains("Start match"),
        afterHint.contains("unrated"),
        !afterHint.contains("Apply handicap"),
        !afterHint.contains("of 4"),
        startedHint.contains("Step 3 of 3"),
        !startedHint.contains("of 4")
      )
    },
    test("matchStepHint still requires Apply for rated ready matches") {
      val matchDef = BracketMatch(
        id = "wb-1-1",
        playerA = Some(Player("Alice")),
        playerB = Some(Player("Bob")),
        state = BracketMatchState.Ready,
        handicapSuggested = Some(2)
      )
      val frozen = List(
        PlayerRating(Player("Alice"), 1700, 80, wins = 0, losses = 0),
        PlayerRating(Player("Bob"), 1450, 90, wins = 0, losses = 0)
      )
      val hint =
        DirectorGuidance.matchStepHint(matchDef, Some(7), frozen)
      assertTrue(hint.contains("Apply handicap"))
    },
    test("friendlyApiError translates unrated non-zero handicap rejection") {
      assertTrue(
        DirectorGuidance
          .friendlyApiError(
            "handicap must be 0 when either player is unrated"
          )
          .contains("stay at 0")
      )
    },
    test("matchWorkflowOverview does not require Apply for every match") {
      assertTrue(
        DirectorGuidance.matchWorkflowOverview.contains(
          "when both players are rated"
        ),
        !DirectorGuidance.matchWorkflowOverview.startsWith(
          "Each match: Ready (compute spot) → Apply handicap →"
        )
      )
    }
  )
}
