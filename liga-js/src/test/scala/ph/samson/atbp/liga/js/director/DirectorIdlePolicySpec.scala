package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.js.api.Models.*
import zio.test.*

object DirectorIdlePolicySpec extends ZIOSpecDefault {

  private val activeTournament = TournamentResponse(
    name = "Spring Open",
    players = Nil,
    completed = false,
    phase = "active",
    raceToByScope = Map.empty,
    bracket = Some(Bracket(size = 2, matches = Nil)),
    frozenRatings = Nil
  )

  private val idleTournament = activeTournament.copy(phase = "none")
  private val definingTournament = activeTournament.copy(phase = "defining")
  private val completedTournament = activeTournament.copy(
    phase = "completed",
    completed = true
  )
  private val emptyLatest = LatestRatingsResponse(Nil)

  def spec = suite("DirectorIdlePolicy")(
    test("requires latest ratings on none and completed phases") {
      assertTrue(
        DirectorIdlePolicy.needsLatestRatings(TournamentPhase.None),
        DirectorIdlePolicy.needsLatestRatings(TournamentPhase.Completed),
        !DirectorIdlePolicy.needsLatestRatings(TournamentPhase.Active),
        !DirectorIdlePolicy.needsLatestRatings(TournamentPhase.Defining)
      )
    },
    test("requires leaderboard on wizard phases only") {
      assertTrue(
        !DirectorIdlePolicy.needsLeaderboard(TournamentPhase.None),
        DirectorIdlePolicy.needsLeaderboard(TournamentPhase.Defining),
        DirectorIdlePolicy.needsLeaderboard(TournamentPhase.Locked),
        DirectorIdlePolicy.needsLeaderboard(TournamentPhase.RaceTo),
        !DirectorIdlePolicy.needsLeaderboard(TournamentPhase.Active),
        !DirectorIdlePolicy.needsLeaderboard(TournamentPhase.Completed)
      )
    },
    test(
      "active tournament yields Live without leaderboard or latest ratings"
    ) {
      assertTrue(
        DirectorIdlePolicy.view(
          maybeTournament = Some(activeTournament),
          maybeLatestRatings = None
        ) == DirectorIdlePolicy.View.Live(activeTournament)
      )
    },
    test("none phase waits for latest ratings then yields Idle payload") {
      assertTrue(
        DirectorIdlePolicy.view(
          maybeTournament = Some(idleTournament),
          maybeLatestRatings = None
        ) == DirectorIdlePolicy.View.LoadingLatestRatings,
        DirectorIdlePolicy.view(
          maybeTournament = Some(idleTournament),
          maybeLatestRatings = Some(emptyLatest)
        ) == DirectorIdlePolicy.View.Idle(emptyLatest)
      )
    },
    test("wizard phase yields Wizard without waiting on leaderboard") {
      assertTrue(
        DirectorIdlePolicy.view(
          maybeTournament = Some(definingTournament),
          maybeLatestRatings = None
        ) == DirectorIdlePolicy.View.Wizard(definingTournament)
      )
    },
    test("waits for tournament before anything else") {
      assertTrue(
        DirectorIdlePolicy.view(None, None) ==
          DirectorIdlePolicy.View.LoadingTournament
      )
    },
    test("completed tournament waits for latest ratings then yields Live") {
      assertTrue(
        DirectorIdlePolicy.view(
          maybeTournament = Some(completedTournament),
          maybeLatestRatings = None
        ) == DirectorIdlePolicy.View.LoadingLatestRatings,
        DirectorIdlePolicy.view(
          maybeTournament = Some(completedTournament),
          maybeLatestRatings = Some(emptyLatest)
        ) == DirectorIdlePolicy.View.Live(completedTournament)
      )
    },
    test(
      "tournament writes drop any prior latest-ratings feed so Complete cannot join idle data"
    ) {
      val idleFeed = LatestRatingsResponse(
        List(LatestRating(Player("Alice"), rating = 1500.0, delta = 5.0))
      )
      assertTrue(
        DirectorIdlePolicy
          .retainedLatestRatingsAfterWrite(
            TournamentPhase.Completed,
            Some(idleFeed)
          )
          .isEmpty,
        DirectorIdlePolicy
          .retainedLatestRatingsAfterWrite(
            TournamentPhase.Active,
            Some(idleFeed)
          )
          .isEmpty,
        DirectorIdlePolicy
          .retainedLatestRatingsAfterWrite(
            TournamentPhase.Defining,
            Some(idleFeed)
          )
          .isEmpty,
        DirectorIdlePolicy.view(
          maybeTournament = Some(completedTournament),
          maybeLatestRatings =
            DirectorIdlePolicy.retainedLatestRatingsAfterWrite(
              TournamentPhase.Completed,
              Some(idleFeed)
            )
        ) == DirectorIdlePolicy.View.LoadingLatestRatings
      )
    }
  )
}
