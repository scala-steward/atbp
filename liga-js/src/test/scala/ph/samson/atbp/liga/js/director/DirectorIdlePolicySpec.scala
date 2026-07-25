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
  private val emptyLeaderboard = LeaderboardResponse(Nil)
  private val emptyLatest = LatestRatingsResponse(Nil)

  def spec = suite("DirectorIdlePolicy")(
    test("requires latest ratings only on none phase") {
      assertTrue(
        DirectorIdlePolicy.needsLatestRatings(TournamentPhase.None),
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
          maybeLeaderboard = None,
          maybeLatestRatings = None
        ) == DirectorIdlePolicy.View.Live(activeTournament)
      )
    },
    test("none phase waits for latest ratings then yields Idle payload") {
      assertTrue(
        DirectorIdlePolicy.view(
          maybeTournament = Some(idleTournament),
          maybeLeaderboard = None,
          maybeLatestRatings = None
        ) == DirectorIdlePolicy.View.LoadingLatestRatings,
        DirectorIdlePolicy.view(
          maybeTournament = Some(idleTournament),
          maybeLeaderboard = None,
          maybeLatestRatings = Some(emptyLatest)
        ) == DirectorIdlePolicy.View.Idle(emptyLatest)
      )
    },
    test("wizard phase waits for leaderboard then yields Wizard payload") {
      assertTrue(
        DirectorIdlePolicy.view(
          maybeTournament = Some(definingTournament),
          maybeLeaderboard = None,
          maybeLatestRatings = None
        ) == DirectorIdlePolicy.View.LoadingLeaderboard,
        DirectorIdlePolicy.view(
          maybeTournament = Some(definingTournament),
          maybeLeaderboard = Some(emptyLeaderboard),
          maybeLatestRatings = None
        ) == DirectorIdlePolicy.View.Wizard(
          definingTournament,
          emptyLeaderboard
        )
      )
    },
    test("waits for tournament before anything else") {
      assertTrue(
        DirectorIdlePolicy.view(None, None, None) ==
          DirectorIdlePolicy.View.LoadingTournament
      )
    }
  )
}
