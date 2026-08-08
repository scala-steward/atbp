package ph.samson.atbp.liga.js.audience

import ph.samson.atbp.liga.js.api.Models.*
import zio.test.*

object AudienceIdlePolicySpec extends ZIOSpecDefault {

  private val activeTournament = TournamentResponse(
    name = "Spring Open",
    players = Nil,
    completed = false,
    phase = "active",
    topN = 2,
    raceToByScope = Map.empty,
    bracket = Some(Bracket(size = 2, matches = Nil)),
    frozenRatings = Nil
  )

  private val idleTournament = activeTournament.copy(phase = "none")
  private val definingTournament = activeTournament.copy(
    phase = "defining",
    bracket = None
  )
  private val completedTournament = activeTournament.copy(
    phase = "completed",
    completed = true
  )
  private val emptyLatest = LatestRatingsResponse(Nil)

  def spec = suite("AudienceIdlePolicy")(
    test("requires latest ratings on none and completed phases") {
      assertTrue(
        AudienceIdlePolicy.needsLatestRatings(TournamentPhase.None),
        AudienceIdlePolicy.needsLatestRatings(TournamentPhase.Completed),
        !AudienceIdlePolicy.needsLatestRatings(TournamentPhase.Active),
        !AudienceIdlePolicy.needsLatestRatings(TournamentPhase.Defining)
      )
    },
    test("active tournament renders without latest ratings loaded") {
      assertTrue(
        AudienceIdlePolicy.view(
          maybeTournament = Some(activeTournament),
          maybeLatestRatings = None
        ) == AudienceIdlePolicy.View.Bracket(activeTournament)
      )
    },
    test("none phase waits for latest ratings then yields Idle payload") {
      assertTrue(
        AudienceIdlePolicy.view(
          maybeTournament = Some(idleTournament),
          maybeLatestRatings = None
        ) == AudienceIdlePolicy.View.LoadingLatestRatings,
        AudienceIdlePolicy.view(
          maybeTournament = Some(idleTournament),
          maybeLatestRatings = Some(emptyLatest)
        ) == AudienceIdlePolicy.View.Idle(emptyLatest)
      )
    },
    test("setup phases yield Setup payload without latest ratings") {
      assertTrue(
        AudienceIdlePolicy.view(
          maybeTournament = Some(definingTournament),
          maybeLatestRatings = None
        ) == AudienceIdlePolicy.View.Setup(definingTournament)
      )
    },
    test("waits for tournament before anything else") {
      assertTrue(
        AudienceIdlePolicy.view(None, None) ==
          AudienceIdlePolicy.View.LoadingTournament
      )
    },
    test("failed latest-ratings attempt still yields empty feed for Ready") {
      val (feed, err) =
        AudienceIdlePolicy.idleLatestRatingsFeed(Left("network down"))
      assertTrue(
        feed == LatestRatingsResponse(Nil),
        err.contains("network down")
      )
    },
    test("successful latest-ratings attempt keeps rows and clears error") {
      val rows = LatestRatingsResponse(
        List(
          LatestRating(
            Player("alice"),
            rating = 1600.0,
            delta = 12.0
          )
        )
      )
      val (feed, err) = AudienceIdlePolicy.idleLatestRatingsFeed(Right(rows))
      assertTrue(feed == rows, err.isEmpty)
    },
    test("completed tournament waits for latest ratings then yields Bracket") {
      assertTrue(
        AudienceIdlePolicy.view(
          maybeTournament = Some(completedTournament),
          maybeLatestRatings = None
        ) == AudienceIdlePolicy.View.LoadingLatestRatings,
        AudienceIdlePolicy.view(
          maybeTournament = Some(completedTournament),
          maybeLatestRatings = Some(emptyLatest)
        ) == AudienceIdlePolicy.View.Bracket(completedTournament)
      )
    },
    test(
      "completed phase reuses cached latest-ratings; idle and first completed fetch"
    ) {
      assertTrue(
        AudienceIdlePolicy.shouldFetchLatestRatingsOnRefresh(
          TournamentPhase.None,
          cached = Some(emptyLatest)
        ),
        AudienceIdlePolicy.shouldFetchLatestRatingsOnRefresh(
          TournamentPhase.Completed,
          cached = None
        ),
        !AudienceIdlePolicy.shouldFetchLatestRatingsOnRefresh(
          TournamentPhase.Completed,
          cached = Some(emptyLatest)
        ),
        !AudienceIdlePolicy.shouldFetchLatestRatingsOnRefresh(
          TournamentPhase.Active,
          cached = None
        )
      )
    }
  )
}
