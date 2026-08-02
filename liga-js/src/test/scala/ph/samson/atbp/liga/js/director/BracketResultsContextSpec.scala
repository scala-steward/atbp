package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.js.api.Models.*
import ph.samson.atbp.liga.model.PlayerRatingLabel
import zio.test.*

object BracketResultsContextSpec extends ZIOSpecDefault {

  private val alice = Player("Alice")
  private val bob = Player("Bob")

  private val completedTournament = TournamentResponse(
    name = "Spring",
    players = List(alice, bob),
    completed = true,
    phase = "completed",
    raceToByScope = Map.empty,
    bracket = Some(
      Bracket(
        size = 8,
        matches = List(
          BracketMatch(
            id = "wb-2-1",
            playerA = Some(alice),
            playerB = Some(bob),
            state = BracketMatchState.Completed,
            result = Some(MatchResult(7, 4))
          )
        )
      )
    ),
    frozenRatings = List(
      PlayerRating(alice, 1543, 80, wins = 0, losses = 0)
    )
  )

  def spec = suite("BracketResultsContext")(
    test("joins latest-ratings by player name for annotated veterans") {
      val latest = LatestRatingsResponse(
        List(LatestRating(alice, rating = 1555.0, delta = 12.0))
      )
      val context =
        BracketResultsContext.fromTournament(completedTournament, latest)
      assertTrue(
        context.cellDisplay(
          "Alice",
          "wb-2-1",
          Some(PlayerRatingLabel.Rated(1543))
        ) ==
          BracketResults.ResultsCellDisplay.Annotate(
            wins = 1,
            losses = 0,
            movement = BracketResults.RatingMovementDisplay.RatedDelta(
              frozen = 1543,
              delta = 12.0
            )
          )
      )
    },
    test("players absent from latest-ratings are skipped") {
      val context = BracketResultsContext.fromTournament(
        completedTournament,
        LatestRatingsResponse(Nil)
      )
      assertTrue(
        context.cellDisplay(
          "Alice",
          "wb-2-1",
          Some(PlayerRatingLabel.Rated(1543))
        ) == BracketResults.ResultsCellDisplay.Skip
      )
    },
    test("inactive context skips annotations for live tournaments") {
      val live = completedTournament.copy(completed = false, phase = "active")
      val context = BracketResultsContext.inactive(live)
      assertTrue(
        context.cellDisplay(
          "Alice",
          "wb-2-1",
          Some(PlayerRatingLabel.Rated(1543))
        ) == BracketResults.ResultsCellDisplay.Skip
      )
    },
    test("precomputes last-slot and W-L indexes matching per-player helpers") {
      val carol = Player("Carol")
      val matches = List(
        BracketMatch(
          id = "wb-1-1",
          playerA = Some(alice),
          playerB = Some(bob),
          state = BracketMatchState.Completed,
          result = Some(MatchResult(7, 4))
        ),
        BracketMatch(
          id = "wb-2-1",
          playerA = Some(alice),
          playerB = Some(carol),
          state = BracketMatchState.Completed,
          result = Some(MatchResult(4, 7))
        )
      )
      val indexes = BracketResults.playerIndexes(matches, bracketSize = 8)
      assertTrue(
        indexes.lastMatchByPlayer.get("Alice") ==
          BracketResults.lastParticipatingMatchId("Alice", matches, 8),
        indexes.lastMatchByPlayer.get("Bob") ==
          BracketResults.lastParticipatingMatchId("Bob", matches, 8),
        indexes.recordByPlayer
          .get("Alice")
          .contains(
            BracketResults.tournamentRecord("Alice", matches)
          ),
        indexes.recordByPlayer
          .get("Bob")
          .contains(
            BracketResults.tournamentRecord("Bob", matches)
          )
      )
    }
  )
}
