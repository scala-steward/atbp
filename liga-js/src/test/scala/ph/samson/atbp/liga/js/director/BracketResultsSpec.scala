package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.js.api.Models.*
import ph.samson.atbp.liga.model.PlayerRatingLabel
import zio.test.*

object BracketResultsSpec extends ZIOSpecDefault {

  private val alice = Player("Alice")
  private val bob = Player("Bob")
  private val carol = Player("Carol")

  private def completed(
      id: String,
      playerA: Option[Player],
      playerB: Option[Player],
      scoreA: Int,
      scoreB: Int
  ): BracketMatch =
    BracketMatch(
      id = id,
      playerA = playerA,
      playerB = playerB,
      state = BracketMatchState.Completed,
      result = Some(MatchResult(scoreA, scoreB))
    )

  private def forfeitCompleted(
      id: String,
      playerA: Option[Player],
      playerB: Option[Player],
      forfeitingSide: String
  ): BracketMatch =
    BracketMatch(
      id = id,
      playerA = playerA,
      playerB = playerB,
      state = BracketMatchState.Completed,
      forfeit = Some(MatchForfeitInfo(forfeitingSide, "no-show"))
    )

  private def byeCompleted(
      id: String,
      player: Player
  ): BracketMatch =
    BracketMatch(
      id = id,
      playerA = Some(player),
      playerB = None,
      state = BracketMatchState.Completed,
      result = Some(MatchResult(0, 0)),
      isBye = true
    )

  def spec = suite("BracketResults")(
    suite("lastParticipatingMatchId")(
      test("returns furthest non-bye match with a recorded result") {
        val matches = List(
          completed("wb-1-1", Some(alice), Some(bob), 7, 4),
          completed("wb-2-1", Some(alice), Some(carol), 7, 5)
        )
        assertTrue(
          BracketResults.lastParticipatingMatchId(
            "Alice",
            matches,
            bracketSize = 8
          ) == Some("wb-2-1")
        )
      },
      test("losers bracket progresses after winners for the same player") {
        val matches = List(
          completed("wb-3-1", Some(alice), Some(bob), 7, 4),
          completed("lb-1-1", Some(alice), Some(carol), 7, 6)
        )
        assertTrue(
          BracketResults.lastParticipatingMatchId(
            "Alice",
            matches,
            bracketSize = 8
          ) == Some("lb-1-1")
        )
      },
      test("grand final is furthest when present") {
        val matches = List(
          completed("lb-4-1", Some(alice), Some(bob), 7, 5),
          completed("gf-1", Some(alice), Some(carol), 7, 4)
        )
        assertTrue(
          BracketResults.lastParticipatingMatchId(
            "Alice",
            matches,
            bracketSize = 8
          ) == Some("gf-1")
        )
      },
      test("never selects bye matches even with placeholder result") {
        val matches = List(
          byeCompleted("wb-1-1", alice),
          completed("wb-2-1", Some(alice), Some(bob), 7, 4)
        )
        assertTrue(
          BracketResults.lastParticipatingMatchId(
            "Alice",
            matches,
            bracketSize = 8
          ) == Some("wb-2-1")
        )
      },
      test("returns None for bye-only appearances") {
        val matches = List(byeCompleted("wb-1-1", alice))
        assertTrue(
          BracketResults
            .lastParticipatingMatchId(
              "Alice",
              matches,
              bracketSize = 8
            )
            .isEmpty
        )
      },
      test("forfeit can be the selected last slot") {
        val matches = List(
          completed("wb-2-1", Some(alice), Some(bob), 7, 4),
          forfeitCompleted(
            "lb-2-1",
            Some(alice),
            Some(carol),
            forfeitingSide = "A"
          )
        )
        assertTrue(
          BracketResults.lastParticipatingMatchId(
            "Alice",
            matches,
            bracketSize = 8
          ) == Some("lb-2-1")
        )
      }
    ),
    suite("tournamentRecord")(
      test("counts scored wins and losses") {
        val matches = List(
          completed("wb-1-1", Some(alice), Some(bob), 7, 4),
          completed("wb-2-1", Some(alice), Some(carol), 4, 7)
        )
        assertTrue(BracketResults.tournamentRecord("Alice", matches) == (1, 1))
      },
      test("counts forfeit win and loss") {
        val matches = List(
          forfeitCompleted(
            "wb-1-1",
            Some(alice),
            Some(bob),
            forfeitingSide = "B"
          ),
          forfeitCompleted(
            "lb-1-1",
            Some(alice),
            Some(carol),
            forfeitingSide = "A"
          )
        )
        assertTrue(BracketResults.tournamentRecord("Alice", matches) == (1, 1))
      },
      test("byes never increment wins or losses") {
        val matches = List(
          byeCompleted("wb-1-1", alice),
          completed("wb-2-1", Some(alice), Some(bob), 7, 4)
        )
        assertTrue(BracketResults.tournamentRecord("Alice", matches) == (1, 0))
      },
      test("bye-only player has zero-zero record") {
        val matches = List(byeCompleted("wb-1-1", alice))
        assertTrue(BracketResults.tournamentRecord("Alice", matches) == (0, 0))
      }
    ),
    suite("resultsCellDisplay")(
      test("incomplete tournament skips results annotation") {
        assertTrue(
          BracketResults.resultsCellDisplay(
            completed = false,
            playerName = "Alice",
            matchId = "wb-2-1",
            matches = List(
              completed("wb-2-1", Some(alice), Some(bob), 7, 4)
            ),
            bracketSize = 8,
            frozenLabel = Some(PlayerRatingLabel.Rated(1500)),
            latestRating = Some(
              LatestRating(alice, rating = 1510.0, delta = 10.0)
            )
          ) == BracketResults.ResultsCellDisplay.Skip
        )
      },
      test("completed non-last slot skips annotation") {
        val matches = List(
          completed("wb-1-1", Some(alice), Some(bob), 7, 4),
          completed("wb-2-1", Some(alice), Some(carol), 7, 5)
        )
        assertTrue(
          BracketResults.resultsCellDisplay(
            completed = true,
            playerName = "Alice",
            matchId = "wb-1-1",
            matches = matches,
            bracketSize = 8,
            frozenLabel = Some(PlayerRatingLabel.Rated(1500)),
            latestRating = Some(
              LatestRating(alice, rating = 1510.0, delta = 10.0)
            )
          ) == BracketResults.ResultsCellDisplay.Skip
        )
      },
      test("completed last slot without latest-ratings entry skips") {
        val matches =
          List(completed("wb-2-1", Some(alice), Some(bob), 7, 4))
        assertTrue(
          BracketResults.resultsCellDisplay(
            completed = true,
            playerName = "Alice",
            matchId = "wb-2-1",
            matches = matches,
            bracketSize = 8,
            frozenLabel = Some(PlayerRatingLabel.Rated(1500)),
            latestRating = None
          ) == BracketResults.ResultsCellDisplay.Skip
        )
      },
      test("completed last slot veteran shows RatedDelta") {
        val matches =
          List(completed("wb-2-1", Some(alice), Some(bob), 7, 4))
        assertTrue(
          BracketResults.resultsCellDisplay(
            completed = true,
            playerName = "Alice",
            matchId = "wb-2-1",
            matches = matches,
            bracketSize = 8,
            frozenLabel = Some(PlayerRatingLabel.Rated(1543)),
            latestRating = Some(
              LatestRating(alice, rating = 1555.0, delta = 12.0)
            )
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
      test("completed last slot guest shows NewRating not unrated") {
        val matches =
          List(completed("wb-2-1", Some(alice), Some(bob), 7, 4))
        assertTrue(
          BracketResults.resultsCellDisplay(
            completed = true,
            playerName = "Alice",
            matchId = "wb-2-1",
            matches = matches,
            bracketSize = 8,
            frozenLabel = Some(PlayerRatingLabel.Unrated),
            latestRating = Some(
              LatestRating(alice, rating = 1342.0, delta = 0.0)
            )
          ) ==
            BracketResults.ResultsCellDisplay.Annotate(
              wins = 1,
              losses = 0,
              movement = BracketResults.RatingMovementDisplay.NewRating(
                post = 1342.0
              )
            )
        )
      }
    ),
    suite("resultsNameEmphasis")(
      test("incomplete tournament stays Live") {
        val matches =
          List(completed("wb-2-1", Some(alice), Some(bob), 7, 4))
        assertTrue(
          BracketResults.resultsNameEmphasis(
            completed = false,
            playerName = "Alice",
            matchId = "wb-2-1",
            matches = matches,
            bracketSize = 8
          ) == BracketResults.ResultsNameEmphasis.Live
        )
      },
      test("completed last participating match is Last") {
        val matches = List(
          completed("wb-1-1", Some(alice), Some(bob), 7, 4),
          completed("wb-2-1", Some(alice), Some(carol), 4, 7)
        )
        assertTrue(
          BracketResults.resultsNameEmphasis(
            completed = true,
            playerName = "Alice",
            matchId = "wb-2-1",
            matches = matches,
            bracketSize = 8
          ) == BracketResults.ResultsNameEmphasis.Last
        )
      },
      test("completed earlier participating match is Prior") {
        val matches = List(
          completed("wb-1-1", Some(alice), Some(bob), 7, 4),
          completed("wb-2-1", Some(alice), Some(carol), 4, 7)
        )
        assertTrue(
          BracketResults.resultsNameEmphasis(
            completed = true,
            playerName = "Alice",
            matchId = "wb-1-1",
            matches = matches,
            bracketSize = 8
          ) == BracketResults.ResultsNameEmphasis.Prior
        )
      },
      test("bye-only appearances stay Live") {
        val matches = List(byeCompleted("wb-1-1", alice))
        assertTrue(
          BracketResults.resultsNameEmphasis(
            completed = true,
            playerName = "Alice",
            matchId = "wb-1-1",
            matches = matches,
            bracketSize = 8
          ) == BracketResults.ResultsNameEmphasis.Live
        )
      }
    ),
    suite("nameClasses")(
      test("Last keeps winner chrome and adds results-last") {
        assertTrue(
          BracketResults.nameClasses(
            BracketResults.ResultsNameEmphasis.Last,
            isWinner = true
          ) == "match-winner results-last",
          BracketResults.nameClasses(
            BracketResults.ResultsNameEmphasis.Last,
            isWinner = false
          ) == "results-last"
        )
      },
      test("Prior dims but keeps winner chrome") {
        assertTrue(
          BracketResults.nameClasses(
            BracketResults.ResultsNameEmphasis.Prior,
            isWinner = true
          ) == "match-winner results-prior",
          BracketResults.nameClasses(
            BracketResults.ResultsNameEmphasis.Prior,
            isWinner = false
          ) == "results-prior"
        )
      },
      test("Live keeps existing winner chrome only") {
        assertTrue(
          BracketResults.nameClasses(
            BracketResults.ResultsNameEmphasis.Live,
            isWinner = true
          ) == "match-winner",
          BracketResults.nameClasses(
            BracketResults.ResultsNameEmphasis.Live,
            isWinner = false
          ) == ""
        )
      }
    ),
    suite("ratedDeltaCssClasses")(
      test("maps signed deltas to rating-up and rating-down") {
        assertTrue(
          BracketResults.ratedDeltaCssClasses(5.4) == List("rating-up"),
          BracketResults.ratedDeltaCssClasses(-3.6) == List("rating-down"),
          BracketResults.ratedDeltaCssClasses(0.0) == Nil
        )
      }
    ),
    suite("formatRatedDeltaLine")(
      test("reuses LatestRatingsView.formatDelta for delta text") {
        assertTrue(
          BracketResults.formatRatedDeltaLine(1543, 12.0) == "1543 +12",
          BracketResults.formatRatedDeltaLine(1500, -4.0) == "1500 -4",
          BracketResults.formatRatedDeltaLine(1500, 0.0) == "1500 +0"
        )
      }
    )
  )
}
