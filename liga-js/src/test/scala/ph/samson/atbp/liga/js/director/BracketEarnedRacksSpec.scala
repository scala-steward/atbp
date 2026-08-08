package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.js.api.Models.*
import ph.samson.atbp.liga.model.PlayerRatingLabel
import zio.test.*

object BracketEarnedRacksSpec extends ZIOSpecDefault {

  private val alice = Player("Alice")
  private val bob = Player("Bob")
  private val carol = Player("Carol")

  private def rated(
      player: Player,
      rating: Double
  ): PlayerRating =
    PlayerRating(player, rating, rd = 80, wins = 0, losses = 0)

  private def completed(
      id: String,
      playerA: Option[Player],
      playerB: Option[Player],
      scoreA: Int,
      scoreB: Int,
      handicapApplied: Option[Int]
  ): BracketMatch =
    BracketMatch(
      id = id,
      playerA = playerA,
      playerB = playerB,
      state = BracketMatchState.Completed,
      handicapApplied = handicapApplied,
      result = Some(MatchResult(scoreA, scoreB))
    )

  private def pending(
      id: String,
      playerA: Option[Player],
      playerB: Option[Player]
  ): BracketMatch =
    BracketMatch(
      id = id,
      playerA = playerA,
      playerB = playerB,
      state = BracketMatchState.Ready
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

  private def byeCompleted(id: String, player: Player): BracketMatch =
    BracketMatch(
      id = id,
      playerA = Some(player),
      playerB = None,
      state = BracketMatchState.Completed,
      result = Some(MatchResult(0, 0)),
      isBye = true
    )

  private val frozen = List(
    rated(alice, 1500),
    rated(bob, 1400),
    rated(carol, 1600)
  )

  def spec = suite("BracketEarnedRacks")(
    suite("earnedAtCell")(
      test("no completed matches hides earned racks") {
        val matches = List(
          pending("wb-1-1", Some(alice), Some(bob))
        )
        assertTrue(
          BracketEarnedRacks
            .earnedAtCell(
              "Alice",
              matches.head,
              matches,
              bracketSize = 8,
              frozenRatings = Map.empty
            )
            .isEmpty
        )
      },
      test("completed 7-4 without handicap shows earned racks on done row") {
        val matchDef =
          completed(
            "wb-1-1",
            Some(alice),
            Some(bob),
            scoreA = 7,
            scoreB = 4,
            handicapApplied = None
          )
        val matches = List(matchDef)
        val ratings = frozen
          .map(r =>
            ph.samson.atbp.liga.model.Player(r.player.name) ->
              ph.samson.atbp.liga.js.api.PlayerRatingConversions.toShared(r)
          )
          .toMap
        assertTrue(
          BracketEarnedRacks.earnedAtCell(
            "Alice",
            matchDef,
            matches,
            bracketSize = 8,
            ratings
          ) == Some((7, 4)),
          BracketEarnedRacks.earnedAtCell(
            "Bob",
            matchDef,
            matches,
            bracketSize = 8,
            ratings
          ) == Some((4, 7))
        )
      },
      test("pending row shows totals through last completed only") {
        val first =
          completed(
            "wb-1-1",
            Some(alice),
            Some(bob),
            scoreA = 7,
            scoreB = 4,
            handicapApplied = None
          )
        val second = pending("wb-2-1", Some(alice), Some(carol))
        val matches = List(first, second)
        val ratings = frozen
          .map(r =>
            ph.samson.atbp.liga.model.Player(r.player.name) ->
              ph.samson.atbp.liga.js.api.PlayerRatingConversions.toShared(r)
          )
          .toMap
        assertTrue(
          BracketEarnedRacks.earnedAtCell(
            "Alice",
            second,
            matches,
            bracketSize = 8,
            ratings
          ) == Some((7, 4))
        )
      },
      test("handicap strips weaker-side spots") {
        val matchDef = completed(
          "wb-1-1",
          Some(bob),
          Some(alice),
          scoreA = 5,
          scoreB = 7,
          handicapApplied = Some(2)
        )
        val matches = List(matchDef)
        val ratings = frozen
          .map(r =>
            ph.samson.atbp.liga.model.Player(r.player.name) ->
              ph.samson.atbp.liga.js.api.PlayerRatingConversions.toShared(r)
          )
          .toMap
        assertTrue(
          BracketEarnedRacks.earnedAtCell(
            "Bob",
            matchDef,
            matches,
            bracketSize = 8,
            ratings
          ) == Some((3, 7)),
          BracketEarnedRacks.earnedAtCell(
            "Alice",
            matchDef,
            matches,
            bracketSize = 8,
            ratings
          ) == Some((7, 3))
        )
      },
      test("bye or forfeit only hides earned racks") {
        val bye = byeCompleted("wb-1-1", alice)
        val forfeit = forfeitCompleted(
          "wb-2-1",
          Some(alice),
          Some(bob),
          forfeitingSide = "A"
        )
        val ratings = frozen
          .map(r =>
            ph.samson.atbp.liga.model.Player(r.player.name) ->
              ph.samson.atbp.liga.js.api.PlayerRatingConversions.toShared(r)
          )
          .toMap
        assertTrue(
          BracketEarnedRacks
            .earnedAtCell(
              "Alice",
              bye,
              List(bye),
              bracketSize = 8,
              ratings
            )
            .isEmpty,
          BracketEarnedRacks
            .earnedAtCell(
              "Alice",
              forfeit,
              List(forfeit),
              bracketSize = 8,
              ratings
            )
            .isEmpty
        )
      },
      test("second done row includes both matches; earlier row stays as-of") {
        val first =
          completed(
            "wb-1-1",
            Some(alice),
            Some(bob),
            scoreA = 7,
            scoreB = 4,
            handicapApplied = None
          )
        val second =
          completed(
            "wb-2-1",
            Some(alice),
            Some(carol),
            scoreA = 4,
            scoreB = 7,
            handicapApplied = None
          )
        val matches = List(first, second)
        val ratings = frozen
          .map(r =>
            ph.samson.atbp.liga.model.Player(r.player.name) ->
              ph.samson.atbp.liga.js.api.PlayerRatingConversions.toShared(r)
          )
          .toMap
        assertTrue(
          BracketEarnedRacks.earnedAtCell(
            "Alice",
            first,
            matches,
            bracketSize = 8,
            ratings
          ) == Some((7, 4)),
          BracketEarnedRacks.earnedAtCell(
            "Alice",
            second,
            matches,
            bracketSize = 8,
            ratings
          ) == Some((11, 11))
        )
      }
    ),
    suite("formatLiveRatingLine")(
      test("appends earned racks to rated and unrated labels") {
        assertTrue(
          BracketEarnedRacks.formatLiveRatingLine(
            PlayerRatingLabel.Rated(1543),
            Some((7, 4))
          ) == "1543 (7-4)",
          BracketEarnedRacks.formatLiveRatingLine(
            PlayerRatingLabel.Unrated,
            Some((2, 1))
          ) == "unrated (2-1)",
          BracketEarnedRacks.formatLiveRatingLine(
            PlayerRatingLabel.Rated(1543),
            None
          ) == "1543",
          BracketEarnedRacks.formatLiveRatingLine(
            PlayerRatingLabel.Unrated,
            None
          ) == "unrated"
        )
      }
    ),
    suite("appendEarnedToMovementLine")(
      test("appends earned racks after rating-movement text") {
        assertTrue(
          BracketEarnedRacks.appendEarnedToMovementLine(
            "1543 +12",
            Some((7, 4))
          ) == "1543 +12 (7-4)",
          BracketEarnedRacks.appendEarnedToMovementLine(
            "1500 (new)",
            Some((2, 1))
          ) == "1500 (new) (2-1)",
          BracketEarnedRacks.appendEarnedToMovementLine(
            "1543 +12",
            None
          ) == "1543 +12"
        )
      }
    )
  )
}
