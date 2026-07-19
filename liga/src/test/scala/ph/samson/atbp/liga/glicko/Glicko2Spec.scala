package ph.samson.atbp.liga.glicko

import dimos.glicko2.Player as GlickoPlayer
import dimos.glicko2.Result
import ph.samson.atbp.liga.model.*
import zio.test.*

import java.time.LocalDate
import scala.util.Random

object Glicko2Spec extends ZIOSpecDefault {

  private val alice = Player("Alice")
  private val bob = Player("Bob")
  private val carol = Player("Carol")

  private def approx(expected: Double, actual: Double): Boolean =
    Math.abs(expected - actual) <= 0.001

  private def singleMatchPeriod(periodMatch: PeriodMatch): Period =
    periodWithMatches(List(periodMatch))

  private def periodWithMatches(matches: List[PeriodMatch]): Period =
    Period(
      name = "test period",
      completed = LocalDate.parse("2026-01-01"),
      matches = matches
    )

  private def snapshotsEquivalent(
      left: Glicko2.Snapshot,
      right: Glicko2.Snapshot
  ): Boolean =
    left.keySet == right.keySet &&
      left.forall { case (player, rating) =>
        val other = Glicko2.ratingOf(right, player)
        approx(rating.rating, other.rating) &&
        approx(rating.rd, other.rd) &&
        rating.wins == other.wins &&
        rating.losses == other.losses
      }

  def spec = suite("Glicko2")(
    suite("Tuning")(
      test("default parameters match spec") {
        val t = Tuning.Default
        assertTrue(
          t.initRating == 1500,
          t.maxDeviation == 350,
          t.initVolatility == 0.06,
          t.tau == 0.5
        )
      }
    ),
    suite("library afterPeriod")(
      test("afterPeriod(Nil) keeps rating stable and increases RD") {
        val before = GlickoPlayer(1500, 200, 0.06)
        val after = before.afterPeriod(Nil, Tuning.Default)
        assertTrue(
          approx(1500, after.rating),
          after.deviation > before.deviation,
          after.volatility.isFinite
        )
      }
    ),
    suite("golden vectors")(
      test("Example 1 — win vs lower-rated established opponent") {
        val a = GlickoPlayer(1500, 350, 0.06)
        val b = GlickoPlayer(1400, 30, 0.06)
        val updated = a.afterPeriod(Seq(Result.WonAgainst(b)), Tuning.Default)
        assertTrue(
          approx(1631.369, updated.rating),
          approx(252.160, updated.deviation)
        )
      },
      test("Example 2 — win then loss in same rating period") {
        val a = GlickoPlayer(1631.368919949588, 252.16000623738702, 0.06)
        val c = GlickoPlayer(1550, 100, 0.06)
        val d = GlickoPlayer(1700, 300, 0.06)
        val updated = a.afterPeriod(
          Seq(Result.WonAgainst(c), Result.DefeatedBy(d)),
          Tuning.Default
        )
        assertTrue(
          approx(1644.372, updated.rating),
          approx(192.020, updated.deviation)
        )
      },
      test("wrapper updateAfterPeriod matches library for single game") {
        val state =
          Glicko2.snapshot(alice, rating = 1500, rd = 350) ++
            Glicko2.snapshot(bob, rating = 1400, rd = 30)
        val after = Glicko2.updateAfterPeriod(
          state,
          singleMatchPeriod(
            PeriodMatch(
              playerA = alice,
              playerB = bob,
              scoreA = 1,
              scoreB = 0,
              raceTo = 1,
              handicapSuggested = 0,
              handicapApplied = 0
            )
          )
        )
        val a = Glicko2.ratingOf(after, alice)
        assertTrue(
          approx(1631.369, a.rating),
          approx(252.160, a.rd)
        )
      }
    ),
    suite("updateAfterPeriod")(
      test(
        "7-4 single-match period expands to correct W-L and winner rated higher"
      ) {
        val periodMatch = PeriodMatch(
          playerA = alice,
          playerB = bob,
          scoreA = 7,
          scoreB = 4,
          raceTo = 7,
          handicapSuggested = 0,
          handicapApplied = 0
        )
        val after =
          Glicko2.updateAfterPeriod(
            Glicko2.empty,
            singleMatchPeriod(periodMatch)
          )
        val a = Glicko2.ratingOf(after, alice)
        val b = Glicko2.ratingOf(after, bob)
        assertTrue(
          a.wins == 7,
          a.losses == 4,
          b.wins == 4,
          b.losses == 7,
          a.rating > b.rating
        )
      },
      test("inactive prior players get afterPeriod(Nil) with unchanged W-L") {
        val periodOne = singleMatchPeriod(
          PeriodMatch(
            playerA = alice,
            playerB = bob,
            scoreA = 7,
            scoreB = 4,
            raceTo = 7,
            handicapSuggested = 0,
            handicapApplied = 0
          )
        )
        val afterPeriodOne = Glicko2.updateAfterPeriod(Glicko2.empty, periodOne)
        val bobAfterOne = Glicko2.ratingOf(afterPeriodOne, bob)

        val periodTwo = singleMatchPeriod(
          PeriodMatch(
            playerA = alice,
            playerB = carol,
            scoreA = 4,
            scoreB = 7,
            raceTo = 7,
            handicapSuggested = 0,
            handicapApplied = 0
          )
        )
        val afterPeriodTwo =
          Glicko2.updateAfterPeriod(afterPeriodOne, periodTwo)
        val bobAfterTwo = Glicko2.ratingOf(afterPeriodTwo, bob)

        assertTrue(
          bobAfterTwo.rd > bobAfterOne.rd,
          bobAfterTwo.wins == bobAfterOne.wins,
          bobAfterTwo.losses == bobAfterOne.losses
        )
      },
      test("multi-opponent period batches all matches into one update") {
        val period = periodWithMatches(
          List(
            PeriodMatch(
              playerA = alice,
              playerB = bob,
              scoreA = 7,
              scoreB = 4,
              raceTo = 7,
              handicapSuggested = 0,
              handicapApplied = 0
            ),
            PeriodMatch(
              playerA = alice,
              playerB = carol,
              scoreA = 4,
              scoreB = 7,
              raceTo = 7,
              handicapSuggested = 0,
              handicapApplied = 0
            )
          )
        )
        val after = Glicko2.updateAfterPeriod(Glicko2.empty, period)
        val a = Glicko2.ratingOf(after, alice)
        val b = Glicko2.ratingOf(after, bob)
        val c = Glicko2.ratingOf(after, carol)
        assertTrue(
          a.wins == 11,
          a.losses == 11,
          b.wins == 4,
          b.losses == 7,
          c.wins == 7,
          c.losses == 4
        )
      },
      test("rematch within period uses period-start opponent ratings") {
        val frozenBob = GlickoPlayer(1500, 350, 0.06)
        val frozenAlice = GlickoPlayer(1500, 350, 0.06)
        val period = periodWithMatches(
          List(
            PeriodMatch(
              playerA = alice,
              playerB = bob,
              scoreA = 7,
              scoreB = 4,
              raceTo = 7,
              handicapSuggested = 0,
              handicapApplied = 0
            ),
            PeriodMatch(
              playerA = alice,
              playerB = bob,
              scoreA = 4,
              scoreB = 7,
              raceTo = 7,
              handicapSuggested = 0,
              handicapApplied = 0
            )
          )
        )
        val games =
          ScoreExpansion.expandGames(7, 4) ++ ScoreExpansion.expandGames(4, 7)
        val aliceResults = games.map {
          case GameWinner.PlayerA => Result.WonAgainst(frozenBob)
          case GameWinner.PlayerB => Result.DefeatedBy(frozenBob)
        }
        val bobResults = games.map {
          case GameWinner.PlayerA => Result.DefeatedBy(frozenAlice)
          case GameWinner.PlayerB => Result.WonAgainst(frozenAlice)
        }
        val expectedAlice =
          frozenAlice.afterPeriod(aliceResults, Tuning.Default)
        val expectedBob = frozenBob.afterPeriod(bobResults, Tuning.Default)
        val after = Glicko2.updateAfterPeriod(Glicko2.empty, period)
        val actualAlice = Glicko2.ratingOf(after, alice)
        val actualBob = Glicko2.ratingOf(after, bob)
        assertTrue(
          approx(expectedAlice.rating, actualAlice.rating),
          approx(expectedAlice.deviation, actualAlice.rd),
          approx(expectedBob.rating, actualBob.rating),
          approx(expectedBob.deviation, actualBob.rd)
        )
      },
      test("rejects periods with zero matches") {
        val emptyPeriod = Period(
          name = "empty period",
          completed = LocalDate.parse("2026-01-01"),
          matches = Nil
        )
        val result =
          try {
            Right(Glicko2.updateAfterPeriod(Glicko2.empty, emptyPeriod))
          } catch {
            case error: IllegalArgumentException => Left(error)
          }
        assertTrue(
          result.isLeft,
          result.left.exists(_.getMessage.contains("zero matches"))
        )
      },
      test("reversed period.matches yields identical snapshot") {
        val matches = List(
          PeriodMatch(
            playerA = alice,
            playerB = bob,
            scoreA = 7,
            scoreB = 4,
            raceTo = 7,
            handicapSuggested = 0,
            handicapApplied = 0
          ),
          PeriodMatch(
            playerA = alice,
            playerB = carol,
            scoreA = 4,
            scoreB = 7,
            raceTo = 7,
            handicapSuggested = 0,
            handicapApplied = 0
          ),
          PeriodMatch(
            playerA = bob,
            playerB = carol,
            scoreA = 5,
            scoreB = 3,
            raceTo = 5,
            handicapSuggested = 0,
            handicapApplied = 0
          )
        )
        val baseline =
          Glicko2.updateAfterPeriod(Glicko2.empty, periodWithMatches(matches))
        val reversed =
          Glicko2.updateAfterPeriod(
            Glicko2.empty,
            periodWithMatches(matches.reverse)
          )
        assertTrue(snapshotsEquivalent(baseline, reversed))
      },
      test("shuffling period.matches yields identical snapshot") {
        val matches = List(
          PeriodMatch(
            playerA = alice,
            playerB = bob,
            scoreA = 7,
            scoreB = 4,
            raceTo = 7,
            handicapSuggested = 0,
            handicapApplied = 0
          ),
          PeriodMatch(
            playerA = alice,
            playerB = carol,
            scoreA = 4,
            scoreB = 7,
            raceTo = 7,
            handicapSuggested = 0,
            handicapApplied = 0
          ),
          PeriodMatch(
            playerA = bob,
            playerB = carol,
            scoreA = 5,
            scoreB = 3,
            raceTo = 5,
            handicapSuggested = 0,
            handicapApplied = 0
          )
        )
        val baseline =
          Glicko2.updateAfterPeriod(Glicko2.empty, periodWithMatches(matches))
        val shuffled =
          Glicko2.updateAfterPeriod(
            Glicko2.empty,
            periodWithMatches(Random.shuffle(matches))
          )
        assertTrue(snapshotsEquivalent(baseline, shuffled))
      }
    ),
    suite("leaderboard")(
      test("returns players sorted by name for deterministic output") {
        val state = Map(
          bob -> PlayerRating(bob, 1600, 100, 5, 3),
          alice -> PlayerRating(alice, 1700, 90, 8, 2)
        )
        val board = Glicko2.leaderboard(state)
        assertTrue(
          board.map(_.player.name) == List("Alice", "Bob")
        )
      }
    ),
    suite("properties")(
      test("RD decreases or stays after games played") {
        val periodMatch = PeriodMatch(
          playerA = alice,
          playerB = bob,
          scoreA = 5,
          scoreB = 3,
          raceTo = 5,
          handicapSuggested = 0,
          handicapApplied = 0
        )
        val before = Glicko2.newPlayerRating(alice)
        val after = Glicko2.ratingOf(
          Glicko2.updateAfterPeriod(
            Glicko2.empty,
            singleMatchPeriod(periodMatch)
          ),
          alice
        )
        assertTrue(after.rd <= before.rd)
      },
      test("ratings remain bounded for extreme rating gaps") {
        val state =
          Glicko2.snapshot(alice, rating = 1500, rd = 350) ++
            Glicko2.snapshot(bob, rating = 5000, rd = 50)
        val after = Glicko2.updateAfterPeriod(
          state,
          singleMatchPeriod(
            PeriodMatch(
              playerA = alice,
              playerB = bob,
              scoreA = 1,
              scoreB = 0,
              raceTo = 1,
              handicapSuggested = 0,
              handicapApplied = 0
            )
          )
        )
        val a = Glicko2.ratingOf(after, alice)
        val b = Glicko2.ratingOf(after, bob)
        assertTrue(
          a.rating.isFinite,
          b.rating.isFinite,
          a.rating > 1500,
          b.rating < 5000,
          a.rd >= 0,
          b.rd >= 0
        )
      }
    )
  )
}
