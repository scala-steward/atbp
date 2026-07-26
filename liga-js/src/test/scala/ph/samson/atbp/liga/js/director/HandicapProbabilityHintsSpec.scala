package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.handicap.WinProbability
import ph.samson.atbp.liga.model.*
import zio.test.*

object HandicapProbabilityHintsSpec extends ZIOSpecDefault {

  private val alice = Player("Alice")
  private val bob = Player("Bob")

  private def rating(player: Player, rating: Double, rd: Double): PlayerRating =
    PlayerRating(player, rating, rd, wins = 0, losses = 0)

  private val weaker = rating(bob, 1450, rd = 90)
  private val stronger = rating(alice, 1700, rd = 80)
  private val raceTo = 7

  def spec = suite("HandicapProbabilityHints")(
    test("neighborhoodSpots matches shared distinct neighborhood") {
      assertTrue(
        HandicapProbabilityHints.neighborhoodSpots(0) == List(0, 1),
        HandicapProbabilityHints.neighborhoodSpots(1) == List(0, 1, 2),
        HandicapProbabilityHints.neighborhoodSpots(3) == List(0, 2, 3, 4)
      )
    },
    test("headerLabel includes weaker name and handicap cap") {
      assertTrue(
        HandicapProbabilityHints.headerLabel("Bob", raceTo = 7) ==
          "Spot to: Bob (max 5 for Race to 7)"
      )
    },
    test("isOverCap flags spots above the race-to cap") {
      assertTrue(
        !HandicapProbabilityHints.isOverCap(5, raceTo = 7),
        HandicapProbabilityHints.isOverCap(6, raceTo = 7)
      )
    },
    test("formatWeakerPercent rounds half-up to whole percent") {
      assertTrue(
        HandicapProbabilityHints.formatWeakerPercent(0.007) == "1%",
        HandicapProbabilityHints.formatWeakerPercent(0.049) == "5%",
        HandicapProbabilityHints.formatWeakerPercent(0.118) == "12%",
        HandicapProbabilityHints.formatWeakerPercent(0.258) == "26%",
        HandicapProbabilityHints.formatWeakerPercent(0.5) == "50%"
      )
    },
    test("neighborhoodRows match WinProbability for each spot") {
      val suggested = 3
      val rows =
        HandicapProbabilityHints.neighborhoodRows(
          weaker,
          stronger,
          raceTo,
          suggested
        )
      val expectedSpots = HandicapProbabilityHints.neighborhoodSpots(suggested)
      val expectedProbs = expectedSpots.map { spot =>
        WinProbability.matchWinProbability(weaker, stronger, raceTo, spot)
      }
      assertTrue(
        rows.map(_.spot) == expectedSpots,
        rows.map(_.weakerProbability) == expectedProbs,
        rows.map(_.weakerPercent) == expectedProbs.map(
          HandicapProbabilityHints.formatWeakerPercent
        )
      )
    },
    test("typedSpotHint is none when typed spot is inside neighborhood") {
      val hint = HandicapProbabilityHints.typedSpotHint(
        weaker,
        stronger,
        raceTo,
        suggested = 3,
        typedInput = "3"
      )
      assertTrue(hint.isEmpty)
    },
    test("typedSpotHint is none when input is blank or unparseable") {
      assertTrue(
        HandicapProbabilityHints
          .typedSpotHint(weaker, stronger, raceTo, 3, "")
          .isEmpty,
        HandicapProbabilityHints
          .typedSpotHint(weaker, stronger, raceTo, 3, "abc")
          .isEmpty
      )
    },
    test("typedSpotHint is none for negative or over-cap typed spots") {
      assertTrue(
        HandicapProbabilityHints
          .typedSpotHint(weaker, stronger, raceTo, 3, "-1")
          .isEmpty,
        HandicapProbabilityHints
          .typedSpotHint(weaker, stronger, raceTo, 3, "6")
          .isEmpty
      )
    },
    test("typedSpotHint shows weaker percent when typed spot is outside") {
      val hint = HandicapProbabilityHints.typedSpotHint(
        weaker,
        stronger,
        raceTo,
        suggested = 3,
        typedInput = "5"
      )
      val expectedProb =
        WinProbability.matchWinProbability(weaker, stronger, raceTo, 5)
      assertTrue(
        hint.contains(
          s"+5 → ${HandicapProbabilityHints.formatWeakerPercent(expectedProb)}"
        )
      )
    },
    test("suggested 0 neighborhood has no duplicate +0 row") {
      val rows =
        HandicapProbabilityHints.neighborhoodRows(
          weaker,
          stronger,
          raceTo,
          suggested = 0
        )
      assertTrue(
        rows.map(_.spot) == List(0, 1),
        rows.size == 2
      )
    },
    test("suggested 1 neighborhood has three distinct rows") {
      val rows =
        HandicapProbabilityHints.neighborhoodRows(
          weaker,
          stronger,
          raceTo,
          suggested = 1
        )
      assertTrue(rows.map(_.spot) == List(0, 1, 2))
    },
    test("large rating gap rows match WinProbability for each spot") {
      val weak = rating(bob, 1100, rd = 50)
      val strong = rating(alice, 2200, rd = 50)
      val suggested = 5
      val rows =
        HandicapProbabilityHints.neighborhoodRows(
          weak,
          strong,
          raceTo,
          suggested
        )
      val expectedSpots = HandicapProbabilityHints.neighborhoodSpots(suggested)
      val expectedProbs = expectedSpots.map { spot =>
        WinProbability.matchWinProbability(weak, strong, raceTo, spot)
      }
      assertTrue(
        rows.map(_.spot) == expectedSpots,
        rows.map(_.weakerProbability) == expectedProbs
      )
    }
  )
}
