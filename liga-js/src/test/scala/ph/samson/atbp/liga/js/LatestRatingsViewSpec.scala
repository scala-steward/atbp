package ph.samson.atbp.liga.js

import zio.test.*

object LatestRatingsViewSpec extends ZIOSpecDefault {

  def spec = suite("LatestRatingsView")(
    test("formatDelta rounds near-zero values to 0") {
      assertTrue(
        LatestRatingsView.formatDelta(0.0) == "0",
        LatestRatingsView.formatDelta(0.0001) == "0",
        LatestRatingsView.formatDelta(-0.0001) == "0"
      )
    },
    test("formatDelta shows signed whole-number deltas") {
      assertTrue(
        LatestRatingsView.formatDelta(5.4) == "+5",
        LatestRatingsView.formatDelta(-3.6) == "-4",
        LatestRatingsView.formatDelta(-12.0) == "-12"
      )
    }
  )
}
