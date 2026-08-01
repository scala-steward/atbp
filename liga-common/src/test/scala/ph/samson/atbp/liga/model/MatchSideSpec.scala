package ph.samson.atbp.liga.model

import zio.test.*

object MatchSideSpec extends ZIOSpecDefault {

  def spec = suite("MatchSide")(
    test("parse accepts A and B only") {
      assertTrue(
        MatchSide.parse("A") == Some(MatchSide.A),
        MatchSide.parse("B") == Some(MatchSide.B),
        MatchSide.parse("C").isEmpty
      )
    },
    test("winnerFromForfeiting returns the non-forfeiting side") {
      assertTrue(
        MatchSide.winnerFromForfeiting(MatchSide.A) == MatchSide.B,
        MatchSide.winnerFromForfeiting(MatchSide.B) == MatchSide.A
      )
    },
    test("select picks the A or B branch") {
      assertTrue(
        MatchSide.select(MatchSide.A, 1, 2) == 1,
        MatchSide.select(MatchSide.B, 1, 2) == 2
      )
    },
    test("wire round-trips parse") {
      assertTrue(
        MatchSide.parse(MatchSide.wire(MatchSide.A)) == Some(MatchSide.A),
        MatchSide.parse(MatchSide.wire(MatchSide.B)) == Some(MatchSide.B)
      )
    }
  )
}
