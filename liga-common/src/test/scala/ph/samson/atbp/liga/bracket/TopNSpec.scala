package ph.samson.atbp.liga.bracket

import zio.test.*

object TopNSpec extends ZIOSpecDefault {

  def spec = suite("TopN")(
    test("legalTopNs for roster 12 is 1 and powers of two up to roster") {
      assertTrue(
        TopN.legalTopNs(12) == List(1, 2, 4, 8),
        !TopN.legalTopNs(12).contains(16)
      )
    },
    test("legalTopNs for roster 8") {
      assertTrue(TopN.legalTopNs(8) == List(1, 2, 4, 8))
    },
    test("legalTopNs for roster 3") {
      assertTrue(TopN.legalTopNs(3) == List(1, 2))
    },
    test("legalTopNs for roster 1") {
      assertTrue(TopN.legalTopNs(1) == List(1))
    },
    test("legalTopNs for roster 64 includes 32 and 64") {
      assertTrue(
        TopN.legalTopNs(64).contains(32),
        TopN.legalTopNs(64).contains(64)
      )
    },
    test("defaultTopN prefers 8 when legal") {
      assertTrue(
        TopN.defaultTopN(12) == 8,
        TopN.defaultTopN(8) == 8,
        TopN.defaultTopN(64) == 8
      )
    },
    test("defaultTopN uses largest legal power of two when 8 is not legal") {
      assertTrue(TopN.defaultTopN(3) == 2)
    },
    test("defaultTopN is 1 when only 1 is legal") {
      assertTrue(TopN.defaultTopN(1) == 1)
    },
    test("legalTopNs never exceeds roster size") {
      assertTrue(
        (1 to 64).forall { roster =>
          TopN.legalTopNs(roster).forall(_ <= roster)
        }
      )
    }
  )
}
