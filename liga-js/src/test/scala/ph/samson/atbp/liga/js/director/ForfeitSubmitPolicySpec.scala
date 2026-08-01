package ph.samson.atbp.liga.js.director

import zio.test.*

object ForfeitSubmitPolicySpec extends ZIOSpecDefault {

  def spec = suite("ForfeitSubmitPolicy")(
    test("accepts trimmed non-blank reason without side effects") {
      val outcome = ForfeitSubmitPolicy.validate("B", "  no-show  ")
      assertTrue(
        outcome == ForfeitSubmitPolicy.Outcome.Ready(
          ForfeitSubmitPolicy.Submit("B", "no-show")
        )
      )
    },
    test("rejects blank and whitespace-only reason") {
      assertTrue(
        ForfeitSubmitPolicy.validate("A", "") ==
          ForfeitSubmitPolicy.Outcome.BlankReason,
        ForfeitSubmitPolicy.validate("A", "   ") ==
          ForfeitSubmitPolicy.Outcome.BlankReason
      )
    }
  )
}
