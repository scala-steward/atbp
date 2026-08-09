package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.js.director.BracketResults.RatingMovementDisplay
import ph.samson.atbp.liga.js.director.BracketResults.ResultsCellDisplay
import ph.samson.atbp.liga.model.PlayerRatingLabel
import zio.test.*

object AppliedHandicapViewSpec extends ZIOSpecDefault {

  def spec = suite("AppliedHandicapView")(
    suite("playerLabelText")(
      test("showRatings true keeps W-L suffix on annotated cells") {
        assertTrue(
          AppliedHandicapView.playerLabelText(
            "Alice",
            ResultsCellDisplay.Annotate(
              wins = 2,
              losses = 1,
              movement = RatingMovementDisplay.RatedDelta(1543, 12)
            ),
            showRatings = true
          ) == "Alice (2-1)"
        )
      },
      test("showRatings false omits W-L suffix on annotated cells") {
        assertTrue(
          AppliedHandicapView.playerLabelText(
            "Alice",
            ResultsCellDisplay.Annotate(
              wins = 2,
              losses = 1,
              movement = RatingMovementDisplay.RatedDelta(1543, 12)
            ),
            showRatings = false
          ) == "Alice"
        )
      },
      test("showRatings false leaves skip cells as plain name") {
        assertTrue(
          AppliedHandicapView.playerLabelText(
            "Bob",
            ResultsCellDisplay.Skip,
            showRatings = false
          ) == "Bob"
        )
      }
    ),
    suite("includesRatingSubline")(
      test("showRatings true includes subline for annotated veterans") {
        assertTrue(
          AppliedHandicapView.includesRatingSubline(
            ResultsCellDisplay.Annotate(
              wins = 1,
              losses = 0,
              movement = RatingMovementDisplay.NewRating(1555)
            ),
            frozenLabel = Some(PlayerRatingLabel.Rated(1543)),
            showRatings = true
          )
        )
      },
      test("showRatings false never includes rating subline") {
        assertTrue(
          !AppliedHandicapView.includesRatingSubline(
            ResultsCellDisplay.Annotate(
              wins = 1,
              losses = 0,
              movement = RatingMovementDisplay.NewRating(1555)
            ),
            frozenLabel = Some(PlayerRatingLabel.Rated(1543)),
            showRatings = false
          ),
          !AppliedHandicapView.includesRatingSubline(
            ResultsCellDisplay.Skip,
            frozenLabel = Some(PlayerRatingLabel.Rated(1543)),
            showRatings = false
          )
        )
      }
    )
  )
}
