package ph.samson.atbp.liga.js.director

import ph.samson.atbp.liga.handicap.Handicap
import ph.samson.atbp.liga.js.api.Models.*
import ph.samson.atbp.liga.js.director.BracketLayout.AppliedHandicapDisplay
import ph.samson.atbp.liga.js.director.BracketLayout.AppliedHandicapSide
import ph.samson.atbp.liga.model as shared
import zio.test.*

object AppliedHandicapLabelsSpec extends ZIOSpecDefault {

  private val alice = Player("Alice")
  private val bob = Player("Bob")

  private def jsRating(
      player: Player,
      rating: Double,
      rd: Double
  ): PlayerRating =
    PlayerRating(player, rating, rd, wins = 0, losses = 0)

  private def preview(weaker: Player, stronger: Player, raceTo: Int) = {
    val weakerRating = shared.PlayerRating(
      shared.Player(weaker.name),
      if (weaker.name == "Bob") 1450 else 1700,
      90,
      wins = 0,
      losses = 0
    )
    val strongerRating = shared.PlayerRating(
      shared.Player(stronger.name),
      if (stronger.name == "Alice") 1700 else 1450,
      80,
      wins = 0,
      losses = 0
    )
    val suggestion = Handicap.suggest(weakerRating, strongerRating, raceTo)
    MatchHandicapPreview(weakerRating, strongerRating, suggestion)
  }

  private def handicapContext(
      frozenRatings: List[PlayerRating]
  ): BracketHandicapContext =
    BracketHandicapContext(frozenRatings, Map("wb-1" -> 7))

  def spec = suite("AppliedHandicapLabels")(
    suite("appliedHandicapDisplay")(
      test("Started with spot and weaker A places spot on A side") {
        val matchDef = BracketMatch(
          id = "wb-1-1",
          playerA = Some(alice),
          playerB = Some(bob),
          state = BracketMatchState.Started,
          handicapApplied = Some(1)
        )
        assertTrue(
          AppliedHandicapLabels.appliedHandicapDisplay(
            matchDef,
            Some(preview(alice, bob, 7))
          ) == AppliedHandicapDisplay.Placed(
            spot = 1,
            side = AppliedHandicapSide.PlayerA,
            weakerName = "Alice"
          )
        )
      },
      test("Started with spot and weaker B places spot on B side") {
        val matchDef = BracketMatch(
          id = "wb-1-1",
          playerA = Some(alice),
          playerB = Some(bob),
          state = BracketMatchState.Started,
          handicapApplied = Some(2)
        )
        assertTrue(
          AppliedHandicapLabels.appliedHandicapDisplay(
            matchDef,
            Some(preview(bob, alice, 7))
          ) == AppliedHandicapDisplay.Placed(
            spot = 2,
            side = AppliedHandicapSide.PlayerB,
            weakerName = "Bob"
          )
        )
      },
      test("Completed with spot uses same placement rules") {
        val matchDef = BracketMatch(
          id = "wb-1-1",
          playerA = Some(alice),
          playerB = Some(bob),
          state = BracketMatchState.Completed,
          handicapApplied = Some(1)
        )
        assertTrue(
          AppliedHandicapLabels.appliedHandicapDisplay(
            matchDef,
            Some(preview(bob, alice, 7))
          ) == AppliedHandicapDisplay.Placed(
            spot = 1,
            side = AppliedHandicapSide.PlayerB,
            weakerName = "Bob"
          )
        )
      },
      test("Ready with applied spot is Hidden") {
        val matchDef = BracketMatch(
          id = "wb-1-1",
          playerA = Some(alice),
          playerB = Some(bob),
          state = BracketMatchState.Ready,
          handicapApplied = Some(1)
        )
        assertTrue(
          AppliedHandicapLabels.appliedHandicapDisplay(
            matchDef,
            Some(preview(bob, alice, 7))
          ) == AppliedHandicapDisplay.Hidden
        )
      },
      test("Pending with applied spot is Hidden") {
        val matchDef = BracketMatch(
          id = "wb-1-1",
          playerA = Some(alice),
          playerB = Some(bob),
          state = BracketMatchState.Pending,
          handicapApplied = Some(1)
        )
        assertTrue(
          AppliedHandicapLabels.appliedHandicapDisplay(
            matchDef,
            Some(preview(bob, alice, 7))
          ) == AppliedHandicapDisplay.Hidden
        )
      },
      test("empty handicapApplied is Hidden") {
        val matchDef = BracketMatch(
          id = "wb-1-1",
          playerA = Some(alice),
          playerB = Some(bob),
          state = BracketMatchState.Started,
          handicapApplied = None
        )
        assertTrue(
          AppliedHandicapLabels.appliedHandicapDisplay(
            matchDef,
            Some(preview(bob, alice, 7))
          ) == AppliedHandicapDisplay.Hidden
        )
      },
      test("zero handicapApplied is Hidden") {
        val matchDef = BracketMatch(
          id = "wb-1-1",
          playerA = Some(alice),
          playerB = Some(bob),
          state = BracketMatchState.Started,
          handicapApplied = Some(0)
        )
        assertTrue(
          AppliedHandicapLabels.appliedHandicapDisplay(
            matchDef,
            Some(preview(bob, alice, 7))
          ) == AppliedHandicapDisplay.Hidden
        )
      },
      test("Started with spot and missing preview is Unresolved") {
        val matchDef = BracketMatch(
          id = "wb-1-1",
          playerA = Some(alice),
          playerB = Some(bob),
          state = BracketMatchState.Started,
          handicapApplied = Some(2)
        )
        assertTrue(
          AppliedHandicapLabels.appliedHandicapDisplay(matchDef, None) ==
            AppliedHandicapDisplay.Unresolved(spot = 2)
        )
      },
      test("preview weaker not on match roster is Unresolved") {
        val matchDef = BracketMatch(
          id = "wb-1-1",
          playerA = Some(alice),
          playerB = Some(bob),
          state = BracketMatchState.Completed,
          handicapApplied = Some(1)
        )
        val carolPreview = preview(
          Player("Carol"),
          alice,
          7
        )
        assertTrue(
          AppliedHandicapLabels.appliedHandicapDisplay(
            matchDef,
            Some(carolPreview)
          ) == AppliedHandicapDisplay.Unresolved(spot = 1)
        )
      }
    ),
    test("forMatch resolves preview from frozen ratings") {
      val frozen = List(
        jsRating(alice, 1700, 80),
        jsRating(bob, 1450, 90)
      )
      val matchDef = BracketMatch(
        id = "wb-1-1",
        playerA = Some(alice),
        playerB = Some(bob),
        state = BracketMatchState.Started,
        handicapApplied = Some(2)
      )
      assertTrue(
        AppliedHandicapLabels.forMatch(
          handicapContext(frozen),
          matchDef
        ) ==
          AppliedHandicapDisplay.Placed(
            spot = 2,
            side = AppliedHandicapSide.PlayerB,
            weakerName = "Bob"
          )
      )
    },
    suite("panelStatusMessage")(
      test("formats Started copy for Placed") {
        assertTrue(
          AppliedHandicapLabels.panelStatusMessage(
            AppliedHandicapDisplay.Placed(
              spot = 2,
              side = AppliedHandicapSide.PlayerB,
              weakerName = "Bob"
            ),
            completed = false
          ) == Some("Handicap applied: +2 to Bob")
        )
      },
      test("formats Completed copy for Placed") {
        assertTrue(
          AppliedHandicapLabels.panelStatusMessage(
            AppliedHandicapDisplay.Placed(
              spot = 1,
              side = AppliedHandicapSide.PlayerA,
              weakerName = "Alice"
            ),
            completed = true
          ) == Some("Handicap was +1 to Alice")
        )
      },
      test("formats Started bug copy for Unresolved") {
        assertTrue(
          AppliedHandicapLabels.panelStatusMessage(
            AppliedHandicapDisplay.Unresolved(spot = 2),
            completed = false
          ) == Some(
            "Handicap applied: +2 — recipient unresolved; please file a bug."
          )
        )
      },
      test("formats Completed bug copy for Unresolved") {
        assertTrue(
          AppliedHandicapLabels.panelStatusMessage(
            AppliedHandicapDisplay.Unresolved(spot = 1),
            completed = true
          ) == Some(
            "Handicap was +1 — recipient unresolved; please file a bug."
          )
        )
      },
      test("Hidden has no panel message") {
        assertTrue(
          AppliedHandicapLabels
            .panelStatusMessage(
              AppliedHandicapDisplay.Hidden,
              completed = false
            )
            .isEmpty
        )
      }
    )
  )
}
