package ph.samson.atbp.liga.js.audience

import ph.samson.atbp.liga.js.api.Models.*
import ph.samson.atbp.liga.js.director.BracketLayout
import zio.test.*

object AudienceSpatialLayoutSpec extends ZIOSpecDefault {

  def spec = suite("AudienceSpatialLayout")(
    test("layout omits rounds with only hidden empty-future Pending matches") {
      val hidden = BracketMatch(
        id = "wb-1-1",
        playerA = None,
        playerB = None,
        state = BracketMatchState.Pending
      )
      val shown = BracketMatch(
        id = "wb-2-1",
        playerA = Some(Player("P1")),
        playerB = Some(Player("P2")),
        state = BracketMatchState.Ready
      )
      val bands =
        AudienceSpatialLayout.layout(
          matches = List(hidden, shown),
          bracketSize = 8
        )
      assertTrue(
        bands.length == 1,
        bands.head.section == BracketLayout.Section.Winners,
        bands.head.columns.map(_.round) == List(2)
      )
    },
    test(
      "layout places unfinished columns left of completed columns in a band"
    ) {
      val player = Some(Player("P1"))
      val opponent = Some(Player("P2"))
      val unfinished = BracketMatch(
        id = "wb-2-1",
        playerA = player,
        playerB = opponent,
        state = BracketMatchState.Started
      )
      val finished = BracketMatch(
        id = "wb-1-1",
        playerA = player,
        playerB = opponent,
        state = BracketMatchState.Completed
      )
      val bands =
        AudienceSpatialLayout.layout(
          matches = List(finished, unfinished),
          bracketSize = 8
        )
      assertTrue(
        bands.length == 1,
        bands.head.columns.map(_.round) == List(2, 1)
      )
    },
    test(
      "layout places later unfinished columns left of earlier unfinished within a band"
    ) {
      val player = Some(Player("P1"))
      val opponent = Some(Player("P2"))
      val round2 = BracketMatch(
        id = "wb-2-1",
        playerA = player,
        playerB = opponent,
        state = BracketMatchState.Ready
      )
      val round3 = round2.copy(id = "wb-3-1")
      val bands =
        AudienceSpatialLayout.layout(
          matches = List(round2, round3),
          bracketSize = 8
        )
      assertTrue(
        bands.length == 1,
        bands.head.columns.map(_.round) == List(3, 2)
      )
    },
    test("layout stacks Winners band above Losers band") {
      val player = Some(Player("P1"))
      val opponent = Some(Player("P2"))
      val wb = BracketMatch(
        id = "wb-1-1",
        playerA = player,
        playerB = opponent,
        state = BracketMatchState.Ready
      )
      val lb = BracketMatch(
        id = "lb-1-1",
        playerA = player,
        playerB = opponent,
        state = BracketMatchState.Ready
      )
      val bands =
        AudienceSpatialLayout.layout(
          matches = List(lb, wb),
          bracketSize = 8
        )
      assertTrue(
        bands.map(_.section) == List(
          BracketLayout.Section.Winners,
          BracketLayout.Section.Losers
        )
      )
    },
    test("layout sorts matches within a column by seed index not status") {
      val player = Some(Player("P1"))
      val ready = BracketMatch(
        id = "wb-2-4",
        playerA = player,
        playerB = None,
        state = BracketMatchState.Ready
      )
      val pending = BracketMatch(
        id = "wb-2-1",
        playerA = player,
        playerB = None,
        state = BracketMatchState.Pending
      )
      val started =
        pending.copy(id = "wb-2-3", state = BracketMatchState.Started)
      val completed =
        pending.copy(id = "wb-2-2", state = BracketMatchState.Completed)
      val bands =
        AudienceSpatialLayout.layout(
          matches = List(completed, started, pending, ready),
          bracketSize = 8
        )
      assertTrue(
        bands.length == 1,
        bands.head.columns.head.matches.map(_.id) ==
          List("wb-2-1", "wb-2-2", "wb-2-3", "wb-2-4")
      )
    },
    test("layout uses BracketLayout.showForAudience visibility") {
      val hidden = BracketMatch(
        id = "wb-1-1",
        playerA = None,
        playerB = None,
        state = BracketMatchState.Pending
      )
      assertTrue(
        !BracketLayout.showForAudience(hidden),
        AudienceSpatialLayout
          .layout(
            matches = List(hidden),
            bracketSize = 8
          )
          .isEmpty
      )
    }
  )
}
