package ph.samson.atbp.liga.bracket

import zio.test.*

object RaceToWizardSpec extends ZIOSpecDefault {

  private val eightPlayerTop2Keys = RaceToScopes.requiredKeys(8, 2)

  private def mapAt(state: RaceToWizard.State, key: String): Int =
    state.raceToByScope(key)

  def spec = suite("RaceToWizard")(
    test("initialState(3) pre-fills all scopes for size-4 bracket") {
      val keys = RaceToScopes.requiredKeys(3, 2)
      val state = RaceToWizard.initialState(3)
      assertTrue(
        state.topN == 2,
        keys == List("wb-1", "wb-2", "lb-1", "lb-2", "gf"),
        keys.forall(key => mapAt(state, key) == 7),
        !state.gfPinned
      )
    },
    test("initialState(8) uses defaultTopN full SE scopes") {
      val keys = RaceToScopes.requiredKeys(8, 8)
      val state = RaceToWizard.initialState(8)
      assertTrue(
        state.topN == 8,
        keys.forall(key => mapAt(state, key) == 7),
        !state.gfPinned
      )
    },
    test("initialState(8, 2) pre-fills classic double-elim scopes") {
      val state = RaceToWizard.initialState(8, topN = 2)
      assertTrue(
        state.topN == 2,
        eightPlayerTop2Keys.forall(key => mapAt(state, key) == 7),
        !state.gfPinned
      )
    },
    test("editing wb-1 cascades winners, losers, and unpinned gf for Top 2") {
      val initial = RaceToWizard.initialState(8, topN = 2)
      val edited = RaceToWizard.applyEdit(initial, "wb-1", 5, playerCount = 8)
      assertTrue(
        mapAt(edited, "wb-1") == 5,
        mapAt(edited, "wb-2") == 5,
        mapAt(edited, "wb-3") == 5,
        mapAt(edited, "lb-1") == 5,
        mapAt(edited, "lb-2") == 5,
        mapAt(edited, "lb-3") == 5,
        mapAt(edited, "lb-4") == 5,
        mapAt(edited, "gf") == 5,
        !edited.gfPinned
      )
    },
    test("editing lb-k cascades only the losers section") {
      val initial = RaceToWizard.initialState(8, topN = 2)
      val edited = RaceToWizard.applyEdit(initial, "lb-2", 5, playerCount = 8)
      assertTrue(
        mapAt(edited, "wb-1") == 7,
        mapAt(edited, "wb-2") == 7,
        mapAt(edited, "wb-3") == 7,
        mapAt(edited, "lb-1") == 7,
        mapAt(edited, "lb-2") == 5,
        mapAt(edited, "lb-3") == 5,
        mapAt(edited, "lb-4") == 5,
        mapAt(edited, "gf") == 7
      )
    },
    test("editing gf pins grand final against later winners edits") {
      val initial = RaceToWizard.initialState(8, topN = 2)
      val withGf = RaceToWizard.applyEdit(initial, "gf", 9, playerCount = 8)
      val afterWb2 = RaceToWizard.applyEdit(withGf, "wb-2", 5, playerCount = 8)
      assertTrue(
        withGf.gfPinned,
        mapAt(withGf, "gf") == 9,
        mapAt(afterWb2, "wb-2") == 5,
        mapAt(afterWb2, "wb-3") == 5,
        mapAt(afterWb2, "gf") == 9,
        afterWb2.gfPinned
      )
    },
    test("editing wb-k updates unpinned gf to wb-N") {
      val initial = RaceToWizard.initialState(8, topN = 2)
      val edited = RaceToWizard.applyEdit(initial, "wb-2", 5, playerCount = 8)
      assertTrue(
        mapAt(edited, "wb-1") == 7,
        mapAt(edited, "wb-2") == 5,
        mapAt(edited, "wb-3") == 5,
        mapAt(edited, "gf") == 5
      )
    },
    test("editing se-1 cascades single-elimination rounds") {
      val initial = RaceToWizard.initialState(12, topN = 8)
      val edited = RaceToWizard.applyEdit(initial, "se-1", 5, playerCount = 12)
      assertTrue(
        mapAt(edited, "se-1") == 5,
        mapAt(edited, "se-2") == 5,
        mapAt(edited, "se-3") == 5
      )
    },
    test("editing se-k cascades only later single-elimination rounds") {
      val initial = RaceToWizard.initialState(12, topN = 8)
      val edited = RaceToWizard.applyEdit(initial, "se-2", 5, playerCount = 12)
      assertTrue(
        mapAt(edited, "se-1") == 7,
        mapAt(edited, "se-2") == 5,
        mapAt(edited, "se-3") == 5
      )
    },
    test("changeTopN from Top 8 to Top 4 keeps overlapping scope values") {
      val customized = RaceToWizard
        .initialState(12, topN = 8)
        .pipe(
          RaceToWizard.applyEdit(_, "wb-1", 5, playerCount = 12)
        )
        .pipe(
          RaceToWizard.applyEdit(_, "se-1", 6, playerCount = 12)
        )
      val scrubbed =
        RaceToWizard.changeTopN(customized, newTopN = 4, playerCount = 12)

      assertTrue(
        scrubbed.topN == 4,
        mapAt(scrubbed, "wb-1") == 5,
        mapAt(scrubbed, "wb-2") == 5,
        mapAt(scrubbed, "lb-1") == 5,
        mapAt(scrubbed, "lb-2") == 5,
        mapAt(scrubbed, "se-1") == 6,
        mapAt(scrubbed, "se-2") == 6,
        scrubbed.raceToByScope.contains("wb-3"),
        scrubbed.raceToByScope.contains("lb-3"),
        scrubbed.raceToByScope.contains("lb-4"),
        !scrubbed.raceToByScope.contains("se-3"),
        !scrubbed.raceToByScope.contains("gf")
      )
    },
    test("changeTopN to larger cut defaults only new scopes") {
      val customized = RaceToWizard
        .initialState(12, topN = 4)
        .pipe(
          RaceToWizard.applyEdit(_, "wb-1", 5, playerCount = 12)
        )
        .pipe(
          RaceToWizard.applyEdit(_, "se-1", 6, playerCount = 12)
        )
      val expanded =
        RaceToWizard.changeTopN(customized, newTopN = 8, playerCount = 12)

      assertTrue(
        expanded.topN == 8,
        mapAt(expanded, "wb-1") == 5,
        mapAt(expanded, "wb-2") == 5,
        mapAt(expanded, "lb-1") == 5,
        mapAt(expanded, "lb-2") == 5,
        mapAt(expanded, "se-1") == 6,
        mapAt(expanded, "se-2") == 6,
        mapAt(expanded, "se-3") == 6,
        !expanded.raceToByScope.contains("wb-3"),
        !expanded.raceToByScope.contains("lb-3"),
        !expanded.raceToByScope.contains("lb-4")
      )
    },
    test("changeTopN scrubs map to required keys only") {
      val state = RaceToWizard.changeTopN(
        RaceToWizard.initialState(12, topN = 8),
        newTopN = 4,
        playerCount = 12
      )
      assertTrue(
        state.raceToByScope.keySet ==
          RaceToScopes.requiredKeys(12, 4).toSet
      )
    },
    test("loadState infers gfPinned when gf differs from wb-N") {
      val serverMap =
        RaceToWizard.initialState(8, topN = 2).raceToByScope.updated("gf", 9)
      val loaded = RaceToWizard.loadState(serverMap, playerCount = 8, topN = 2)
      assertTrue(loaded.gfPinned, mapAt(loaded, "gf") == 9, loaded.topN == 2)
    },
    test("loadState leaves gf unpinned when gf matches wb-N") {
      val serverMap = RaceToWizard.initialState(8, topN = 2).raceToByScope
      val loaded = RaceToWizard.loadState(serverMap, playerCount = 8, topN = 2)
      assertTrue(!loaded.gfPinned)
    },
    test("loadState scrubs unknown scopes") {
      val serverMap =
        RaceToWizard.initialState(12, topN = 4).raceToByScope ++
          Map("se-3" -> 99, "gf" -> 99)
      val loaded = RaceToWizard.loadState(serverMap, playerCount = 12, topN = 4)
      assertTrue(
        loaded.raceToByScope.keySet ==
          RaceToScopes.requiredKeys(12, 4).toSet,
        !loaded.raceToByScope.contains("se-3"),
        !loaded.raceToByScope.contains("gf")
      )
    },
    test("spec 8-player differentiated scenario") {
      val step1 = RaceToWizard.applyEdit(
        RaceToWizard.initialState(8, topN = 2),
        "lb-1",
        5,
        playerCount = 8
      )
      val step2 = RaceToWizard.applyEdit(step1, "wb-2", 5, playerCount = 8)
      val step3 = RaceToWizard.applyEdit(step2, "gf", 9, playerCount = 8)
      assertTrue(
        mapAt(step1, "lb-1") == 5,
        mapAt(step1, "lb-4") == 5,
        mapAt(step1, "wb-3") == 7,
        mapAt(step2, "wb-1") == 7,
        mapAt(step2, "wb-2") == 5,
        mapAt(step2, "wb-3") == 5,
        mapAt(step2, "gf") == 5,
        mapAt(step3, "gf") == 9,
        step3.gfPinned
      )
    }
  )

  extension [A](value: A) {
    def pipe[B](f: A => B): B = f(value)
  }
}
