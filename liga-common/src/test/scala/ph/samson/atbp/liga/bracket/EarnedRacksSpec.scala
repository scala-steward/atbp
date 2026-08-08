package ph.samson.atbp.liga.bracket

import ph.samson.atbp.liga.model.Player
import zio.test.*

object EarnedRacksSpec extends ZIOSpecDefault {

  private val alice = Player("Alice")
  private val bob = Player("Bob")
  private val carol = Player("Carol")
  private val dave = Player("Dave")

  private def profile(
      player: Player,
      ratingSeed: Double,
      drawOrder: Int
  ): EarnedRacks.SurvivorProfile =
    EarnedRacks.SurvivorProfile(player, ratingSeed, drawOrder)

  def spec = suite("EarnedRacks")(
    test("earnedScores excludes handicap spots on the weaker side") {
      val matchDef = EarnedRacks.PlayedMatch(
        playerA = bob,
        playerB = alice,
        scoreA = 4,
        scoreB = 7,
        handicapApplied = 3,
        weaker = Some(bob),
        kind = EarnedRacks.MatchKind.Played
      )
      assertTrue(EarnedRacks.earnedScores(matchDef) == ((1, 7)))
    },
    test("byes and forfeits contribute zero earned racks") {
      val bye = EarnedRacks.PlayedMatch(
        playerA = alice,
        playerB = bob,
        scoreA = 1,
        scoreB = 0,
        handicapApplied = 0,
        weaker = None,
        kind = EarnedRacks.MatchKind.Bye
      )
      val forfeit = bye.copy(kind = EarnedRacks.MatchKind.Forfeit)
      assertTrue(
        EarnedRacks.earnedScores(bye) == ((0, 0)),
        EarnedRacks.earnedScores(forfeit) == ((0, 0)),
        EarnedRacks.rackDifferential(List(bye), alice) == 0
      )
    },
    test(
      "rankSurvivors orders by rack differential then rating then draw order"
    ) {
      val played = List(
        EarnedRacks.PlayedMatch(
          playerA = alice,
          playerB = bob,
          scoreA = 7,
          scoreB = 4,
          handicapApplied = 0,
          weaker = None,
          kind = EarnedRacks.MatchKind.Played
        ),
        EarnedRacks.PlayedMatch(
          playerA = carol,
          playerB = dave,
          scoreA = 5,
          scoreB = 7,
          handicapApplied = 0,
          weaker = None,
          kind = EarnedRacks.MatchKind.Played
        )
      )
      val profiles = Map(
        alice -> profile(alice, ratingSeed = 1500, drawOrder = 1),
        bob -> profile(bob, ratingSeed = 1400, drawOrder = 2),
        carol -> profile(carol, ratingSeed = 1600, drawOrder = 3),
        dave -> profile(dave, ratingSeed = 1300, drawOrder = 4)
      )
      val ranked =
        EarnedRacks.rankSurvivors(
          List(alice, bob, carol, dave),
          played,
          profiles
        )
      assertTrue(
        ranked == Right(List(alice, dave, carol, bob))
      )
    },
    test("rankSurvivors returns Left when a survivor has no profile") {
      val ranked = EarnedRacks.rankSurvivors(
        List(alice, bob),
        matches = Nil,
        profiles =
          Map(alice -> profile(alice, ratingSeed = 1500, drawOrder = 1))
      )
      assertTrue(
        ranked == Left("missing survivor profile for Bob")
      )
    },
    test("assignSingleElimSlots uses 1 vs N snake placement") {
      val ranked = List(alice, bob, carol, dave)
      assertTrue(
        EarnedRacks.assignSingleElimSlots(ranked) ==
          List(alice, dave, bob, carol)
      )
    }
  )
}
