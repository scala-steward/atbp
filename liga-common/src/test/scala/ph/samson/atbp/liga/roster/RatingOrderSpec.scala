package ph.samson.atbp.liga.roster

import ph.samson.atbp.liga.model.Player
import ph.samson.atbp.liga.model.PlayerRating
import zio.test.*

object RatingOrderSpec extends ZIOSpecDefault {

  private def pr(name: String, rating: Double, rd: Double): PlayerRating =
    PlayerRating(Player(name), rating, rd, wins = 0, losses = 0)

  def spec = suite("RatingOrder")(
    test("sorts by descending rating") {
      val players = List(
        pr("Alice", 1800, rd = 100),
        pr("Bob", 1600, rd = 100),
        pr("Carol", 1700, rd = 100)
      )
      assertTrue(
        RatingOrder.sortBestFirst(players).map(_.player.name) ==
          List("Alice", "Carol", "Bob")
      )
    },
    test("tie-break uses lower RD then alphabetical name") {
      val amy = pr("Amy", 1500, rd = 100)
      val zoe = pr("Zoe", 1500, rd = 100)
      val bob = pr("Bob", 1500, rd = 80)
      assertTrue(
        RatingOrder.sortBestFirst(List(zoe, amy, bob)).map(_.player.name) ==
          List("Bob", "Amy", "Zoe")
      )
    },
    test("compareBestFirst is transitive on rating ties") {
      val alice = pr("Alice", 1500, rd = 120)
      val bob = pr("Bob", 1500, rd = 80)
      assertTrue(
        RatingOrder.compareBestFirst(bob, alice),
        !RatingOrder.compareBestFirst(alice, bob)
      )
    }
  )
}
