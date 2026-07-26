package ph.samson.atbp.liga.handicap

import zio.test.*

object HandicapSpec extends ZIOSpecDefault {

  def spec = suite("Handicap")(
    test("probabilityNeighborhoodSpots is a distinct ordered neighborhood") {
      assertTrue(
        Handicap.probabilityNeighborhoodSpots(0) == List(0, 1),
        Handicap.probabilityNeighborhoodSpots(1) == List(0, 1, 2),
        Handicap.probabilityNeighborhoodSpots(3) == List(0, 2, 3, 4)
      )
    }
  )
}
