package ph.samson.atbp.liga

import zio.test.*

object ScaffoldSpec extends ZIOSpecDefault {

  def spec = suite("Scaffold")(
    test("liga module is on the test classpath") {
      assertTrue(true)
    }
  )
}
