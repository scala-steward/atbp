package ph.samson.atbp.liga.js.audience

import zio.test.*

object AudienceRouteSpec extends ZIOSpecDefault {

  def spec = suite("AudienceRoute")(
    test("spatial bracket path is recognized with and without trailing slash") {
      assertTrue(
        AudienceRoute.isSpatialBracket("/audience/bracket"),
        AudienceRoute.isSpatialBracket("/audience/bracket/"),
        !AudienceRoute.isSpatialBracket("/audience"),
        !AudienceRoute.isSpatialBracket("/audience/"),
        !AudienceRoute.isSpatialBracket("/")
      )
    }
  )
}
