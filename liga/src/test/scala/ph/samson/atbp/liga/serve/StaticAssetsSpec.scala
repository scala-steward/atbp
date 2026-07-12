package ph.samson.atbp.liga.serve

import zio.http.*
import zio.test.*

object StaticAssetsSpec extends ZIOSpecDefault {

  def spec = suite("StaticAssets")(
    test("director HTML mounts Laminar app and references JS bundle") {
      val html = StaticAssets.directorHtml
      assertTrue(
        html.contains("id=\"app\""),
        html.contains("Liga Director"),
        html.contains("/assets/js/")
      )
    },
    test("audience HTML mounts Laminar app and references JS bundle") {
      val html = StaticAssets.audienceHtml
      assertTrue(
        html.contains("id=\"app\""),
        html.contains("Liga Audience"),
        html.contains("/assets/js/")
      )
    },
    test("asset routes serve director bundle from classpath") {
      val path =
        StaticAssets.jsAssetPath(StaticAssets.directorScriptName)
      for {
        response <- StaticAssets.assetRoutes.runZIO(Request.get(path))
        body <- response.body.asString
      } yield assertTrue(
        response.status == Status.Ok,
        body.nonEmpty,
        response
          .header(Header.ContentType)
          .exists(_.mediaType == MediaType.application.javascript)
      )
    },
    test("asset routes reject path traversal in file name") {
      for {
        response <- StaticAssets.assetRoutes.runZIO(
          Request.get("/assets/js/..")
        )
      } yield assertTrue(response.status == Status.NotFound)
    },
    test("asset routes return 404 for unknown JS file") {
      for {
        response <- StaticAssets.assetRoutes.runZIO(
          Request.get("/assets/js/missing.js")
        )
      } yield assertTrue(response.status == Status.NotFound)
    }
  )
}
