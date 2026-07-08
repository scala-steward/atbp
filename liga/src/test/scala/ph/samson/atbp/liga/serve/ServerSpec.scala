package ph.samson.atbp.liga.serve

import better.files.File
import ph.samson.atbp.liga.serve.Server as LigaServer
import zio.http.*
import zio.test.*

object ServerSpec extends ZIOSpecDefault {

  private val ctx = ServeContext(
    dataDir = File(getClass.getResource("/periods")),
    tournamentDir =
      Some(File(getClass.getResource("/tournaments/eight-player-seeded")))
  )

  def spec = suite("Server")(
    test("GET /health returns ok") {
      for {
        response <- LigaServer
          .routes(ctx, BindConfig())
          .runZIO(Request.get("/health"))
        body <- response.body.asString
      } yield assertTrue(response.status == Status.Ok, body == "ok")
    },
    test("GET / returns director SPA shell HTML") {
      for {
        response <- LigaServer
          .routes(ctx, BindConfig())
          .runZIO(Request.get("/"))
        body <- response.body.asString
      } yield assertTrue(
        response.status == Status.Ok,
        body.contains("Liga Director"),
        body.contains("id=\"app\""),
        body.contains("/assets/js/"),
        response
          .header(Header.ContentType)
          .exists(_.mediaType == MediaType.text.html)
      )
    },
    test("GET /audience returns audience SPA shell HTML") {
      for {
        response <- LigaServer
          .routes(ctx, BindConfig())
          .runZIO(Request.get("/audience"))
        body <- response.body.asString
      } yield assertTrue(
        response.status == Status.Ok,
        body.contains("Liga Audience"),
        body.contains("id=\"app\""),
        body.contains("/assets/js/")
      )
    },
    test("default BindConfig binds localhost:5442") {
      val httpConfig = LigaServer.httpConfig(BindConfig())
      assertTrue(
        httpConfig.address.getHostString == "127.0.0.1",
        httpConfig.address.getPort == 5442
      )
    },
    test("loads audience poll interval from HOCON") {
      val hocon =
        """host = "127.0.0.1"
          |port = 5442
          |audience-poll-interval-seconds = 10
          |""".stripMargin
      for {
        config <- ServeConfig.parseHocon(hocon)
      } yield assertTrue(config.audiencePollIntervalSeconds == 10)
    }
  )
}
