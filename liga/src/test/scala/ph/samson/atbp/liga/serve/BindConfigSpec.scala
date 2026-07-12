package ph.samson.atbp.liga.serve

import better.files.File
import ph.samson.atbp.liga.serve.Routes as LigaRoutes
import ph.samson.atbp.liga.serve.Server as LigaServer
import zio.http.*
import zio.test.*

import java.net.InetAddress

object BindConfigSpec extends ZIOSpecDefault {

  private val loopback: Option[InetAddress] =
    Some(InetAddress.getLoopbackAddress)

  private val remote: Option[InetAddress] =
    Some(InetAddress.getByName("192.168.1.50"))

  private val ctx = ServeContext(
    dataDir = File(getClass.getResource("/periods")),
    tournamentDir =
      Some(File(getClass.getResource("/tournaments/eight-player-seeded")))
  )

  private def get(path: String, from: Option[InetAddress]): Request =
    Request.get(path).copy(remoteAddress = from)

  def spec = suite("BindConfig")(
    test("default bind host is localhost") {
      val bind = BindConfig()
      assertTrue(bind.bindHost == "127.0.0.1")
    },
    test("--lan bind host is all interfaces") {
      val bind = BindConfig(lan = true)
      assertTrue(bind.bindHost == "0.0.0.0")
    },
    test("httpConfig uses bind host from BindConfig") {
      val localhost = LigaServer.httpConfig(BindConfig())
      val lan = LigaServer.httpConfig(BindConfig(lan = true, port = 9000))
      assertTrue(
        localhost.address.getHostString == "127.0.0.1",
        localhost.address.getPort == 5442,
        lan.address.getHostString == "0.0.0.0",
        lan.address.getPort == 9000
      )
    },
    test("--lan allows read API from non-localhost") {
      val bind = BindConfig(lan = true)
      for {
        tournament <- LigaRoutes
          .routes(ctx, bind)
          .runZIO(get("/api/tournament", remote))
        audience <- LigaRoutes
          .routes(ctx, bind)
          .runZIO(get("/audience", remote))
        leaderboard <- LigaRoutes
          .routes(ctx, bind)
          .runZIO(get("/api/leaderboard", remote))
        config <- LigaRoutes
          .routes(ctx, bind)
          .runZIO(get("/api/config", remote))
      } yield assertTrue(
        tournament.status == Status.Ok,
        audience.status == Status.Ok,
        leaderboard.status == Status.Ok,
        config.status == Status.Ok
      )
    },
    test("--lan blocks director SPA from non-localhost") {
      val bind = BindConfig(lan = true)
      for {
        response <- LigaRoutes
          .routes(ctx, bind)
          .runZIO(get("/", remote))
      } yield assertTrue(response.status == Status.Forbidden)
    },
    test("--lan still allows director SPA from localhost") {
      val bind = BindConfig(lan = true)
      for {
        response <- LigaRoutes
          .routes(ctx, bind)
          .runZIO(get("/", loopback))
        body <- response.body.asString
      } yield assertTrue(
        response.status == Status.Ok,
        body.contains("Liga Director")
      )
    },
    test("isLocalDirector returns false when remoteAddress is absent") {
      assertTrue(!BindConfig.isLocalDirector(Request.get("/")))
    },
    test("--lan blocks write routes from non-localhost") {
      val bind = BindConfig(lan = true)
      for {
        response <- LigaRoutes
          .routes(ctx, bind)
          .runZIO(
            Request
              .post(
                "/api/tournament/seed",
                Body.fromString("""{"roundRaceTo":{"1":7}}""")
              )
              .copy(remoteAddress = remote)
          )
      } yield assertTrue(response.status == Status.Forbidden)
    }
  )
}
