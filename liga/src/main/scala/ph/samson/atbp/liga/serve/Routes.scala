package ph.samson.atbp.liga.serve

import zio.*
import zio.http.*
import zio.json.EncoderOps

object Routes {

  def routes(
      ctx: ServeContext,
      bind: BindConfig
  ): Routes[Any, Response] =
    StaticAssets.assetRoutes ++ staticRoutes(bind) ++ readApi(
      ctx,
      bind
    ) ++ DirectorRoutes.routes(ctx)

  private def htmlResponse(html: String): Response =
    Response(
      headers = Headers(Header.ContentType(MediaType.text.html)),
      body = Body.fromString(html)
    )

  private def staticRoutes(bind: BindConfig): Routes[Any, Response] =
    zio.http.Routes(
      Method.GET / "health" -> handler(Response.text("ok")),
      Method.GET / Root -> handler { (req: Request) =>
        if (bind.lan && !bind.isLocalDirector(req)) {
          ZIO.succeed(Response.text("forbidden").status(Status.Forbidden))
        } else {
          ZIO.succeed(htmlResponse(StaticAssets.directorHtml))
        }
      },
      Method.GET / "audience" -> handler(
        htmlResponse(StaticAssets.audienceHtml)
      )
    )

  private def readApi(
      ctx: ServeContext,
      bind: BindConfig
  ): Routes[Any, Response] =
    zio.http.Routes(
      Method.GET / "api" / "config" -> handler(
        Response.json(ApiJson.configFrom(bind).toJson)
      ),
      Method.GET / "api" / "tournament" -> handler {
        (for {
          state <- ctx.loadTournament
          hasDir <- ctx.hasActiveDir
        } yield Response.json(
          ApiJson.tournamentFrom(state, hasDir).toJson
        )).catchAll(err =>
          ZIO.succeed(
            Response
              .text(err.getMessage)
              .status(Status.InternalServerError)
          )
        )
      },
      Method.GET / "api" / "leaderboard" -> handler {
        (for {
          state <- ctx.loadTournament
          ratings <- ctx.loadLeaderboard(state)
        } yield Response.json(ApiJson.leaderboardFrom(ratings).toJson))
          .catchAll(err =>
            ZIO.succeed(
              Response
                .text(err.getMessage)
                .status(Status.InternalServerError)
            )
          )
      }
    )
}
