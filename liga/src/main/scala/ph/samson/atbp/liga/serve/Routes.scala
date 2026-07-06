package ph.samson.atbp.liga.serve

import zio.*
import zio.http.*
import zio.json.EncoderOps

object Routes {

  def routes(
      ctx: ServeContext,
      bind: BindConfig
  ): Routes[Any, Response] =
    staticRoutes(bind) ++ readApi(ctx) ++ DirectorRoutes.routes(ctx)

  private val directorPlaceholder =
    """<!DOCTYPE html>
      |<html lang="en">
      |<head><meta charset="utf-8"><title>Liga</title></head>
      |<body><p>Liga director UI (coming soon)</p></body>
      |</html>""".stripMargin

  private val audiencePlaceholder =
    """<!DOCTYPE html>
      |<html lang="en">
      |<head><meta charset="utf-8"><title>Liga Audience</title></head>
      |<body><p>Liga audience display (coming soon)</p></body>
      |</html>""".stripMargin

  private def staticRoutes(bind: BindConfig): Routes[Any, Response] =
    zio.http.Routes(
      Method.GET / "health" -> handler(Response.text("ok")),
      Method.GET / Root -> handler { (req: Request) =>
        if (bind.lan && !bind.isLocalDirector(req)) {
          ZIO.succeed(Response.text("forbidden").status(Status.Forbidden))
        } else {
          ZIO.succeed(Response.html(directorPlaceholder))
        }
      },
      Method.GET / "audience" -> handler(Response.html(audiencePlaceholder))
    )

  private def readApi(ctx: ServeContext): Routes[Any, Response] =
    zio.http.Routes(
      Method.GET / "api" / "tournament" -> handler {
        ctx.loadTournament
          .map(state => Response.json(ApiJson.tournamentFrom(state).toJson))
          .catchAll(err =>
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
