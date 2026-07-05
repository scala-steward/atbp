package ph.samson.atbp.liga.serve

import zio.*
import zio.http.*
import zio.http.Server as HttpServer

object Server {

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

  val routes: Routes[Any, Response] = Routes(
    Method.GET / "health" -> handler(Response.text("ok")),
    Method.GET / Root -> handler(Response.html(directorPlaceholder)),
    Method.GET / "audience" -> handler(Response.html(audiencePlaceholder))
  )

  def httpConfig(config: ServeConfig): HttpServer.Config =
    HttpServer.Config.default.binding(config.host, config.port)

  /** Start the HTTP server until interrupted; shuts down gracefully on interrupt. */
  def run(config: ServeConfig): ZIO[Any, Throwable, Nothing] =
    HttpServer.serve(routes).provide(
      ZLayer.succeed(httpConfig(config)),
      HttpServer.live
    )
}
