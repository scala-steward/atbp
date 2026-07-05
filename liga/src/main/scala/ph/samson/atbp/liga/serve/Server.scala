package ph.samson.atbp.liga.serve

import zio.*
import zio.http.*
import zio.http.Server as HttpServer

object Server {

  def routes(ctx: ServeContext): Routes[Any, Response] =
    ph.samson.atbp.liga.serve.Routes.routes(ctx)

  def httpConfig(config: ServeConfig): HttpServer.Config =
    HttpServer.Config.default.binding(config.host, config.port)

  /** Start the HTTP server until interrupted; shuts down gracefully on
    * interrupt.
    */
  def run(
      config: ServeConfig,
      ctx: ServeContext
  ): ZIO[Any, Throwable, Nothing] =
    HttpServer
      .serve(routes(ctx))
      .provide(
        ZLayer.succeed(httpConfig(config)),
        HttpServer.live
      )
}
