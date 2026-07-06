package ph.samson.atbp.liga.serve

import zio.*
import zio.http.*
import zio.http.Server as HttpServer

object Server {

  def routes(
      ctx: ServeContext,
      bind: BindConfig
  ): Routes[Any, Response] =
    ph.samson.atbp.liga.serve.Routes.routes(ctx, bind)

  def httpConfig(bind: BindConfig): HttpServer.Config =
    HttpServer.Config.default.binding(bind.bindHost, bind.port)

  /** Start the HTTP server until interrupted; shuts down gracefully on
    * interrupt.
    */
  def run(
      bind: BindConfig,
      ctx: ServeContext
  ): ZIO[Any, Throwable, Nothing] =
    HttpServer
      .serve(routes(ctx, bind))
      .provide(
        ZLayer.succeed(httpConfig(bind)),
        HttpServer.live
      )
}
