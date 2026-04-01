package ph.samson.atbp.http

import zio.Scope
import zio.Semaphore
import zio.Trace
import zio.UIO
import zio.ZIO
import zio.http
import zio.http.Body
import zio.http.ClientSSLConfig
import zio.http.Headers
import zio.http.Method
import zio.http.Response
import zio.http.URL
import zio.http.Version
import zio.http.WebSocketApp
import zio.http.ZClient
import zio.http.ZClientAspect

object ConcurrencyLimiter {

  private type Aspect =
    ZClientAspect[Nothing, Any, Nothing, Body, Nothing, Any, Nothing, Response]

  def apply(limit: Int): UIO[Aspect] = for {
    limiter <- Semaphore.make(limit)
  } yield new Aspect {

    override def apply[
        ReqEnv,
        Env >: Nothing <: Any,
        In >: Nothing <: Body,
        Err >: Nothing <: Any,
        Out >: Nothing <: Response
    ](
        client: ZClient[Env, ReqEnv, In, Err, Out]
    ): ZClient[Env, ReqEnv, In, Err, Out] = {

      val oldDriver = client.driver

      val newDriver = new ZClient.Driver[Env, ReqEnv, Err] {

        override def request(
            version: Version,
            method: Method,
            url: URL,
            headers: Headers,
            body: Body,
            sslConfig: Option[ClientSSLConfig],
            proxy: Option[http.Proxy]
        )(implicit trace: Trace): ZIO[Env & ReqEnv, Err, Response] = {
          for {
            response <- limiter.withPermit(
              oldDriver.request(
                version,
                method,
                url,
                headers,
                body,
                sslConfig,
                proxy
              )
            )
          } yield response
        }

        override def socket[Env1 <: Env](
            version: Version,
            url: URL,
            headers: Headers,
            app: WebSocketApp[Env1]
        )(implicit
            trace: Trace,
            ev: ReqEnv =:= Scope
        ): ZIO[Env1 & ReqEnv, Err, Response] =
          client.driver.socket(version, url, headers, app)
      }

      client.transform(client.bodyEncoder, client.bodyDecoder, newDriver)
    }
  }
}
