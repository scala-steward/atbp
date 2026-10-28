package ph.samson.atbp.jira

import zio.Promise
import zio.Ref
import zio.Scope
import zio.Trace
import zio.ZIO
import zio.ZLayer
import zio.http.{Client as _, *}
import zio.test.*

object ClientInterruptionSpec extends ZIOSpecDefault {

  private def clientLayer(
      respond: URL => ZIO[Scope, Throwable, Response],
      concurrentRequests: Int
  ) = {
    val driver = new ZClient.Driver[Any, Scope, Throwable] {
      override def request(
          version: Version,
          method: Method,
          url: URL,
          headers: Headers,
          body: Body,
          sslConfig: Option[ClientSSLConfig],
          proxy: Option[Proxy]
      )(implicit trace: Trace): ZIO[Scope, Throwable, Response] =
        respond(url)

      override def socket[Env1 <: Any](
          version: Version,
          url: URL,
          headers: Headers,
          app: WebSocketApp[Env1]
      )(implicit trace: Trace, ev: Scope =:= Scope) =
        ZIO.dieMessage("Unexpected websocket request")
    }
    ZLayer.succeed(ZClient.fromDriver(driver)) >>> Client.layer(
      Conf(
        "example.atlassian.net",
        "test-user",
        "test-token",
        concurrentRequests
      )
    )
  }

  private def response = Response.json(
    """{"self":"page","startAt":0,"maxResults":100,"total":0,"isLast":true,"values":[]}"""
  )

  override def spec = suite("Jira client interruption")(
    test(
      "a canceled caller lets HTTP work finish while another read succeeds"
    ) {
      ZIO.scoped {
        for {
          started <- Promise.make[Nothing, Unit]
          release <- Promise.make[Nothing, Unit]
          completed <- Promise.make[Nothing, Unit]
          exited <- Promise.make[Nothing, Unit]
          _ <- ZIO.addFinalizer(release.succeed(()).unit)
          respond = (url: URL) =>
            (if (url.path.encode.contains("/FIRST-1/")) {
               (ZIO.uninterruptible {
                 started.succeed(()) *>
                   (ZIO.yieldNow *> ZIO.descriptor)
                     .repeatUntil(_.interrupters.nonEmpty) *>
                   release.await
               } *> completed.succeed(())).ensuring(exited.succeed(()))
             } else ZIO.unit).as(response)
          result <- (for {
            client <- ZIO.service[Client]
            first <- client.getChangelogs("FIRST-1").fork
            _ <- started.await
            // Cancellation and the independent read must complete while the
            // original HTTP operation is still waiting for its release gate.
            interrupted <- first.interrupt
            independent <- client.getChangelogs("SECOND-1").exit
            _ <- release.succeed(())
            _ <- exited.await
            finished <- completed.poll
          } yield assertTrue(
            interrupted.isInterrupted,
            independent.isSuccess,
            finished.nonEmpty
          )).provide(clientLayer(respond, concurrentRequests = 2))
        } yield result
      }
    },
    test("a read canceled while waiting for a permit is never dispatched") {
      ZIO.scoped {
        for {
          started <- Promise.make[Nothing, Unit]
          release <- Promise.make[Nothing, Unit]
          requests <- Ref.make(List.empty[String])
          _ <- ZIO.addFinalizer(release.succeed(()).unit)
          respond = (url: URL) =>
            requests.update(_ :+ url.path.encode) *>
              (if (url.path.encode.contains("/FIRST-1/")) {
                 started.succeed(()) *> release.await
               } else ZIO.unit).as(response)
          result <- (for {
            client <- ZIO.service[Client]
            first <- client.getChangelogs("FIRST-1").fork
            _ <- started.await
            queued <- client.getChangelogs("QUEUED-1").fork
            // Allow the background request to reach the occupied permit.
            _ <- TestClock.adjust(zio.Duration.Zero)
            interrupted <- queued.interrupt
            _ <- release.succeed(())
            _ <- first.join
            _ <- client.getChangelogs("THIRD-1")
            dispatched <- requests.get
          } yield assertTrue(
            interrupted.isInterrupted,
            dispatched.exists(_.contains("/FIRST-1/")),
            dispatched.exists(_.contains("/THIRD-1/")),
            !dispatched.exists(_.contains("/QUEUED-1/"))
          )).provide(clientLayer(respond, concurrentRequests = 1))
        } yield result
      }
    }
  ) @@ TestAspect.timeout(zio.Duration.fromSeconds(20))
}
