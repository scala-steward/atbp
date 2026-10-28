package ph.samson.atbp.plate

import better.files.File
import ph.samson.atbp.jira.Client
import ph.samson.atbp.jira.Conf
import zio.Scope
import zio.Trace
import zio.ZIO
import zio.ZLayer
import zio.durationInt
import zio.http.{Client as _, *}
import zio.stream.ZStream
import zio.test.*

import java.time.ZoneOffset
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter

object StaleInterruptionSpec extends ZIOSpecDefault {

  private val emptyChangelog =
    """{"self":"page","startAt":0,"maxResults":100,"total":0,"isLast":true,"values":[]}"""
  private def progressChangelog(created: String) =
    s"""{"self":"page","startAt":0,"maxResults":100,"total":1,"isLast":true,"values":[{"id":"1","created":"$created","items":[{"field":"status","toString":"In Progress"}]}]}"""
  private val emptyComments =
    """{"startAt":0,"maxResults":100,"total":0,"comments":[]}"""

  private def issue(key: String) =
    s"""{"id":"$key","key":"$key","self":"https://example.atlassian.net/rest/api/3/issue/$key","fields":{"summary":"Test","statuscategorychangedate":"2026-01-01T00:00:00.000+0000","created":"2026-01-01T00:00:00.000+0000","updated":"2026-01-01T00:00:00.000+0000","versions":[],"fixVersions":[],"labels":[],"issuetype":{"self":"type","id":"1","name":"Task","hierarchyLevel":0},"status":{"id":"3","name":"In Progress","description":"","statusCategory":{"id":4,"key":"indeterminate","name":"In Progress"}}}}"""

  private def localClient(port: Int) = ZLayer.fromZIO {
    ZIO.serviceWith[zio.http.Client] { client =>
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
          client.driver.request(
            version,
            method,
            url.scheme(Scheme.HTTP).host("127.0.0.1").port(port),
            headers,
            body,
            None,
            None
          )

        override def socket[Env1 <: Any](
            version: Version,
            url: URL,
            headers: Headers,
            app: WebSocketApp[Env1]
        )(implicit trace: Trace, ev: Scope =:= Scope) =
          ZIO.dieMessage("Unexpected websocket request")
      }
      client.transform(client.bodyEncoder, client.bodyDecoder, driver)
    }
  }

  override def spec = suite("Stale interruption")(
    test(
      "pooled local HTTP tolerates repeated parallel descendant cancellation"
    ) {
      val recentProgress = progressChangelog(
        DateTimeFormatter
          .ofPattern("yyyy-MM-dd'T'HH:mm:ss.SSSZ")
          .format(ZonedDateTime.now(ZoneOffset.UTC))
      )
      val routes = Routes(
        RoutePattern.any -> handler { (_: Path, request: Request) =>
          val path = request.url.path.encode
          if (path.endsWith("/search/jql")) {
            request.body.asString.orDie.map { query =>
              val key = "ROOT-\\d+".r.findFirstIn(query)
              val children = key
                .map { parent =>
                  val id = parent.stripPrefix("ROOT-")
                  val descendants =
                    if (id.toInt % 2 == 0) List(issue(s"WIN-$id"))
                    else List(issue(s"WIN-$id"), issue(s"SLOW-$id"))
                  descendants.mkString(",")
                }
                .getOrElse("")
              Response.json(s"""{"isLast":true,"issues":[$children]}""")
            }
          } else if (path.contains("/SLOW-") && path.endsWith("/changelog")) {
            val delayed = ZStream
              .fromZIO(
                ZIO
                  .sleep(50.millis)
                  .as(zio.Chunk.fromArray(emptyChangelog.getBytes))
              )
              .flattenChunks
            ZIO.succeed(
              Response.json("").copy(body = Body.fromStreamChunked(delayed))
            )
          } else if (path.contains("/WIN-") && path.endsWith("/changelog")) {
            ZIO.succeed(Response.json(recentProgress))
          } else if (path.endsWith("/changelog")) {
            ZIO.succeed(Response.json(emptyChangelog))
          } else if (path.endsWith("/comment")) {
            ZIO.succeed(Response.json(emptyComments))
          } else {
            ZIO.succeed(Response.json(issue(path.split('/').last)))
          }
        }
      )
      ZIO
        .scoped {
          for {
            port <- Server.install(routes)
            source <- ZIO.acquireRelease(
              ZIO.attemptBlockingIO {
                // Saturate the default ten-connection pool while race losers
                // release connections and unrelated issue checks continue.
                val lines = (1 to 40).map { i =>
                  s"- [Parent $i](https://example.atlassian.net/browse/ROOT-$i)"
                }
                File
                  .newTemporaryFile("stale-stress", ".md")
                  .overwrite(
                    lines.mkString("# Plate\n", "\n", "\n")
                  )
              }
            )(file => ZIO.attemptBlockingIO(file.delete()).orDie)
            target <- ZIO.acquireRelease(
              ZIO.attemptBlockingIO(
                File.newTemporaryFile("stale-stress-result", ".md")
              )
            )(file => ZIO.attemptBlockingIO(file.delete()).orDie)
            _ <- ZIO
              .foreachDiscard(1 to 100) { iteration =>
                ZIO
                  .serviceWithZIO[Inspector](_.stale(source, Some(target)))
                  .withParallelism(100)
                  .flatMap(result =>
                    ZIO.attemptBlockingIO(result.contentAsString)
                  )
                  .flatMap { output =>
                    ZIO
                      .fail(
                        new Exception(
                          s"Unexpected stale output at iteration $iteration: $output"
                        )
                      )
                      .unless(output.isEmpty)
                  }
              }
              .provide(
                ZClient.default >>> localClient(port),
                Client.layer(
                  Conf("example.atlassian.net", "test-user", "test-token", 100)
                ),
                Inspector.layer()
              )
          } yield assertTrue(true)
        }
        .provide(Server.defaultWithPort(0))
    } @@ TestAspect.withLiveClock
  ) @@ TestAspect.timeout(zio.Duration.fromSeconds(60))
}
