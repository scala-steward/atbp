package ph.samson.atbp.jira

import zio.Ref
import zio.Scope
import zio.Trace
import zio.ZIO
import zio.ZLayer
import zio.http.{Client as _, *}
import zio.schema.codec.DecodeError
import zio.test.*

object ResponseDecodingSpec extends ZIOSpecDefault {

  private def isDecodeError(error: Throwable): Boolean = error match {
    case _: DecodeError => true
    case _              => false
  }

  private def clientLayer(response: Response): ZLayer[Any, Throwable, Client] =
    clientLayer(response, None)

  private def clientLayer(response: Response, hits: Option[Ref[Int]]) = {
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
        ZIO.foreachDiscard(hits)(_.update(_ + 1)).as(response)

      override def socket[Env1 <: Any](
          version: Version,
          url: URL,
          headers: Headers,
          app: WebSocketApp[Env1]
      )(implicit trace: Trace, ev: Scope =:= Scope) =
        ZIO.dieMessage("Unexpected websocket request")
    }
    ZLayer.succeed(ZClient.fromDriver(driver)) >>> Client.layer(
      Conf("example.atlassian.net", "test-user", "secret-token", 2)
    )
  }

  override def spec = suite("Jira response decoding")(
    test("decodes changelogs with missing, null and present authors") {
      val body =
        """{"self":"page","startAt":0,"maxResults":100,"total":3,"isLast":true,"values":[
          |{"id":"missing","created":"2026-09-28T00:00:00.000+0000","items":[]},
          |{"id":"null","author":null,"created":"2026-09-28T00:00:00.000+0000","items":[]},
          |{"id":"present","author":{"self":"user","accountId":"123","displayName":"Test","active":true,"timeZone":"UTC","accountType":"atlassian"},"created":"2026-09-28T00:00:00.000+0000","items":[]}
          |]}""".stripMargin
      for {
        changelogs <- ZIO
          .serviceWithZIO[Client](_.getChangelogs("TEST-42"))
          .provide(clientLayer(Response.json(body)))
      } yield assertTrue(
        changelogs.map(_.id) == List("missing", "null", "present"),
        changelogs
          .map(_.author.map(_.displayName)) == List(None, None, Some("Test"))
      )
    },
    test(
      "identifies the request, type and failing object beyond the response prefix"
    ) {
      val valid =
        """{"id":"earlier","author":{"self":"user","accountId":"123","displayName":"Test","active":true,"timeZone":"UTC","accountType":"atlassian"},"created":"2026-09-28T00:00:00.000+0000","items":[]}"""
      val invalid =
        """{"id":"bad-history-73","items":[]}"""
      val values = (List.fill(73)(valid) :+ invalid).mkString(",")
      val body =
        s"""{"self":"page","startAt":0,"maxResults":100,"total":74,"isLast":true,"values":[$values]}"""
      val response =
        Response.json(body).addHeader("X-AREQUESTID", "request-123")
      for {
        hits <- Ref.make(0)
        result <- ZIO
          .serviceWithZIO[Client](_.getChangelogs("TEST-42"))
          .either
          .provide(clientLayer(response, Some(hits)))
        count <- hits.get
      } yield result match {
        case Left(error) =>
          val message = error.getMessage
          assertTrue(
            message.contains(
              "GET https://example.atlassian.net/rest/api/3/issue/TEST-42/changelog"
            ),
            message.contains("PageBean[Changelog]"),
            message.contains("200"),
            message.contains(".values[73].created(missing)"),
            message.contains("bad-history-73"),
            message.contains("request-123"),
            message.contains("\"startAt\":0"),
            message.contains("JSON at $.values[73]"),
            !message.contains("secret-token"),
            !message.contains("earlier"),
            count == 1,
            isDecodeError(error.getCause)
          )
        case Right(_) => assertNever("Invalid changelog must fail")
      }
    },
    test(
      "includes POST search context and the containing object for a wrong field type"
    ) {
      val response = Response.json("""{"isLast":"unexpected","issues":[]}""")
      for {
        error <- ZIO
          .serviceWithZIO[Client](_.search("project = TEST"))
          .flip
          .provide(clientLayer(response))
      } yield assertTrue(
        error.getMessage.contains(
          "POST https://example.atlassian.net/rest/api/3/search/jql"
        ),
        error.getMessage.contains("SearchResults"),
        error.getMessage.contains("page=1"),
        error.getMessage.contains("jql=project = TEST"),
        error.getMessage.contains("JSON at $:"),
        error.getMessage.contains("\"isLast\":\"unexpected\"")
      )
    },
    test("includes comment query parameters and limits oversized excerpts") {
      val response = Response
        .json(s"""{"comments":[],"unexpected":"${"x" * 10000}"}""")
        .addHeader("Set-Cookie", "private-cookie")
      for {
        error <- ZIO
          .serviceWithZIO[Client](_.getComments("TEST-42"))
          .flip
          .provide(clientLayer(response))
      } yield assertTrue(
        error.getMessage.contains("/issue/TEST-42/comment?expand=renderedBody"),
        error.getMessage.contains("PageOfComments"),
        error.getMessage.contains("[truncated]"),
        error.getMessage.length < 5000,
        !error.getMessage.contains("private-cookie")
      )
    },
    test("reports malformed JSON without losing the original decode error") {
      for {
        error <- ZIO
          .serviceWithZIO[Client](_.getIssue("TEST-42"))
          .flip
          .provide(clientLayer(Response.text("<html>bad response</html>\n")))
      } yield assertTrue(
        error.getMessage.contains(
          "GET https://example.atlassian.net/rest/api/3/issue/TEST-42"
        ),
        error.getMessage.contains("as Issue"),
        error.getMessage.contains(
          "Response body excerpt: <html>bad response</html>\\u000a"
        ),
        isDecodeError(error.getCause)
      )
    },
    test("valid responses still decode") {
      for {
        issues <- ZIO
          .serviceWithZIO[Client](_.search("project = TEST"))
          .provide(
            clientLayer(Response.json("""{"isLast":true,"issues":[]}"""))
          )
      } yield assertTrue(issues.isEmpty)
    },
    test("non-success HTTP responses remain status errors") {
      for {
        error <- ZIO
          .serviceWithZIO[Client](_.getIssue("TEST-42"))
          .flip
          .provide(clientLayer(Response.status(Status.Forbidden)))
      } yield error match {
        case _: ph.samson.atbp.http.StatusCheck.BadStatus => assertTrue(true)
        case other => assertNever(s"Expected HTTP status error, got $other")
      }
    }
  )
}
