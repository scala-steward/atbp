package ph.samson.atbp.liga.js

import better.files.File
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.serve.ApiJson.{*, given}
import ph.samson.atbp.liga.serve.BindConfig
import ph.samson.atbp.liga.serve.DirectorRoutes
import ph.samson.atbp.liga.serve.Routes as LigaRoutes
import ph.samson.atbp.liga.serve.ServeContext
import zio.*
import zio.http.*
import zio.json.*
import zio.test.*

/** JVM contract tests for `liga-js` API models and response decoding. */
object ApiClientContractSpec extends ZIOSpecDefault {

  private val tournamentFixture =
    """{
      |  "name": "Spring Open",
      |  "players": [{"name": "P1"}, {"name": "P2"}],
      |  "completed": false,
      |  "roundRaceTo": {"1": 7},
      |  "bracket": {
      |    "size": 2,
      |    "matches": [{
      |      "id": "wb-1-1",
      |      "playerA": {"name": "P1"},
      |      "playerB": {"name": "P2"},
      |      "state": "Started",
      |      "raceTo": 7,
      |      "handicapSuggested": 2,
      |      "handicapApplied": 3
      |    }]
      |  },
      |  "frozenRatings": [{
      |    "player": {"name": "P1"},
      |    "rating": 1690.0,
      |    "rd": 100.0,
      |    "wins": 0,
      |    "losses": 0
      |  }]
      |}""".stripMargin

  private val leaderboardFixture =
    """{
      |  "ratings": [{
      |    "player": {"name": "Alice"},
      |    "rating": 1500.0,
      |    "rd": 350.0,
      |    "wins": 12,
      |    "losses": 8
      |  }]
      |}""".stripMargin

  private def decodeResponse[T: JsonDecoder](
      status: Int,
      body: String
  ): Either[String, T] =
    if (status >= 200 && status < 300) {
      body.fromJson[T]
    } else {
      Left(s"HTTP $status: $body")
    }

  def spec = suite("ApiClientContract")(
    test("tournament fixture matches JS client JSON schema") {
      val parsed = tournamentFixture.fromJson[TournamentResponse]
      assertTrue(
        parsed.isRight,
        parsed.exists(_.name == "Spring Open"),
        parsed.exists(_.roundRaceTo == Map(1 -> 7)),
        parsed.exists(
          _.bracket.exists(_.matches.head.state == BracketMatchState.Started)
        ),
        parsed.exists(
          _.bracket.exists(_.matches.head.handicapApplied.contains(3))
        )
      )
    },
    test("leaderboard fixture matches JS client JSON schema") {
      val parsed = leaderboardFixture.fromJson[LeaderboardResponse]
      assertTrue(
        parsed.isRight,
        parsed.exists(_.ratings.head.player.name == "Alice"),
        parsed.exists(_.ratings.head.wins == 12)
      )
    },
    test("live GET /api/tournament JSON parses for JS client") {
      val ctx = ServeContext(
        dataDir = File(getClass.getResource("/periods")),
        tournamentDir =
          Some(File(getClass.getResource("/tournaments/eight-player-partial")))
      )
      for {
        response <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(Request.get("/api/tournament"))
        body <- response.body.asString
        parsed <- ZIO.fromEither(
          decodeResponse[TournamentResponse](response.status.code, body)
        )
      } yield assertTrue(
        response.status == Status.Ok,
        parsed.name == "Spring Open",
        parsed.bracket.exists(_.matches.nonEmpty)
      )
    },
    test("decodeResponse surfaces HTTP and JSON failures") {
      val ok =
        decodeResponse[TournamentResponse](
          200,
          """{"name":"T","players":[],"completed":false,"roundRaceTo":{},"bracket":null,"frozenRatings":[]}"""
        )
      val httpErr = decodeResponse[TournamentResponse](403, "forbidden")
      val jsonErr = decodeResponse[TournamentResponse](200, "not json")
      assertTrue(
        ok.exists(_.name == "T"),
        httpErr == Left("HTTP 403: forbidden"),
        jsonErr.isLeft
      )
    },
    test("director POST request bodies match JS client encodings") {
      import DirectorRoutes.*
      assertTrue(
        SeedRequest(Map(1 -> 7, 2 -> 5)).toJson.contains("\"roundRaceTo\""),
        HandicapRequest(3).toJson == """{"handicap":3}""",
        ResultRequest(7, 4).toJson == """{"scoreA":7,"scoreB":4}"""
      )
    }
  )
}
