package ph.samson.atbp.liga.serve

import better.files.File
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.serve.ApiJson.*
import ph.samson.atbp.liga.serve.Routes as LigaRoutes
import zio.*
import zio.http.*
import zio.json.*
import zio.test.*

object ReadApiSpec extends ZIOSpecDefault {

  private def fixtureTournament(name: String): File =
    File(getClass.getResource(s"/tournaments/$name"))

  private def fixturePeriods: File =
    File(getClass.getResource("/periods"))

  private def context(
      dataDir: File,
      tournamentName: String
  ): ServeContext =
    ServeContext(
      dataDir = dataDir,
      tournamentDir = fixtureTournament(tournamentName)
    )

  def spec = suite("ReadApi")(
    test("GET /api/tournament returns bracket and match handicaps") {
      val ctx = context(fixturePeriods, "eight-player-partial")
      for {
        response <- LigaRoutes
          .routes(ctx)
          .runZIO(Request.get("/api/tournament"))
        body <- response.body.asString
        parsed <- ZIO.fromEither(body.fromJson[TournamentResponse])
        matchDef = parsed.bracket
          .flatMap(_.matches.find(_.id == "wb-1-1"))
          .get
      } yield assertTrue(
        response.status == Status.Ok,
        response
          .header(Header.ContentType)
          .exists(_.mediaType == MediaType.application.json),
        parsed.name == "Spring Open",
        parsed.players.size == 8,
        !parsed.completed,
        parsed.bracket.exists(_.size == 8),
        matchDef.state == BracketMatchState.Started,
        matchDef.handicapSuggested.contains(2),
        matchDef.handicapApplied.contains(3)
      )
    },
    test("GET /api/leaderboard returns period-start frozen ratings") {
      val ctx = context(fixturePeriods, "eight-player-seeded")
      for {
        response <- LigaRoutes
          .routes(ctx)
          .runZIO(Request.get("/api/leaderboard"))
        body <- response.body.asString
        parsed <- ZIO.fromEither(body.fromJson[LeaderboardResponse])
      } yield assertTrue(
        response.status == Status.Ok,
        parsed.ratings.size == 8,
        parsed.ratings
          .map(_.player.name)
          .sorted == (1 to 8).map(i => s"P$i").toList,
        parsed.ratings.find(_.player.name == "P1").exists(_.rating == 1690.0)
      )
    },
    test("GET /api/leaderboard falls back to period files before seeding") {
      for {
        unseeded <- ZIO.attempt {
          val dir = File.newTemporaryDirectory("liga-read-api")
          val target = dir / "tournament-test"
          target.createDirectoryIfNotExists()
          File(
            getClass.getResource(
              "/tournaments/eight-player-seeded/000001-created.json"
            )
          ).copyTo(target / "000001-created.json")
          ServeContext(dataDir = fixturePeriods, tournamentDir = target)
        }
        response <- LigaRoutes
          .routes(unseeded)
          .runZIO(Request.get("/api/leaderboard"))
        body <- response.body.asString
        parsed <- ZIO.fromEither(body.fromJson[LeaderboardResponse])
        _ <- ZIO.attemptBlocking(
          unseeded.tournamentDir.delete(swallowIOExceptions = true)
        )
      } yield assertTrue(
        parsed.ratings.nonEmpty,
        parsed.ratings.exists(_.player.name == "Alice")
      )
    },
    test("TournamentResponse round-trips through JSON codec") {
      val sample = TournamentResponse(
        name = "Spring Open",
        players = List(Player("Alice"), Player("Bob")),
        completed = false,
        roundRaceTo = Map(1 -> 7),
        bracket = None,
        frozenRatings = List(
          PlayerRating(Player("Alice"), 1500, 350, 0, 0)
        )
      )
      val json = sample.toJson
      val parsed = json.fromJson[TournamentResponse]
      assertTrue(parsed == Right(sample), json.contains("\"roundRaceTo\""))
    }
  )
}
