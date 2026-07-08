package ph.samson.atbp.liga.serve

import better.files.File
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.serve.ApiJson.*
import ph.samson.atbp.liga.serve.Routes as LigaRoutes
import ph.samson.atbp.liga.tournament.EventCodec
import ph.samson.atbp.liga.tournament.EventLog
import ph.samson.atbp.liga.tournament.events.TournamentEvent
import zio.*
import zio.http.*
import zio.json.*
import zio.test.*

import java.net.InetAddress
import java.time.Instant

object WriteApiSpec extends ZIOSpecDefault {

  private val at = Instant.parse("2026-03-15T18:00:00Z")

  private val loopback: Option[InetAddress] =
    Some(InetAddress.getLoopbackAddress)

  private val remote: Option[InetAddress] =
    Some(InetAddress.getByName("192.168.1.50"))

  private def localhostPost(path: String, body: String): Request =
    Request
      .post(path, Body.fromString(body))
      .copy(remoteAddress = loopback)

  private def remotePost(path: String, body: String): Request =
    Request
      .post(path, Body.fromString(body))
      .copy(remoteAddress = remote)

  private def withTempTournament(
      seeded: Boolean
  ): ZIO[Any, Throwable, (ServeContext, File)] =
    ZIO.attempt {
      val root = File.newTemporaryDirectory("liga-write-api")
      val dataDir = root / "data"
      val tournamentDir = root / "tournament-test"
      dataDir.createDirectoryIfNotExists()
      tournamentDir.createDirectoryIfNotExists()
      File(getClass.getResource("/periods/eight-player-seed.liga"))
        .copyTo(dataDir / "eight-player-seed.liga")
      val players = (1 to 8).map(i => Player(s"P$i")).toList
      val wizardEvents = List(
        TournamentEvent.Created(
          seq = 1,
          at = at,
          payload = TournamentCreatedPayload(
            name = "Spring Open",
            players = Nil
          )
        ),
        TournamentEvent.PlayersSet(
          seq = 2,
          at = at,
          payload = PlayersSetPayload(players = players)
        ),
        TournamentEvent.PlayersLocked(
          seq = 3,
          at = at,
          payload = PlayersLockedPayload()
        )
      )
      if (seeded) {
        File(getClass.getResource("/tournaments/eight-player-seeded"))
          .copyTo(tournamentDir, overwrite = true)
      } else {
        wizardEvents.foreach { event =>
          (tournamentDir / EventLog.filenameFor(event))
            .write(EventCodec.encode(event))
        }
      }
      val ctx =
        ServeContext(dataDir = dataDir, tournamentDir = Some(tournamentDir))
      (ctx, root)
    }

  private def cleanup(root: File): UIO[Unit] =
    ZIO.attemptBlocking(root.delete(swallowIOExceptions = true)).unit.orDie

  def spec = suite("WriteApi")(
    test("wizard create → players → lock → race-to → seed flow") {
      for {
        root <- ZIO.attemptBlocking(
          File.newTemporaryDirectory("liga-wizard-api")
        )
        dataDir = root / "data"
        _ <- ZIO.attemptBlocking {
          dataDir.createDirectoryIfNotExists()
          File(getClass.getResource("/periods/eight-player-seed.liga"))
            .copyTo(dataDir / "eight-player-seed.liga")
        }
        ctx = ServeContext(dataDir = dataDir, tournamentDir = None)
        playersJson = (1 to 8).map(i => s"""{"name":"P$i"}""").mkString("[", ",", "]")
        create <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(
            localhostPost(
              "/api/tournament/create",
              """{"name":"Spring Open"}"""
            )
          )
        createdBody <- create.body.asString
        created <- ZIO.fromEither(createdBody.fromJson[TournamentResponse])
        _ <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(
            localhostPost(
              "/api/tournament/players",
              s"""{"players":$playersJson}"""
            )
          )
        _ <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(localhostPost("/api/tournament/lock", "{}"))
        _ <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(
            localhostPost(
              "/api/tournament/race-to",
              """{"roundRaceTo":{"1":7,"2":7,"3":7,"4":7}}"""
            )
          )
        seed <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(localhostPost("/api/tournament/seed", "{}"))
        seedBody <- seed.body.asString
        seeded <- ZIO.fromEither(seedBody.fromJson[TournamentResponse])
        _ <- cleanup(root)
      } yield assertTrue(
        create.status == Status.Ok,
        created.phase == "defining",
        seed.status == Status.Ok,
        seeded.phase == "active",
        seeded.bracket.exists(_.size == 8)
      )
    },
    test(
      "POST /api/tournament/seed creates bracket with frozen period ratings"
    ) {
      for {
        (ctx, root) <- withTempTournament(seeded = false)
        seedBody = """{"roundRaceTo":{"1":7,"2":7,"3":7,"4":7}}"""
        response <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(localhostPost("/api/tournament/seed", seedBody))
        body <- response.body.asString
        parsed <- ZIO.fromEither(body.fromJson[TournamentResponse])
        seededFileExists = (ctx.tournamentDir.get / "000008-seeded.json").exists
        _ <- cleanup(root)
      } yield assertTrue(
        response.status == Status.Ok,
        parsed.bracket.exists(_.size == 8),
        parsed.frozenRatings.size == 8,
        parsed.frozenRatings.map(_.player.name).sorted == (1 to 8)
          .map(i => s"P$i")
          .toList,
        parsed.roundRaceTo == Map(1 -> 7, 2 -> 7, 3 -> 7, 4 -> 7),
        seededFileExists
      )
    },
    test("seed → ready → handicap → start → result flow") {
      for {
        (ctx, root) <- withTempTournament(seeded = false)
        seed <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(
            localhostPost(
              "/api/tournament/seed",
              """{"roundRaceTo":{"1":7,"2":7,"3":7,"4":7}}"""
            )
          )
        ready <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(localhostPost("/api/matches/wb-1-1/ready", ""))
        readyBody <- ready.body.asString
        afterReady <- ZIO.fromEither(readyBody.fromJson[TournamentResponse])
        handicap <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(
            localhostPost(
              "/api/matches/wb-1-1/handicap",
              """{"handicap":3}"""
            )
          )
        start <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(localhostPost("/api/matches/wb-1-1/start", ""))
        result <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(
            localhostPost(
              "/api/matches/wb-1-1/result",
              """{"scoreA":7,"scoreB":4}"""
            )
          )
        resultBody <- result.body.asString
        finalState <- ZIO.fromEither(resultBody.fromJson[TournamentResponse])
        matchDef = finalState.bracket
          .flatMap(_.matches.find(_.id == "wb-1-1"))
          .get
        _ <- cleanup(root)
      } yield assertTrue(
        seed.status == Status.Ok,
        ready.status == Status.Ok,
        afterReady.bracket
          .flatMap(_.matches.find(_.id == "wb-1-1"))
          .flatMap(_.handicapSuggested)
          .nonEmpty,
        handicap.status == Status.Ok,
        start.status == Status.Ok,
        result.status == Status.Ok,
        matchDef.state == BracketMatchState.Completed,
        matchDef.handicapApplied.contains(3),
        matchDef.result.contains(MatchResult(7, 4))
      )
    },
    test("write routes reject non-localhost requests with 403") {
      for {
        (ctx, root) <- withTempTournament(seeded = false)
        response <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(
            remotePost(
              "/api/tournament/seed",
              """{"roundRaceTo":{"1":7,"2":7,"3":7,"4":7}}"""
            )
          )
        _ <- cleanup(root)
      } yield assertTrue(response.status == Status.Forbidden)
    },
    test("ready rejects illegal transition with 400") {
      for {
        (ctx, root) <- withTempTournament(seeded = false)
        _ <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(
            localhostPost(
              "/api/tournament/seed",
              """{"roundRaceTo":{"1":7,"2":7,"3":7,"4":7}}"""
            )
          )
        response <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(localhostPost("/api/matches/wb-1-1/ready", ""))
        again <- LigaRoutes
          .routes(ctx, BindConfig())
          .runZIO(localhostPost("/api/matches/wb-1-1/ready", ""))
        _ <- cleanup(root)
      } yield assertTrue(
        response.status == Status.Ok,
        again.status == Status.BadRequest
      )
    }
  )
}
