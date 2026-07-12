package ph.samson.atbp.liga

import better.files.File
import ph.samson.atbp.liga.bracket.BracketRounds
import ph.samson.atbp.liga.cli.LeaderboardRenderer
import ph.samson.atbp.liga.io.PeriodLoader
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.serve.ApiJson.*
import ph.samson.atbp.liga.serve.BindConfig
import ph.samson.atbp.liga.serve.Routes as LigaRoutes
import ph.samson.atbp.liga.serve.ServeContext
import ph.samson.atbp.liga.tournament.EventCodec
import ph.samson.atbp.liga.tournament.EventLog
import ph.samson.atbp.liga.tournament.PeriodEmission
import ph.samson.atbp.liga.tournament.Resume
import ph.samson.atbp.liga.tournament.events.TournamentEvent
import zio.*
import zio.http.*
import zio.json.*
import zio.test.*

import java.net.InetAddress
import java.time.Instant
import java.time.LocalDate

/** End-to-end verification: tournament completion, period emission,
  * leaderboard.
  */
object EndToEndSpec extends ZIOSpecDefault {

  private val at = Instant.parse("2026-03-15T18:00:00Z")
  private val completed = LocalDate.parse("2026-03-15")

  private val loopback: Option[InetAddress] =
    Some(InetAddress.getLoopbackAddress)

  private def localhostGet(path: String): Request =
    Request.get(path).copy(remoteAddress = loopback)

  private def localhostPost(path: String, body: String): Request =
    Request
      .post(path, Body.fromString(body))
      .copy(remoteAddress = loopback)

  private def routes(ctx: ServeContext): Routes[Any, Response] =
    LigaRoutes.routes(ctx, BindConfig())

  private def parseTournament(body: String): Task[TournamentResponse] =
    ZIO.fromEither(
      body
        .fromJson[TournamentResponse]
        .left
        .map(msg => new RuntimeException(msg))
    )

  private def withTempDataDir[R, A](
      f: File => ZIO[R, Throwable, A]
  ): ZIO[R, Throwable, A] =
    ZIO.acquireReleaseWith(
      ZIO.attemptBlocking(File.newTemporaryDirectory("liga-e2e"))
    )(root => ZIO.attemptBlocking(root.delete()).ignore) { root =>
      val dataDir = root / "data"
      ZIO.attemptBlocking(dataDir.createDirectoryIfNotExists()).flatMap { _ =>
        f(dataDir)
      }
    }

  private def seedPeriodFile(dataDir: File): Unit = {
    File(getClass.getResource("/periods/eight-player-seed.liga"))
      .copyTo(dataDir / "eight-player-seed.liga")
    ()
  }

  private def seedBody(playerCount: Int): String = {
    val rounds = BracketRounds.requiredKeys(playerCount)
    val raceTo = rounds.map(r => s""""$r":7""").mkString(",")
    s"""{"roundRaceTo":{$raceTo}}"""
  }

  private def writeCreatedTournament(dataDir: File, playerCount: Int): File =
    writeCreatedTournament(dataDir, playerCount, "Spring Open")

  private def writeCreatedTournament(
      dataDir: File,
      playerCount: Int,
      name: String
  ): File = {
    val tournamentDir = dataDir / "tournament-20260315-spring-open"
    tournamentDir.createDirectoryIfNotExists()
    val players = (1 to playerCount).map(i => Player(s"P$i")).toList
    val events = List(
      TournamentEvent.Created(
        seq = 1,
        at = at,
        payload = TournamentCreatedPayload(name = name, players = Nil)
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
    events.foreach { event =>
      (tournamentDir / EventLog.filenameFor(event))
        .write(EventCodec.encode(event))
    }
    tournamentDir
  }

  private def freshContext(dataDir: File, tournamentDir: File): ServeContext =
    ServeContext(dataDir = dataDir, tournamentDir = Some(tournamentDir))

  private def postTournament(
      ctx: ServeContext,
      path: String
  ): ZIO[Scope, Throwable, TournamentResponse] =
    postTournament(ctx, path, "")

  private def postTournament(
      ctx: ServeContext,
      path: String,
      body: String
  ): ZIO[Scope, Throwable, TournamentResponse] =
    for {
      response <- routes(ctx).runZIO(localhostPost(path, body))
      text <- response.body.asString
      _ <- ZIO.when(response.status != Status.Ok) {
        ZIO.fail(
          new RuntimeException(s"$path returned ${response.status}: $text")
        )
      }
      parsed <- parseTournament(text)
    } yield parsed

  private def playMatch(
      ctx: ServeContext,
      matchId: String
  ): ZIO[Scope, Throwable, TournamentResponse] =
    playMatch(ctx, matchId, winnerIsA = true)

  private def playMatch(
      ctx: ServeContext,
      matchId: String,
      winnerIsA: Boolean
  ): ZIO[Scope, Throwable, TournamentResponse] =
    for {
      afterReady <- postTournament(ctx, s"/api/matches/$matchId/ready")
      suggested = afterReady.bracket
        .flatMap(_.matches.find(_.id == matchId))
        .flatMap(_.handicapSuggested)
        .getOrElse(0)
      _ <- postTournament(
        ctx,
        s"/api/matches/$matchId/handicap",
        s"""{"handicap":$suggested}"""
      )
      _ <- postTournament(ctx, s"/api/matches/$matchId/start")
      (scoreA, scoreB) = if (winnerIsA) (7, 4) else (4, 7)
      afterResult <- postTournament(
        ctx,
        s"/api/matches/$matchId/result",
        s"""{"scoreA":$scoreA,"scoreB":$scoreB}"""
      )
    } yield afterResult

  private def playAllReadyMatches(
      ctx: ServeContext
  ): ZIO[Scope, Throwable, TournamentResponse] = {
    def loop(
        state: TournamentResponse
    ): ZIO[Scope, Throwable, TournamentResponse] = {
      val readyIds =
        state.bracket.toList
          .flatMap(_.matches)
          .filter(_.state == BracketMatchState.Ready)
          .map(_.id)
      if (readyIds.isEmpty) {
        ZIO.succeed(state)
      } else {
        for {
          afterRound <- ZIO.foldLeft(readyIds)(state) { (_, matchId) =>
            playMatch(ctx, matchId)
          }
          next <- loop(afterRound)
        } yield next
      }
    }

    for {
      response <- routes(ctx).runZIO(localhostGet("/api/tournament"))
      body <- response.body.asString
      initial <- parseTournament(body)
      finalState <- loop(initial)
    } yield finalState
  }

  private def renderLeaderboard(dataDir: File): Task[String] =
    PeriodLoader.loadAll(dataDir).map(LeaderboardRenderer.render)

  private def allMatchesCompleted(state: TournamentResponse): Boolean =
    state.bracket.exists(
      _.matches.forall(_.state == BracketMatchState.Completed)
    )

  def spec = suite("EndToEnd")(
    test(
      "leaderboard CLI output is bit-for-bit reproducible after period emission"
    ) {
      withTempDataDir { dataDir =>
        for {
          _ <- ZIO.attemptBlocking(seedPeriodFile(dataDir))
          before <- renderLeaderboard(dataDir)
          _ <- ZIO.attemptBlocking(
            writeCreatedTournament(dataDir, playerCount = 8)
          )
          resolved <- Resume.resolve(dataDir)
          ctx = freshContext(dataDir, resolved.get)
          _ <- postTournament(ctx, "/api/tournament/seed", seedBody(8))
          finalState <- playAllReadyMatches(ctx)
          completedState <- postTournament(
            ctx,
            "/api/tournament/complete",
            s"""{"completed":"$completed"}"""
          )
          files <- ZIO.attemptBlocking(dataDir.list.map(_.name).toList)
          emitted = dataDir / PeriodEmission.periodFilename(
            "Spring Open",
            completed
          )
          first <- renderLeaderboard(dataDir)
          second <- renderLeaderboard(dataDir)
        } yield assertTrue(
          allMatchesCompleted(finalState),
          completedState.completed,
          files.contains(emitted.name),
          first == second,
          first != before,
          first.linesIterator.toList.nonEmpty
        )
      }
    },
    test("sixteen-player tournament completes via HTTP API") {
      withTempDataDir { dataDir =>
        for {
          _ <- ZIO.attemptBlocking(seedPeriodFile(dataDir))
          tournamentDir <- ZIO.attemptBlocking(
            writeCreatedTournament(dataDir, playerCount = 16)
          )
          resolved <- Resume.resolve(dataDir)
          ctx = freshContext(dataDir, resolved.get)
          seeded <- postTournament(ctx, "/api/tournament/seed", seedBody(16))
          finalState <- playAllReadyMatches(ctx)
        } yield assertTrue(
          resolved.contains(tournamentDir),
          seeded.bracket.exists(_.size == 16),
          seeded.bracket.exists(_.matches.size == 30),
          allMatchesCompleted(finalState)
        )
      }
    }
  )
}
