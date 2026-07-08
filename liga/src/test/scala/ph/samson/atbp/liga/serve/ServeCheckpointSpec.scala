package ph.samson.atbp.liga.serve

import better.files.File
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.serve.ApiJson.*
import ph.samson.atbp.liga.serve.Routes as LigaRoutes
import ph.samson.atbp.liga.tournament.EventCodec
import ph.samson.atbp.liga.tournament.EventLog
import ph.samson.atbp.liga.tournament.Replay
import ph.samson.atbp.liga.tournament.Resume
import ph.samson.atbp.liga.tournament.events.TournamentEvent
import zio.*
import zio.http.*
import zio.json.*
import zio.test.*

import java.net.InetAddress
import java.time.Instant

/** Checkpoint verification before Phase 4 frontend work. */
object ServeCheckpointSpec extends ZIOSpecDefault {

  private val at = Instant.parse("2026-03-15T18:00:00Z")

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
      f: (File, File) => ZIO[R, Throwable, A]
  ): ZIO[R, Throwable, A] =
    ZIO.acquireReleaseWith(
      ZIO.attemptBlocking(File.newTemporaryDirectory("liga-serve-checkpoint"))
    )(root => ZIO.attemptBlocking(root.delete()).ignore) { root =>
      val dataDir = root / "data"
      ZIO.attemptBlocking(dataDir.createDirectoryIfNotExists()).flatMap { _ =>
        f(dataDir, root)
      }
    }

  private def seedPeriodFile(dataDir: File): Unit = {
    File(getClass.getResource("/periods/eight-player-seed.liga"))
      .copyTo(dataDir / "eight-player-seed.liga")
    ()
  }

  private val tournamentDirName = "tournament-20260315-spring-open"

  private def writeCreatedTournament(dataDir: File): File = {
    val tournamentDir = dataDir / tournamentDirName
    tournamentDir.createDirectoryIfNotExists()
    val players = (1 to 8).map(i => Player(s"P$i")).toList
    val events = List(
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
    events.foreach { event =>
      (tournamentDir / EventLog.filenameFor(event))
        .write(EventCodec.encode(event))
    }
    tournamentDir
  }

  private def copyPartialFixture(dataDir: File): File = {
    val tournamentDir = dataDir / tournamentDirName
    File(getClass.getResource("/tournaments/eight-player-partial"))
      .copyTo(tournamentDir, overwrite = true)
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
          new RuntimeException(
            s"$path returned ${response.status}: $text"
          )
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
      response <- routes(ctx)
        .runZIO(localhostGet("/api/tournament"))
      body <- response.body.asString
      initial <- parseTournament(body)
      finalState <- loop(initial)
    } yield finalState
  }

  private val seedBody =
    """{"roundRaceTo":{"1":7,"2":7,"3":7,"4":7,"5":7}}"""

  private def allMatchesCompleted(state: TournamentResponse): Boolean =
    state.bracket.exists(
      _.matches.forall(_.state == BracketMatchState.Completed)
    )

  private def recordResult(
      ctx: ServeContext,
      matchId: String
  ): ZIO[Scope, Throwable, TournamentResponse] =
    recordResult(ctx, matchId, winnerIsA = true)

  private def recordResult(
      ctx: ServeContext,
      matchId: String,
      winnerIsA: Boolean
  ): ZIO[Scope, Throwable, TournamentResponse] = {
    val (scoreA, scoreB) = if (winnerIsA) (7, 4) else (4, 7)
    postTournament(
      ctx,
      s"/api/matches/$matchId/result",
      s"""{"scoreA":$scoreA,"scoreB":$scoreB}"""
    )
  }

  def spec = suite("ServeCheckpoint")(
    test("full eight-player tournament runnable via HTTP API alone") {
      withTempDataDir { (dataDir, _) =>
        for {
          _ <- ZIO.attemptBlocking(seedPeriodFile(dataDir))
          tournamentDir <- ZIO.attemptBlocking(writeCreatedTournament(dataDir))
          resolved <- Resume.resolve(dataDir)
          ctx = freshContext(dataDir, resolved.get)
          _ <- postTournament(ctx, "/api/tournament/seed", seedBody)
          finalState <- playAllReadyMatches(ctx)
        } yield assertTrue(
          resolved == Some(tournamentDir),
          finalState.bracket.exists(_.matches.size == 14),
          allMatchesCompleted(finalState)
        )
      }
    },
    test("crash recovery resumes partial tournament and continues via API") {
      withTempDataDir { (dataDir, _) =>
        for {
          _ <- ZIO.attemptBlocking(seedPeriodFile(dataDir))
          tournamentDir <- ZIO.attemptBlocking(copyPartialFixture(dataDir))
          resumedDir <- Resume.resolve(dataDir)
          resumedCtx = freshContext(dataDir, resumedDir.get)
          before <- Replay.replayDir(resumedDir.get)
          started = before.bracket
            .flatMap(_.matches.find(_.id == "wb-1-1"))
            .get
          afterCrash <- recordResult(resumedCtx, "wb-1-1")
          restartedCtx = freshContext(dataDir, resumedDir.get)
          afterRestart <- routes(restartedCtx)
            .runZIO(localhostGet("/api/tournament"))
            .flatMap(r => r.body.asString.flatMap(parseTournament))
        } yield assertTrue(
          resumedDir.contains(tournamentDir),
          started.state == BracketMatchState.Started,
          afterCrash.bracket
            .flatMap(_.matches.find(_.id == "wb-1-1"))
            .exists(_.state == BracketMatchState.Completed),
          afterRestart.bracket
            .flatMap(_.matches.find(_.id == "wb-1-1"))
            .exists(_.state == BracketMatchState.Completed)
        )
      }
    },
    test("simulated restart after seed preserves event log state") {
      withTempDataDir { (dataDir, _) =>
        for {
          _ <- ZIO.attemptBlocking(seedPeriodFile(dataDir))
          tournamentDir <- ZIO.attemptBlocking(writeCreatedTournament(dataDir))
          firstCtx = freshContext(dataDir, tournamentDir)
          seeded <- postTournament(firstCtx, "/api/tournament/seed", seedBody)
          _ <- Resume.resolve(dataDir)
          restartedCtx = freshContext(dataDir, tournamentDir)
          afterRestart <- routes(restartedCtx)
            .runZIO(localhostGet("/api/tournament"))
            .flatMap(r => r.body.asString.flatMap(parseTournament))
          eventCount <- EventLog.read(tournamentDir).map(_.size)
        } yield assertTrue(
          seeded.bracket.exists(_.size == 8),
          afterRestart.bracket.exists(_.size == 8),
          afterRestart.frozenRatings.size == 8,
          eventCount >= 5
        )
      }
    }
  )
}
