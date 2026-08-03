package ph.samson.atbp.liga.serve

import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.tournament.EventCodec
import ph.samson.atbp.liga.tournament.EventLog
import ph.samson.atbp.liga.tournament.PeriodEmission
import ph.samson.atbp.liga.tournament.Tournament
import zio.*
import zio.http.*
import zio.json.*
import zio.json.EncoderOps

import java.time.Instant
import java.time.LocalDate

/** Director-only POST routes (localhost). */
object DirectorRoutes {

  import EventCodec.given

  private val internalServerErrorBody = "internal server error"

  final case class CreateRequest(name: String)
  final case class PlayersRequest(players: List[Player])
  final case class RaceToRequest(raceToByScope: Map[String, Int])
  final case class SeedRequest(raceToByScope: Map[String, Int] = Map.empty)
  final case class HandicapRequest(handicap: Int)
  final case class CompleteRequest(completed: Option[LocalDate] = None)
  final case class ResultRequest(scoreA: Int, scoreB: Int)
  final case class ForfeitRequest(forfeitingSide: String, reason: String)

  given JsonCodec[CompleteRequest] = DeriveJsonCodec.gen

  given JsonCodec[CreateRequest] = DeriveJsonCodec.gen
  given JsonCodec[PlayersRequest] = DeriveJsonCodec.gen
  given JsonCodec[RaceToRequest] = DeriveJsonCodec.gen
  given JsonCodec[SeedRequest] = DeriveJsonCodec.gen
  given JsonCodec[HandicapRequest] = DeriveJsonCodec.gen
  given JsonCodec[ResultRequest] = DeriveJsonCodec.gen
  given JsonCodec[ForfeitRequest] = DeriveJsonCodec.gen

  def routes(ctx: ServeContext): Routes[Any, Response] =
    zio.http.Routes(
      Method.POST / "api" / "tournament" / "create" -> handler {
        (req: Request) =>
          directorOnly(req)(handleCreate(ctx, req))
      },
      Method.POST / "api" / "tournament" / "players" -> handler {
        (req: Request) =>
          directorOnly(req)(handlePlayers(ctx, req))
      },
      Method.POST / "api" / "tournament" / "lock" -> handler { (req: Request) =>
        directorOnly(req)(handleLock(ctx))
      },
      Method.POST / "api" / "tournament" / "race-to" -> handler {
        (req: Request) =>
          directorOnly(req)(handleRaceTo(ctx, req))
      },
      Method.POST / "api" / "tournament" / "seed" -> handler { (req: Request) =>
        directorOnly(req)(handleSeed(ctx, req))
      },
      Method.POST / "api" / "matches" / string("matchId") / "ready" -> handler {
        (matchId: String, req: Request) =>
          directorOnly(req)(handleReady(ctx, matchId))
      },
      Method.POST / "api" / "matches" / string(
        "matchId"
      ) / "handicap" -> handler { (matchId: String, req: Request) =>
        directorOnly(req)(handleHandicap(ctx, matchId, req))
      },
      Method.POST / "api" / "matches" / string("matchId") / "start" -> handler {
        (matchId: String, req: Request) =>
          directorOnly(req)(handleStart(ctx, matchId))
      },
      Method.POST / "api" / "matches" / string(
        "matchId"
      ) / "result" -> handler { (matchId: String, req: Request) =>
        directorOnly(req)(handleResult(ctx, matchId, req))
      },
      Method.POST / "api" / "matches" / string(
        "matchId"
      ) / "forfeit" -> handler { (matchId: String, req: Request) =>
        directorOnly(req)(handleForfeit(ctx, matchId, req))
      },
      Method.POST / "api" / "tournament" / "complete" -> handler {
        (req: Request) =>
          directorOnly(req)(handleComplete(ctx, req))
      }
    )

  private[serve] def directorOnly(
      req: Request
  )(
      effect: Task[Response]
  ): UIO[Response] =
    if (BindConfig.isLocalDirector(req)) {
      effect
        .catchSome {
          case ServeContext.CommandError(message) =>
            ZIO.succeed(badRequest(message))
          case ServeContext.DirCollisionError(message) =>
            ZIO.succeed(conflict(message))
          case EventLog.InvalidSeq(expected, actual) =>
            ZIO.succeed(
              conflict(
                s"event seq must be $expected, got $actual; retry the request"
              )
            )
          case err: PeriodEmission.EmissionError
              if err.message.contains("already exists") ||
                err.message.contains("mismatch") =>
            ZIO.succeed(conflict(err.message))
        }
        .catchAll { _ =>
          ZIO.succeed(internalServerError)
        }
    } else {
      ZIO.succeed(Response.text("forbidden").status(Status.Forbidden))
    }

  private def parseJson[A: JsonDecoder](body: String): Task[A] =
    ZIO.fromEither(
      body.fromJson[A].left.map(msg => ServeContext.CommandError(msg))
    )

  private def handleCreate(ctx: ServeContext, req: Request): Task[Response] =
    for {
      body <- req.body.asString
      parsed <- parseJson[CreateRequest](body)
      _ <- ctx.createTournament(parsed.name)
      response <- jsonState(ctx)
    } yield response

  private def handlePlayers(ctx: ServeContext, req: Request): Task[Response] =
    for {
      body <- req.body.asString
      parsed <- parseJson[PlayersRequest](body)
      state <- ctx.loadTournament
      seq <- ctx.nextSeq
      at = Instant.now()
      event <- ZIO.fromEither(
        Tournament
          .setPlayers(state, parsed.players, seq, at)
          .left
          .map(err => ServeContext.CommandError(err.message))
      )
      _ <- ctx.appendWizardEvent(event)
      response <- jsonState(ctx)
    } yield response

  private def handleLock(ctx: ServeContext): Task[Response] =
    for {
      state <- ctx.loadTournament
      seq <- ctx.nextSeq
      at = Instant.now()
      event <- ZIO.fromEither(
        Tournament
          .lockPlayers(state, seq, at)
          .left
          .map(err => ServeContext.CommandError(err.message))
      )
      _ <- ctx.appendWizardEvent(event)
      response <- jsonState(ctx)
    } yield response

  private def handleRaceTo(ctx: ServeContext, req: Request): Task[Response] =
    for {
      body <- req.body.asString
      parsed <- parseJson[RaceToRequest](body)
      state <- ctx.loadTournament
      seq <- ctx.nextSeq
      at = Instant.now()
      events <- ZIO.fromEither(
        Tournament
          .setRaceToByScope(state, parsed.raceToByScope, seq, at)
          .left
          .map(err => ServeContext.CommandError(err.message))
      )
      _ <- ctx.appendWizardEvents(events)
      response <- jsonState(ctx)
    } yield response

  private def handleSeed(ctx: ServeContext, req: Request): Task[Response] =
    for {
      body <- req.body.asString
      parsed <-
        if (body.isBlank) {
          ZIO.succeed(SeedRequest())
        } else {
          parseJson[SeedRequest](body)
        }
      _ <- ctx.seedBracket(parsed.raceToByScope)
      response <- jsonState(ctx)
    } yield response

  private def handleReady(ctx: ServeContext, matchId: String): Task[Response] =
    for {
      _ <- ctx.applyMatchCommands { (current, seq, at) =>
        Tournament.ready(current, matchId, seq, at)
      }
      response <- jsonState(ctx)
    } yield response

  private def handleHandicap(
      ctx: ServeContext,
      matchId: String,
      req: Request
  ): Task[Response] =
    for {
      body <- req.body.asString
      parsed <- parseJson[HandicapRequest](body)
      _ <- ctx.applyMatchCommand { (current, seq, at) =>
        Tournament.applyHandicap(current, matchId, parsed.handicap, seq, at)
      }
      response <- jsonState(ctx)
    } yield response

  private def handleStart(ctx: ServeContext, matchId: String): Task[Response] =
    for {
      _ <- ctx.applyMatchCommand { (current, seq, at) =>
        Tournament.start(current, matchId, seq, at)
      }
      response <- jsonState(ctx)
    } yield response

  private def handleResult(
      ctx: ServeContext,
      matchId: String,
      req: Request
  ): Task[Response] =
    for {
      body <- req.body.asString
      parsed <- parseJson[ResultRequest](body)
      _ <- ctx.applyMatchCommand { (current, seq, at) =>
        Tournament.recordResult(
          current,
          matchId,
          parsed.scoreA,
          parsed.scoreB,
          seq,
          at
        )
      }
      response <- jsonState(ctx)
    } yield response

  private def handleForfeit(
      ctx: ServeContext,
      matchId: String,
      req: Request
  ): Task[Response] =
    for {
      body <- req.body.asString
      parsed <- parseJson[ForfeitRequest](body)
      _ <- ctx.applyMatchCommand { (current, seq, at) =>
        Tournament.recordForfeit(
          current,
          matchId,
          parsed.forfeitingSide,
          parsed.reason,
          seq,
          at
        )
      }
      response <- jsonState(ctx)
    } yield response

  private def handleComplete(ctx: ServeContext, req: Request): Task[Response] =
    for {
      body <- req.body.asString
      parsed <-
        if (body.isBlank) {
          ZIO.succeed(CompleteRequest())
        } else {
          parseJson[CompleteRequest](body)
        }
      completed = parsed.completed.getOrElse(LocalDate.now())
      _ <- ctx.completeTournament(completed)
      response <- jsonState(ctx)
    } yield response

  private def jsonState(ctx: ServeContext): Task[Response] =
    for {
      hasDir <- ctx.hasActiveDir
      (reloaded, timing) <- ctx.loadTournamentWithTiming
    } yield Response.json(
      ApiJson.tournamentFrom(reloaded, hasDir, timing).toJson
    )

  private def badRequest(message: String): Response =
    Response.text(message).status(Status.BadRequest)

  private def conflict(message: String): Response =
    Response.text(message).status(Status.Conflict)

  private def internalServerError: Response =
    Response.text(internalServerErrorBody).status(Status.InternalServerError)
}
