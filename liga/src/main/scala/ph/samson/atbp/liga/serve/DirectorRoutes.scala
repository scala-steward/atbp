package ph.samson.atbp.liga.serve

import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.tournament.EventCodec
import ph.samson.atbp.liga.tournament.Tournament
import zio.*
import zio.http.*
import zio.json.*
import zio.json.EncoderOps

import java.time.Instant

/** Director-only POST routes (localhost). */
object DirectorRoutes {

  import EventCodec.given

  final case class CreateRequest(name: String)
  final case class PlayersRequest(players: List[Player])
  final case class RaceToRequest(roundRaceTo: Map[Int, Int])
  final case class SeedRequest(roundRaceTo: Map[Int, Int] = Map.empty)
  final case class HandicapRequest(handicap: Int)
  final case class ResultRequest(scoreA: Int, scoreB: Int)

  given JsonCodec[CreateRequest] = DeriveJsonCodec.gen
  given JsonCodec[PlayersRequest] = DeriveJsonCodec.gen
  given JsonCodec[RaceToRequest] = DeriveJsonCodec.gen
  given JsonCodec[SeedRequest] = DeriveJsonCodec.gen
  given JsonCodec[HandicapRequest] = DeriveJsonCodec.gen
  given JsonCodec[ResultRequest] = DeriveJsonCodec.gen

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
      }
    )

  private def directorOnly(
      req: Request
  )(
      effect: Task[Response]
  ): UIO[Response] =
    if (BindConfig.isLocalDirector(req)) {
      effect
        .catchSome { case ServeContext.CommandError(message) =>
          ZIO.succeed(badRequest(message))
        }
        .catchAll { err =>
          ZIO.succeed(
            Response
              .text(err.getMessage)
              .status(Status.InternalServerError)
          )
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
      state <- ctx.createTournament(parsed.name)
      response <- jsonState(ctx, state)
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
      updated <- ctx.appendWizardEvent(event)
      response <- jsonState(ctx, updated)
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
      updated <- ctx.appendWizardEvent(event)
      response <- jsonState(ctx, updated)
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
          .setRoundRaceTo(state, parsed.roundRaceTo, seq, at)
          .left
          .map(err => ServeContext.CommandError(err.message))
      )
      updated <- ctx.appendWizardEvents(events)
      response <- jsonState(ctx, updated)
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
      state <- ctx.seedBracket(parsed.roundRaceTo)
      response <- jsonState(ctx, state)
    } yield response

  private def handleReady(ctx: ServeContext, matchId: String): Task[Response] =
    for {
      state <- ctx.applyMatchCommand { (current, seq, at) =>
        Tournament.ready(current, matchId, seq, at)
      }
      response <- jsonState(ctx, state)
    } yield response

  private def handleHandicap(
      ctx: ServeContext,
      matchId: String,
      req: Request
  ): Task[Response] =
    for {
      body <- req.body.asString
      parsed <- parseJson[HandicapRequest](body)
      state <- ctx.applyMatchCommand { (current, seq, at) =>
        Tournament.applyHandicap(current, matchId, parsed.handicap, seq, at)
      }
      response <- jsonState(ctx, state)
    } yield response

  private def handleStart(ctx: ServeContext, matchId: String): Task[Response] =
    for {
      state <- ctx.applyMatchCommand { (current, seq, at) =>
        Tournament.start(current, matchId, seq, at)
      }
      response <- jsonState(ctx, state)
    } yield response

  private def handleResult(
      ctx: ServeContext,
      matchId: String,
      req: Request
  ): Task[Response] =
    for {
      body <- req.body.asString
      parsed <- parseJson[ResultRequest](body)
      state <- ctx.applyMatchCommand { (current, seq, at) =>
        Tournament.recordResult(
          current,
          matchId,
          parsed.scoreA,
          parsed.scoreB,
          seq,
          at
        )
      }
      response <- jsonState(ctx, state)
    } yield response

  private def jsonState(ctx: ServeContext, state: TournamentState): Task[Response] =
    ctx.hasActiveDir.map { hasDir =>
      Response.json(ApiJson.tournamentFrom(state, hasDir).toJson)
    }

  private def badRequest(message: String): Response =
    Response.text(message).status(Status.BadRequest)
}
