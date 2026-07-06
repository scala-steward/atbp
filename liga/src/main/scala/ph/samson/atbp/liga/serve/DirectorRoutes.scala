package ph.samson.atbp.liga.serve

import ph.samson.atbp.liga.model.TournamentState
import ph.samson.atbp.liga.tournament.Tournament
import zio.*
import zio.http.*
import zio.json.*
import zio.json.EncoderOps

/** Director-only POST routes (localhost). */
object DirectorRoutes {

  final case class SeedRequest(roundRaceTo: Map[Int, Int] = Map.empty)
  final case class HandicapRequest(handicap: Int)
  final case class ResultRequest(scoreA: Int, scoreB: Int)

  given JsonDecoder[SeedRequest] = DeriveJsonDecoder.gen
  given JsonDecoder[HandicapRequest] = DeriveJsonDecoder.gen
  given JsonDecoder[ResultRequest] = DeriveJsonDecoder.gen

  def routes(ctx: ServeContext): Routes[Any, Response] =
    zio.http.Routes(
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
    } yield jsonState(state)

  private def handleReady(ctx: ServeContext, matchId: String): Task[Response] =
    for {
      state <- ctx.applyMatchCommand { (current, seq, at) =>
        Tournament.ready(current, matchId, seq, at)
      }
    } yield jsonState(state)

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
    } yield jsonState(state)

  private def handleStart(ctx: ServeContext, matchId: String): Task[Response] =
    for {
      state <- ctx.applyMatchCommand { (current, seq, at) =>
        Tournament.start(current, matchId, seq, at)
      }
    } yield jsonState(state)

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
    } yield jsonState(state)

  private def jsonState(state: TournamentState): Response =
    Response.json(ApiJson.tournamentFrom(state).toJson)

  private def badRequest(message: String): Response =
    Response.text(message).status(Status.BadRequest)
}
