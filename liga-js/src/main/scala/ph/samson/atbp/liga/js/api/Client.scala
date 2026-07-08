package ph.samson.atbp.liga.js.api

import org.scalajs.dom
import ph.samson.atbp.liga.js.api.Models.*
import zio.json.*

import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.scalajs.js

/** Browser fetch client for Liga serve read and director POST routes. */
final class ApiClient private (baseUrl: String)(using ExecutionContext) {

  import ApiClient.*

  def getTournament: Future[TournamentResponse] =
    get("/api/tournament")

  def getLeaderboard: Future[LeaderboardResponse] =
    get("/api/leaderboard")

  def createTournament(name: String): Future[TournamentResponse] =
    post(
      "/api/tournament/create",
      Some(CreateRequest(name).toJson)
    )

  def setPlayers(players: List[Player]): Future[TournamentResponse] =
    post(
      "/api/tournament/players",
      Some(PlayersRequest(players).toJson)
    )

  def lockPlayers(): Future[TournamentResponse] =
    post("/api/tournament/lock", Some("{}"))

  def setRaceTo(roundRaceTo: Map[Int, Int]): Future[TournamentResponse] =
    post(
      "/api/tournament/race-to",
      Some(RaceToRequest(roundRaceTo).toJson)
    )

  def seed(): Future[TournamentResponse] =
    seed(Map.empty)

  def seed(roundRaceTo: Map[Int, Int]): Future[TournamentResponse] =
    post(
      "/api/tournament/seed",
      Some(SeedRequest(roundRaceTo).toJson)
    )

  def ready(matchId: String): Future[TournamentResponse] =
    post(s"/api/matches/$matchId/ready", None)

  def applyHandicap(
      matchId: String,
      handicap: Int
  ): Future[TournamentResponse] =
    post(
      s"/api/matches/$matchId/handicap",
      Some(HandicapRequest(handicap).toJson)
    )

  def start(matchId: String): Future[TournamentResponse] =
    post(s"/api/matches/$matchId/start", None)

  def recordResult(
      matchId: String,
      scoreA: Int,
      scoreB: Int
  ): Future[TournamentResponse] =
    post(
      s"/api/matches/$matchId/result",
      Some(ResultRequest(scoreA, scoreB).toJson)
    )

  private def url(path: String): String = {
    val base = if (baseUrl.endsWith("/")) baseUrl.dropRight(1) else baseUrl
    s"$base$path"
  }

  private def get[T: JsonDecoder](path: String): Future[T] =
    fetch(dom.HttpMethod.GET, path, None)

  private def post[T: JsonDecoder](
      path: String,
      jsonBody: Option[String]
  ): Future[T] =
    fetch(dom.HttpMethod.POST, path, jsonBody)

  private def fetch[T: JsonDecoder](
      httpMethod: dom.HttpMethod,
      path: String,
      jsonBody: Option[String]
  ): Future[T] = {
    val init = new dom.RequestInit {
      this.method = httpMethod
      jsonBody.foreach { json =>
        this.body = json
        this.headers = js.Dictionary("Content-Type" -> "application/json")
      }
    }
    dom
      .fetch(url(path), init)
      .toFuture
      .flatMap { response =>
        response.text().toFuture.flatMap { text =>
          decodeResponse[T](response.status, text) match {
            case Right(value) => Future.successful(value)
            case Left(err)    => Future.failed(err)
          }
        }
      }
      .recoverWith { case err: ApiError =>
        Future.failed(err)
      }
      .recoverWith { case other =>
        Future.failed(NetworkFailure(other))
      }
  }
}

object ApiClient {

  def apply()(using ExecutionContext): ApiClient =
    new ApiClient("")

  def apply(baseUrl: String)(using ExecutionContext): ApiClient =
    new ApiClient(baseUrl)

  sealed abstract class ApiError(message: String) extends Exception(message)

  final case class NetworkFailure(cause: Throwable)
      extends ApiError(Option(cause.getMessage).getOrElse("network error"))

  final case class HttpFailure(status: Int, body: String)
      extends ApiError(s"HTTP $status: $body")

  final case class DecodeFailure(message: String)
      extends ApiError(s"JSON decode error: $message")

  def decodeResponse[T: JsonDecoder](
      status: Int,
      body: String
  ): Either[ApiError, T] =
    if (status >= 200 && status < 300) {
      body.fromJson[T].left.map(DecodeFailure(_))
    } else {
      Left(HttpFailure(status, body))
    }
}
