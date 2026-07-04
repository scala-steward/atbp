package ph.samson.atbp.liga.tournament

import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.tournament.events.TournamentEvent
import zio.json.*

object EventCodec {

  given JsonCodec[Player] = DeriveJsonCodec.gen
  given JsonCodec[PlayerRating] = DeriveJsonCodec.gen
  given JsonCodec[MatchResult] = DeriveJsonCodec.gen
  given JsonCodec[BracketMatchState] = DeriveJsonCodec.gen
  given JsonCodec[BracketMatch] = DeriveJsonCodec.gen
  given JsonCodec[Bracket] = DeriveJsonCodec.gen
  given JsonCodec[TournamentCreatedPayload] = DeriveJsonCodec.gen
  given JsonCodec[RoundRaceToSetPayload] = DeriveJsonCodec.gen
  given JsonCodec[BracketSeededPayload] = DeriveJsonCodec.gen
  given JsonCodec[MatchReadyPayload] = DeriveJsonCodec.gen
  given JsonCodec[HandicapAppliedPayload] = DeriveJsonCodec.gen
  given JsonCodec[MatchStartedPayload] = DeriveJsonCodec.gen
  given JsonCodec[MatchResultPayload] = DeriveJsonCodec.gen
  given JsonCodec[TournamentCompletedPayload] = DeriveJsonCodec.gen
  given JsonCodec[TournamentEvent] = DeriveJsonCodec.gen

  def encode(event: TournamentEvent): String =
    event.toJson

  def decode(json: String): Either[String, TournamentEvent] =
    json.fromJson[TournamentEvent]

  /** Ensures each event `seq` is strictly greater than the previous when sorted. */
  def validateMonotonicSeq(events: List[TournamentEvent]): Either[String, Unit] = {
    val sorted = events.sortBy(_.seq)
    val violation = sorted.sliding(2).collectFirst {
      case List(previous, next) if previous.seq >= next.seq =>
        s"Event seq must be strictly increasing: ${previous.seq} then ${next.seq}"
    }
    violation.toLeft(())
  }
}
