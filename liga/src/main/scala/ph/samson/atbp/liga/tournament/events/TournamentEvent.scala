package ph.samson.atbp.liga.tournament.events

import ph.samson.atbp.liga.model.*
import zio.json.*

import java.time.Instant

/** Append-only tournament event (v1). */
@jsonDiscriminator("type")
sealed trait TournamentEvent {
  def seq: Int
  def at: Instant
}

object TournamentEvent {

  @jsonHint("TournamentCreated")
  final case class Created(
      seq: Int,
      at: Instant,
      payload: TournamentCreatedPayload
  ) extends TournamentEvent

  @jsonHint("RoundRaceToSet")
  final case class RoundRaceToSet(
      seq: Int,
      at: Instant,
      payload: RoundRaceToSetPayload
  ) extends TournamentEvent

  @jsonHint("BracketSeeded")
  final case class BracketSeeded(
      seq: Int,
      at: Instant,
      payload: BracketSeededPayload
  ) extends TournamentEvent

  @jsonHint("MatchReady")
  final case class MatchReady(
      seq: Int,
      at: Instant,
      payload: MatchReadyPayload
  ) extends TournamentEvent

  @jsonHint("HandicapApplied")
  final case class HandicapApplied(
      seq: Int,
      at: Instant,
      payload: HandicapAppliedPayload
  ) extends TournamentEvent

  @jsonHint("MatchStarted")
  final case class MatchStarted(
      seq: Int,
      at: Instant,
      payload: MatchStartedPayload
  ) extends TournamentEvent

  @jsonHint("MatchResult")
  final case class MatchResult(
      seq: Int,
      at: Instant,
      payload: MatchResultPayload
  ) extends TournamentEvent

  @jsonHint("TournamentCompleted")
  final case class TournamentCompleted(
      seq: Int,
      at: Instant,
      payload: TournamentCompletedPayload
  ) extends TournamentEvent
}
