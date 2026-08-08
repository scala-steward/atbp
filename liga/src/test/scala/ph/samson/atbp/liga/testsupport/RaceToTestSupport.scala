package ph.samson.atbp.liga.testsupport

import ph.samson.atbp.liga.bracket.RaceToScopes
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.tournament.events.TournamentEvent

import java.time.Instant

object RaceToTestSupport {

  def uniformRaceTo(playerCount: Int): Map[String, Int] =
    uniformRaceTo(playerCount, topN = 2, raceTo = 7)

  def uniformRaceTo(
      playerCount: Int,
      topN: Int,
      raceTo: Int
  ): Map[String, Int] =
    RaceToScopes.requiredKeys(playerCount, topN).map(_ -> raceTo).toMap

  /** Winners 7, losers 5, grand final 9 — exercises per-section resolution. */
  def differentiatedRaceTo(playerCount: Int): Map[String, Int] =
    differentiatedRaceTo(playerCount, topN = 2)

  def differentiatedRaceTo(
      playerCount: Int,
      topN: Int
  ): Map[String, Int] =
    RaceToScopes
      .requiredKeys(playerCount, topN)
      .map {
        case scope if scope.startsWith("lb-") => scope -> 5
        case scope if scope == "gf"           => scope -> 9
        case scope                            => scope -> 7
      }
      .toMap

  def formatSetEvent(
      playerCount: Int,
      startSeq: Int,
      at: Instant
  ): TournamentEvent.FormatSet =
    formatSetEvent(playerCount, startSeq, at, topN = 2, raceTo = 7)

  def formatSetEvent(
      playerCount: Int,
      startSeq: Int,
      at: Instant,
      topN: Int
  ): TournamentEvent.FormatSet =
    formatSetEvent(playerCount, startSeq, at, topN, raceTo = 7)

  def formatSetEvent(
      playerCount: Int,
      startSeq: Int,
      at: Instant,
      topN: Int,
      raceTo: Int
  ): TournamentEvent.FormatSet =
    TournamentEvent.FormatSet(
      seq = startSeq,
      at = at,
      payload = FormatSetPayload(
        topN = topN,
        raceToByScope = uniformRaceTo(playerCount, topN, raceTo)
      )
    )
}
