package ph.samson.atbp.liga.testsupport

import ph.samson.atbp.liga.bracket.RaceToScopes
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.tournament.events.TournamentEvent

import java.time.Instant

object RaceToTestSupport {

  def uniformRaceTo(playerCount: Int, raceTo: Int): Map[String, Int] =
    RaceToScopes.requiredKeys(playerCount).map(_ -> raceTo).toMap

  def uniformRaceTo(playerCount: Int): Map[String, Int] =
    uniformRaceTo(playerCount, raceTo = 7)

  def raceToSetEvents(
      playerCount: Int,
      startSeq: Int,
      at: Instant,
      raceTo: Int
  ): List[TournamentEvent.RaceToSet] =
    RaceToScopes.requiredKeys(playerCount).zipWithIndex.map {
      case (scope, index) =>
        TournamentEvent.RaceToSet(
          seq = startSeq + index,
          at = at,
          payload = RaceToSetPayload(scope = scope, raceTo = raceTo)
        )
    }

  def raceToSetEvents(
      playerCount: Int,
      startSeq: Int,
      at: Instant
  ): List[TournamentEvent.RaceToSet] =
    raceToSetEvents(playerCount, startSeq, at, raceTo = 7)
}
