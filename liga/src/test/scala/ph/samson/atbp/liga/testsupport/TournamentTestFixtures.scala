package ph.samson.atbp.liga.testsupport

import ph.samson.atbp.liga.bracket.BracketGen
import ph.samson.atbp.liga.glicko.Tuning
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.tournament.events.TournamentEvent

import java.time.Instant

object TournamentTestFixtures {

  val at: Instant = Instant.parse("2026-03-15T18:00:00Z")

  def rating(name: String, r: Double): PlayerRating =
    PlayerRating(Player(name), r, rd = 100, wins = 0, losses = 0)

  def unratedRating(name: String): PlayerRating = {
    val tuning = Tuning.Default
    PlayerRating(
      Player(name),
      tuning.initRating,
      tuning.maxDeviation,
      wins = 0,
      losses = 0
    )
  }

  def withUnratedFrozenRating(
      state: TournamentState,
      player: Player
  ): TournamentState =
    state.copy(
      frozenRatings =
        state.frozenRatings.updated(player, unratedRating(player.name))
    )

  val eightPlayerRatings: List[PlayerRating] =
    (1 to 8).map(i => rating(s"P$i", 1700 - i * 10)).toList

  def seededState(raceToByScope: Map[String, Int]): TournamentState = {
    val bracket = BracketGen.generate(eightPlayerRatings)
    TournamentState(
      name = "Spring Open",
      players = eightPlayerRatings.map(_.player),
      bracket = Some(bracket),
      frozenRatings = eightPlayerRatings.map(r => r.player -> r).toMap,
      raceToByScope = raceToByScope
    )
  }

  def seededState(): TournamentState =
    seededState(RaceToTestSupport.uniformRaceTo(8))

  def matchOf(state: TournamentState, id: String): BracketMatch =
    state.bracket.flatMap(_.matches.find(_.id == id)).get

  def withMatch(
      state: TournamentState,
      id: String
  )(
      update: BracketMatch => BracketMatch
  ): TournamentState =
    state.copy(
      bracket = state.bracket.map { bracket =>
        bracket.copy(
          matches = bracket.matches.map { matchDef =>
            if (matchDef.id == id) {
              update(matchDef)
            } else {
              matchDef
            }
          }
        )
      }
    )

  def seededEvents(state: TournamentState): List[TournamentEvent] = {
    val players = eightPlayerRatings.map(_.player)
    val raceToEvents =
      RaceToTestSupport.raceToSetEvents(playerCount = 8, startSeq = 4, at = at)
    List(
      TournamentEvent.Created(
        seq = 1,
        at = at,
        payload = TournamentCreatedPayload(
          name = state.name,
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
    ) ++ raceToEvents :+ TournamentEvent.BracketSeeded(
      seq = 12,
      at = at,
      payload = BracketSeededPayload(
        frozenRatings = eightPlayerRatings,
        bracket = state.bracket.get
      )
    )
  }
}
