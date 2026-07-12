package ph.samson.atbp.liga.tournament

import ph.samson.atbp.liga.bracket.BracketGen
import ph.samson.atbp.liga.glicko.Tuning
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.tournament.events.TournamentEvent

import java.time.Instant

/** Builds bracket seeding events from period-end ratings. */
object Seed {

  sealed trait Error {
    def message: String
  }

  final case class TournamentCompletedError() extends Error {
    val message: String = "tournament is already completed"
  }

  final case class AlreadySeededError() extends Error {
    val message: String = "bracket is already seeded"
  }

  final case class NoPlayersError() extends Error {
    val message: String = "tournament has no players"
  }

  final case class InvalidPlayerCountError(count: Int) extends Error {
    val message: String = s"player count $count must be between 8 and 64"
  }

  final case class MissingPlayerError(name: String) extends Error {
    val message: String = s"unknown player: $name"
  }

  final case class PlayersNotLockedError() extends Error {
    val message: String = "cannot seed bracket before roster is locked"
  }

  def buildEvents(
      state: TournamentState,
      periodRatings: List[PlayerRating],
      roundRaceTo: Map[Int, Int],
      startSeq: Int,
      at: Instant
  ): Either[Error, List[TournamentEvent]] =
    for {
      _ <- validateState(state)
      ratings <- resolveRatings(state.players, periodRatings)
      _ <- validatePlayerCount(ratings.size)
      bracket = BracketGen.generate(ratings)
      raceToEvents = roundRaceTo.toList.sortBy(_._1).zipWithIndex.map {
        case ((round, raceTo), index) =>
          TournamentEvent.RoundRaceToSet(
            seq = startSeq + index,
            at = at,
            payload = RoundRaceToSetPayload(round = round, raceTo = raceTo)
          )
      }
      seededSeq = startSeq + raceToEvents.size
      seeded = TournamentEvent.BracketSeeded(
        seq = seededSeq,
        at = at,
        payload = BracketSeededPayload(
          frozenRatings = ratings,
          bracket = bracket
        )
      )
    } yield raceToEvents :+ seeded

  private def validateState(
      state: TournamentState
  ): Either[Error, Unit] =
    TournamentValidation.validateSeedState(state).left.map {
      case "tournament is already completed" => TournamentCompletedError()
      case "bracket is already seeded"       => AlreadySeededError()
      case "cannot seed bracket before roster is locked" =>
        PlayersNotLockedError()
      case "tournament has no players" => NoPlayersError()
      case _                           => NoPlayersError()
    }

  private def validatePlayerCount(count: Int): Either[Error, Unit] =
    if (count >= 8 && count <= 64) {
      Right(())
    } else {
      Left(InvalidPlayerCountError(count))
    }

  private def resolveRatings(
      players: List[Player],
      periodRatings: List[PlayerRating]
  ): Either[Error, List[PlayerRating]] = {
    val byName = periodRatings.map(r => r.player.name -> r).toMap
    val tuning = Tuning.Default
    players.foldLeft(Right(Nil): Either[Error, List[PlayerRating]]) {
      case (Right(acc), player) =>
        val rating = byName.getOrElse(
          player.name,
          PlayerRating(
            player = player,
            rating = tuning.initRating,
            rd = tuning.maxDeviation,
            wins = 0,
            losses = 0
          )
        )
        Right(acc :+ rating)
      case (left, _) => left
    }
  }
}
