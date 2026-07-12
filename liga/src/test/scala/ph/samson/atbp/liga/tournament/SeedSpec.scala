package ph.samson.atbp.liga.tournament

import ph.samson.atbp.liga.glicko.Tuning
import ph.samson.atbp.liga.model.*
import zio.test.*

import java.time.Instant

object SeedSpec extends ZIOSpecDefault {

  private val at = Instant.parse("2026-03-15T18:00:00Z")

  private def rating(name: String, r: Double): PlayerRating =
    PlayerRating(Player(name), r, rd = 100, wins = 2, losses = 1)

  private def lockedState(players: List[Player]): TournamentState =
    TournamentState(
      name = "Open",
      players = players,
      playersLocked = true,
      roundRaceTo = Map(1 -> 7, 2 -> 7, 3 -> 7, 4 -> 7)
    )

  private val fullRoundRaceTo: Map[Int, Int] =
    Map(1 -> 7, 2 -> 7, 3 -> 7, 4 -> 7)

  private val eightPlayers: List[Player] =
    (1 to 8).map(i => Player(s"P$i")).toList

  private val periodRatings: List[PlayerRating] =
    eightPlayers.map(p => rating(p.name, 1700))

  def spec = suite("Seed")(
    test("seed rejected when roster is not locked") {
      val state = TournamentState(
        name = "Open",
        players = eightPlayers,
        playersLocked = false
      )
      assertTrue(
        Seed
          .buildEvents(state, periodRatings, fullRoundRaceTo, startSeq = 2, at)
          .isLeft
      )
    },
    test("seed rejected before race-to is complete") {
      val state = lockedState(eightPlayers).copy(roundRaceTo = Map.empty)
      assertTrue(
        Seed
          .buildEvents(state, periodRatings, fullRoundRaceTo, startSeq = 2, at)
          .isLeft
      )
    },
    test("guest player receives default rating at seed") {
      val guest = Player("Zara")
      val players = guest :: eightPlayers.take(7)
      val state = lockedState(players)
      val result =
        Seed.buildEvents(
          state,
          periodRatings,
          fullRoundRaceTo,
          startSeq = 2,
          at
        )
      val tuning = Tuning.Default
      val guestRating = result.toOption.get
        .collect { case event: events.TournamentEvent.BracketSeeded =>
          event.payload.frozenRatings
        }
        .flatten
        .find(_.player == guest)
        .get
      assertTrue(
        result.isRight,
        guestRating.rating == tuning.initRating,
        guestRating.rd == tuning.maxDeviation,
        guestRating.wins == 0,
        guestRating.losses == 0
      )
    },
    test("period players keep computed ratings at seed") {
      val state = lockedState(eightPlayers)
      val result =
        Seed.buildEvents(
          state,
          periodRatings,
          fullRoundRaceTo,
          startSeq = 2,
          at
        )
      val seeded = result.toOption.get.collect {
        case event: events.TournamentEvent.BracketSeeded => event
      }.last
      assertTrue(
        seeded.payload.frozenRatings
          .find(_.player == Player("P1"))
          .exists(
            _.rating == 1700
          )
      )
    }
  )
}
