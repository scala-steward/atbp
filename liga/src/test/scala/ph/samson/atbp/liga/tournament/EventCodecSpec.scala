package ph.samson.atbp.liga.tournament

import ph.samson.atbp.liga.bracket.BracketGen
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.tournament.events.TournamentEvent
import zio.test.*

import java.time.Instant
import java.time.LocalDate

object EventCodecSpec extends ZIOSpecDefault {

  private val at = Instant.parse("2026-03-15T18:00:00Z")

  private def rating(name: String, r: Double): PlayerRating =
    PlayerRating(Player(name), r, rd = 100, wins = 0, losses = 0)

  private def roundTrip(event: TournamentEvent): Boolean =
    EventCodec.decode(EventCodec.encode(event)) == Right(event)

  def spec = suite("EventCodec")(
    test("TournamentCreated round-trips through JSON") {
      val event = TournamentEvent.Created(
        seq = 1,
        at = at,
        payload = TournamentCreatedPayload(
          name = "Spring Open",
          players = List(Player("Alice"), Player("Bob"))
        )
      )
      assertTrue(
        roundTrip(event),
        EventCodec.encode(event).contains("\"type\":\"TournamentCreated\"")
      )
    },
    test("RoundRaceToSet round-trips through JSON") {
      val event = TournamentEvent.RoundRaceToSet(
        seq = 2,
        at = at,
        payload = RoundRaceToSetPayload(round = 1, raceTo = 7)
      )
      assertTrue(roundTrip(event))
    },
    test("BracketSeeded round-trips through JSON") {
      val players = (1 to 8).map(i => rating(s"P$i", 1700 - i * 10)).toList
      val event = TournamentEvent.BracketSeeded(
        seq = 3,
        at = at,
        payload = BracketSeededPayload(
          frozenRatings = players,
          bracket = BracketGen.generate(players)
        )
      )
      assertTrue(roundTrip(event))
    },
    test("MatchReady round-trips through JSON") {
      val event = TournamentEvent.MatchReady(
        seq = 4,
        at = at,
        payload = MatchReadyPayload(matchId = "wb-1-1", handicapSuggested = 2)
      )
      assertTrue(roundTrip(event))
    },
    test("HandicapApplied round-trips through JSON") {
      val event = TournamentEvent.HandicapApplied(
        seq = 5,
        at = at,
        payload =
          HandicapAppliedPayload(matchId = "wb-1-1", handicapApplied = 3)
      )
      assertTrue(roundTrip(event))
    },
    test("MatchStarted round-trips through JSON") {
      val event = TournamentEvent.MatchStarted(
        seq = 6,
        at = at,
        payload = MatchStartedPayload(matchId = "wb-1-1")
      )
      assertTrue(roundTrip(event))
    },
    test("MatchResult round-trips through JSON") {
      val event = TournamentEvent.MatchResult(
        seq = 7,
        at = at,
        payload = MatchResultPayload(matchId = "wb-1-1", scoreA = 7, scoreB = 4)
      )
      assertTrue(roundTrip(event))
    },
    test("TournamentCompleted round-trips through JSON") {
      val event = TournamentEvent.TournamentCompleted(
        seq = 8,
        at = at,
        payload =
          TournamentCompletedPayload(completed = LocalDate.parse("2026-03-15"))
      )
      assertTrue(roundTrip(event))
    },
    test("PlayersSet round-trips through JSON") {
      val event = TournamentEvent.PlayersSet(
        seq = 9,
        at = at,
        payload = PlayersSetPayload(
          players = List(Player("Alice"), Player("Bob"))
        )
      )
      assertTrue(
        roundTrip(event),
        EventCodec.encode(event).contains("\"type\":\"PlayersSet\"")
      )
    },
    test("PlayersLocked round-trips through JSON") {
      val event = TournamentEvent.PlayersLocked(
        seq = 10,
        at = at,
        payload = PlayersLockedPayload()
      )
      assertTrue(
        roundTrip(event),
        EventCodec.encode(event).contains("\"type\":\"PlayersLocked\"")
      )
    },
    test("filenameFor PlayersSet and PlayersLocked") {
      val playersSet = TournamentEvent.PlayersSet(
        seq = 2,
        at = at,
        payload = PlayersSetPayload(players = List(Player("Alice")))
      )
      val playersLocked = TournamentEvent.PlayersLocked(
        seq = 3,
        at = at,
        payload = PlayersLockedPayload()
      )
      assertTrue(
        EventLog.filenameFor(playersSet) == "000002-players-set.json",
        EventLog.filenameFor(playersLocked) == "000003-players-locked.json"
      )
    },
    test("validateMonotonicSeq accepts strictly increasing seq values") {
      val events = List(
        TournamentEvent.Created(
          seq = 1,
          at = at,
          payload = TournamentCreatedPayload("Open", Nil)
        ),
        TournamentEvent.BracketSeeded(
          seq = 2,
          at = at,
          payload = BracketSeededPayload(Nil, Bracket(8, Nil))
        )
      )
      assertTrue(EventCodec.validateMonotonicSeq(events).isRight)
    },
    test("validateMonotonicSeq rejects duplicate seq") {
      val first = TournamentEvent.Created(
        seq = 1,
        at = at,
        payload = TournamentCreatedPayload("Open", Nil)
      )
      val duplicate = TournamentEvent.MatchStarted(
        seq = 1,
        at = at,
        payload = MatchStartedPayload("wb-1-1")
      )
      assertTrue(EventCodec.validateMonotonicSeq(List(first, duplicate)).isLeft)
    }
  )
}
