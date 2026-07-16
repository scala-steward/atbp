package ph.samson.atbp.liga.tournament

import better.files.File
import ph.samson.atbp.liga.bracket.BracketGen
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.testsupport.RaceToTestSupport
import ph.samson.atbp.liga.tournament.EventCodec
import ph.samson.atbp.liga.tournament.events.TournamentEvent

import java.time.Instant
import java.time.LocalDate

/** One-off helper to regenerate replay fixture directories. */
object ReplayFixtureWriter {

  private val at = Instant.parse("2026-03-15T18:00:00Z")

  private def rating(name: String, r: Double): PlayerRating =
    PlayerRating(Player(name), r, rd = 100, wins = 0, losses = 0)

  private def eightPlayerRatings: List[PlayerRating] =
    (1 to 8).map(i => rating(s"P$i", 1700 - i * 10)).toList

  private def writeFixtures(root: File): Unit = {
    val players = eightPlayerRatings
    val playerList = players.map(_.player)
    val bracket = BracketGen.generate(players)
    val created = TournamentEvent.Created(
      seq = 1,
      at = at,
      payload = TournamentCreatedPayload(
        name = "Spring Open",
        players = Nil
      )
    )
    val playersSet = TournamentEvent.PlayersSet(
      seq = 2,
      at = at,
      payload = PlayersSetPayload(players = playerList)
    )
    val locked = TournamentEvent.PlayersLocked(
      seq = 3,
      at = at,
      payload = PlayersLockedPayload()
    )
    val raceToEvents =
      RaceToTestSupport.raceToSetEvents(playerCount = 8, startSeq = 4, at = at)
    val seeded = TournamentEvent.BracketSeeded(
      seq = 12,
      at = at,
      payload = BracketSeededPayload(
        frozenRatings = players,
        bracket = bracket
      )
    )
    val lifecycle = List(
      TournamentEvent.MatchReady(
        seq = 13,
        at = at,
        payload = MatchReadyPayload(matchId = "wb-1-1", handicapSuggested = 2)
      ),
      TournamentEvent.HandicapApplied(
        seq = 14,
        at = at,
        payload =
          HandicapAppliedPayload(matchId = "wb-1-1", handicapApplied = 3)
      ),
      TournamentEvent.MatchStarted(
        seq = 15,
        at = at,
        payload = MatchStartedPayload(matchId = "wb-1-1")
      )
    )
    val completed = TournamentEvent.TournamentCompleted(
      seq = 16,
      at = at,
      payload =
        TournamentCompletedPayload(completed = LocalDate.parse("2026-03-15"))
    )

    val wizard = List(created, playersSet, locked) ++ raceToEvents :+ seeded
    writeDir(root / "eight-player-seeded", wizard)
    writeDir(root / "eight-player-partial", wizard ++ lifecycle)
    writeDir(
      root / "eight-player-complete",
      wizard ++ lifecycle :+ completed
    )
  }

  private def writeDir(dir: File, events: List[TournamentEvent]): Unit = {
    dir.delete(swallowIOExceptions = true)
    dir.createDirectoryIfNotExists(createParents = true)
    events.foreach { event =>
      val file = dir / EventLog.filenameFor(event)
      file.write(EventCodec.encode(event))
    }
  }

  def main(args: Array[String]): Unit = {
    val root = File("src/test/resources/tournaments")
    writeFixtures(root)
  }
}
