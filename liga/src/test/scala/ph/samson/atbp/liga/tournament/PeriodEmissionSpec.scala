package ph.samson.atbp.liga.tournament

import better.files.File
import ph.samson.atbp.liga.io.PeriodCodec
import ph.samson.atbp.liga.io.PeriodLoader
import ph.samson.atbp.liga.model.*
import zio.ZIO
import zio.test.*

import java.time.LocalDate

object PeriodEmissionSpec extends ZIOSpecDefault {

  private val completed = LocalDate.parse("2026-03-15")

  private def rating(name: String, r: Double): PlayerRating =
    PlayerRating(Player(name), r, rd = 100, wins = 0, losses = 0)

  private val alice = rating("Alice", 1700)
  private val bob = rating("Bob", 1400)

  private def handicappedMatchState(
      boardA: Int,
      boardB: Int,
      handicapApplied: Int
  ): TournamentState = {
    val completedMatch = BracketMatch(
      id = "wb-1-1",
      playerA = Some(alice.player),
      playerB = Some(bob.player),
      state = BracketMatchState.Completed,
      handicapSuggested = Some(2),
      handicapApplied = Some(handicapApplied),
      result = Some(MatchResult(boardA, boardB))
    )
    TournamentState(
      name = "Spring Open",
      players = List(alice.player, bob.player),
      bracket = Some(Bracket(size = 8, matches = List(completedMatch))),
      frozenRatings = Map(alice.player -> alice, bob.player -> bob),
      raceToByScope = Map("wb-1" -> 7)
    )
  }

  def spec = suite("PeriodEmission")(
    test("periodFilename uses completed date and slugified tournament name") {
      assertTrue(
        PeriodEmission.periodFilename("Spring Open", completed) ==
          "2026-03-15-spring-open.liga"
      )
    },
    test("boardToRack subtracts handicap from weaker player board total") {
      val (rackA, rackB) = PeriodEmission.boardToRack(
        scoreA = 7,
        scoreB = 5,
        weaker = bob.player,
        playerA = alice.player,
        playerB = bob.player,
        handicapApplied = 2
      )
      assertTrue(rackA == 7, rackB == 3)
    },
    test("boardToRack never goes below zero") {
      val (rackA, rackB) = PeriodEmission.boardToRack(
        scoreA = 7,
        scoreB = 1,
        weaker = bob.player,
        playerA = alice.player,
        playerB = bob.player,
        handicapApplied = 5
      )
      assertTrue(rackA == 7, rackB == 0)
    },
    test("emitted handicapped match uses rack-only scores for Glicko2 games") {
      val state =
        handicappedMatchState(boardA = 7, boardB = 5, handicapApplied = 2)
      val period = PeriodEmission.toPeriod(state, completed).toOption.get
      val matchRow = period.matches.head
      val games = ScoreExpansion.expandGames(matchRow.scoreA, matchRow.scoreB)
      assertTrue(
        matchRow.scoreA == 7,
        matchRow.scoreB == 3,
        matchRow.handicapApplied == 2,
        games.size == 10
      )
    },
    test("write emits parseable period file at data root") {
      ZIO.acquireReleaseWith(
        ZIO.attemptBlocking(File.newTemporaryDirectory("liga-period-emission"))
      )(root => ZIO.attemptBlocking(root.delete()).ignore) { root =>
        val state =
          handicappedMatchState(boardA = 7, boardB = 4, handicapApplied = 0)
        for {
          written <- PeriodEmission.write(root, state, completed)
          parsed <- PeriodCodec.parseFile(written)
        } yield assertTrue(
          written.parent == root,
          written.name == "2026-03-15-spring-open.liga",
          parsed.name == "Spring Open",
          parsed.completed == completed,
          parsed.matches.nonEmpty
        )
      }
    },
    test("leaderboard discovers emitted period alongside existing files") {
      ZIO.acquireReleaseWith(
        ZIO.attemptBlocking(
          File.newTemporaryDirectory("liga-period-leaderboard")
        )
      )(root => ZIO.attemptBlocking(root.delete()).ignore) { root =>
        val seed = File(getClass.getResource("/periods/eight-player-seed.liga"))
        ZIO.attemptBlocking(seed.copyTo(root / seed.name)).flatMap { _ =>
          val state =
            handicappedMatchState(boardA = 7, boardB = 4, handicapApplied = 0)
          for {
            before <- PeriodLoader.discover(root)
            _ <- PeriodEmission.write(root, state, completed)
            after <- PeriodLoader.discover(root)
          } yield assertTrue(
            before.size == 1,
            after.size == 2,
            after.last.period.completed == completed
          )
        }
      }
    }
  )
}
