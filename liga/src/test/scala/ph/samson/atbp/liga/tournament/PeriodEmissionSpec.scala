package ph.samson.atbp.liga.tournament

import better.files.File
import ph.samson.atbp.liga.io.PeriodCodec
import ph.samson.atbp.liga.io.PeriodLoader
import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.testsupport.PeriodHoconTestSupport
import zio.ZIO
import zio.test.*

import java.time.LocalDate

object PeriodEmissionSpec extends ZIOSpecDefault {

  private val completed = LocalDate.parse("2026-03-15")

  private def rating(name: String, r: Double): PlayerRating =
    PlayerRating(Player(name), r, rd = 100, wins = 0, losses = 0)

  private val alice = rating("Alice", 1700)
  private val bob = rating("Bob", 1400)
  private val carol = rating("Carol", 1500)

  private def completedMatch(
      id: String,
      playerA: PlayerRating,
      playerB: PlayerRating,
      boardA: Int,
      boardB: Int,
      handicapApplied: Int
  ): BracketMatch =
    BracketMatch(
      id = id,
      playerA = Some(playerA.player),
      playerB = Some(playerB.player),
      state = BracketMatchState.Completed,
      handicapSuggested = Some(handicapApplied),
      handicapApplied = Some(handicapApplied),
      result = Some(MatchResult(boardA, boardB))
    )

  private def forfeitedMatch(
      id: String,
      playerA: PlayerRating,
      playerB: PlayerRating,
      forfeitingSide: String,
      reason: String
  ): BracketMatch =
    BracketMatch(
      id = id,
      playerA = Some(playerA.player),
      playerB = Some(playerB.player),
      state = BracketMatchState.Completed,
      result = None,
      isBye = false,
      forfeit = Some(
        MatchForfeitInfo(forfeitingSide = forfeitingSide, reason = reason)
      )
    )

  private def handicappedMatchState(
      boardA: Int,
      boardB: Int,
      handicapApplied: Int
  ): TournamentState = {
    TournamentState(
      name = "Spring Open",
      players = List(alice.player, bob.player),
      bracket = Some(
        Bracket(
          size = 8,
          matches = List(
            completedMatch(
              "wb-1-1",
              alice,
              bob,
              boardA,
              boardB,
              handicapApplied
            )
          )
        )
      ),
      frozenRatings = Map(alice.player -> alice, bob.player -> bob),
      raceToByScope = Map("wb-1" -> 7)
    )
  }

  /** Minimal multi-match completed tournament spanning WB / LB / GF IDs. */
  private def multiMatchCompletedState: TournamentState =
    TournamentState(
      name = "Spring Open",
      players = List(alice.player, bob.player, carol.player),
      bracket = Some(
        Bracket(
          size = 4,
          matches = List(
            completedMatch(
              "wb-1-1",
              alice,
              bob,
              boardA = 7,
              boardB = 4,
              handicapApplied = 0
            ),
            completedMatch(
              "lb-1-1",
              bob,
              carol,
              boardA = 5,
              boardB = 7,
              handicapApplied = 0
            ),
            completedMatch(
              "gf-1",
              alice,
              carol,
              boardA = 7,
              boardB = 3,
              handicapApplied = 0
            )
          )
        )
      ),
      frozenRatings = Map(
        alice.player -> alice,
        bob.player -> bob,
        carol.player -> carol
      ),
      raceToByScope = Map("wb-1" -> 7, "lb-1" -> 7, "gf" -> 7)
    )

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
    },
    test("emitted match objects start with # <BracketMatch.id> comments") {
      ZIO.acquireReleaseWith(
        ZIO.attemptBlocking(
          File.newTemporaryDirectory("liga-period-match-id-comments")
        )
      )(root => ZIO.attemptBlocking(root.delete()).ignore) { root =>
        for {
          written <- PeriodEmission.write(
            root,
            multiMatchCompletedState,
            completed
          )
          text <- ZIO.attemptBlocking(written.contentAsString)
        } yield {
          val firstLines = PeriodHoconTestSupport.firstInteriorLines(text)
          assertTrue(
            // Emission sorts matches by BracketMatch.id
            firstLines == List("# gf-1", "# lb-1-1", "# wb-1-1"),
            text.contains("# wb-1-1"),
            text.contains("# lb-1-1"),
            text.contains("# gf-1")
          )
        }
      }
    },
    test("writeOrVerify succeeds when existing commented file matches period") {
      ZIO.acquireReleaseWith(
        ZIO.attemptBlocking(
          File.newTemporaryDirectory("liga-period-write-or-verify-ok")
        )
      )(root => ZIO.attemptBlocking(root.delete()).ignore) { root =>
        val state = multiMatchCompletedState
        for {
          written <- PeriodEmission.write(root, state, completed)
          before <- ZIO.attemptBlocking(written.contentAsString)
          _ <- PeriodEmission.writeOrVerify(root, state, completed)
          after <- ZIO.attemptBlocking(written.contentAsString)
        } yield assertTrue(
          before.contains("# wb-1-1"),
          after == before
        )
      }
    },
    test("writeOrVerify fails when existing file differs semantically") {
      ZIO.acquireReleaseWith(
        ZIO.attemptBlocking(
          File.newTemporaryDirectory("liga-period-write-or-verify-mismatch")
        )
      )(root => ZIO.attemptBlocking(root.delete()).ignore) { root =>
        val original = multiMatchCompletedState
        val mismatched = original.copy(
          bracket = original.bracket.map { bracket =>
            bracket.copy(
              matches = bracket.matches.map {
                case m if m.id == "wb-1-1" =>
                  m.copy(result = Some(MatchResult(7, 0)))
                case other => other
              }
            )
          }
        )
        for {
          _ <- PeriodEmission.write(root, original, completed)
          result <- PeriodEmission
            .writeOrVerify(root, mismatched, completed)
            .either
        } yield assertTrue(
          result.isLeft,
          result.left.exists(_.getMessage.contains("mismatch"))
        )
      }
    },
    test("forfeited matches are omitted while scored matches still emit") {
      val state = TournamentState(
        name = "Spring Open",
        players = List(alice.player, bob.player, carol.player),
        bracket = Some(
          Bracket(
            size = 4,
            matches = List(
              completedMatch(
                "wb-1-1",
                alice,
                bob,
                boardA = 7,
                boardB = 4,
                handicapApplied = 0
              ),
              forfeitedMatch(
                "lb-1-1",
                bob,
                carol,
                forfeitingSide = "A",
                reason = "no-show"
              ),
              completedMatch(
                "gf-1",
                alice,
                carol,
                boardA = 7,
                boardB = 3,
                handicapApplied = 0
              )
            )
          )
        ),
        frozenRatings = Map(
          alice.player -> alice,
          bob.player -> bob,
          carol.player -> carol
        ),
        raceToByScope = Map("wb-1" -> 7, "lb-1" -> 7, "gf" -> 7)
      )
      val period = PeriodEmission.toPeriod(state, completed).toOption.get
      val pairs =
        period.matches.map(m => (m.playerA.name, m.playerB.name)).toSet
      assertTrue(
        period.matches.size == 2,
        pairs == Set(("Alice", "Bob"), ("Alice", "Carol"))
      )
    },
    test("all-forfeit tournament cannot emit a period") {
      val state = TournamentState(
        name = "Spring Open",
        players = List(alice.player, bob.player),
        bracket = Some(
          Bracket(
            size = 2,
            matches = List(
              forfeitedMatch(
                "wb-1-1",
                alice,
                bob,
                forfeitingSide = "B",
                reason = "illness"
              )
            )
          )
        ),
        frozenRatings = Map(alice.player -> alice, bob.player -> bob),
        raceToByScope = Map("wb-1" -> 7)
      )
      val result = PeriodEmission.toPeriod(state, completed)
      assertTrue(
        result.isLeft,
        result.left.exists(_.contains("no recorded match results"))
      )
    }
  )
}
