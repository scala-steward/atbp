package ph.samson.atbp.liga.bracket

import ph.samson.atbp.liga.model.*
import ph.samson.atbp.liga.tournament.MatchLifecycle
import zio.test.*

object BracketSpec extends ZIOSpecDefault {

  private def rating(name: String, r: Double, rd: Double): PlayerRating =
    PlayerRating(Player(name), r, rd, wins = 0, losses = 0)

  private def ratings(names: List[String]): List[PlayerRating] =
    names.zipWithIndex.map { case (name, i) =>
      rating(name, 2000 - i * 50, rd = 100)
    }

  private def findMatch(bracket: Bracket, id: String): BracketMatch =
    bracket.matches.find(_.id == id).get

  private def wb2Matches(bracket: Bracket): List[BracketMatch] =
    bracket.matches.filter(_.id.startsWith("wb-2-"))

  private def playerInMatch(matchDef: BracketMatch, name: String): Boolean =
    matchDef.playerA.contains(Player(name)) ||
      matchDef.playerB.contains(Player(name))

  def spec = suite("Bracket")(
    suite("Seeding")(
      test("orders players by rating descending") {
        val players = ratings(List("Alice", "Bob", "Carol", "Dave"))
        val seeds = Seeding.seedOrder(players)
        assertTrue(
          seeds.map(_.player.name) == List("Alice", "Bob", "Carol", "Dave")
        )
      },
      test("tie-break uses lower RD then alphabetical name") {
        val alice = rating("Alice", 1500, rd = 120)
        val bob = rating("Bob", 1500, rd = 80)
        val seeds = Seeding.seedOrder(List(alice, bob))
        assertTrue(seeds.map(_.player.name) == List("Bob", "Alice"))
      },
      test("bracket positions pair 1 vs N for 8 players") {
        val order = Seeding.bracketSlotSeeds(bracketSize = 8)
        assertTrue(order == List(1, 8, 4, 5, 2, 7, 3, 6))
      }
    ),
    suite("BracketGen")(
      test("generates 14 matches for 8 players") {
        val players = ratings((1 to 8).map(i => s"P$i").toList)
        val bracket = BracketGen.generate(players)
        assertTrue(
          bracket.size == 8,
          bracket.matches.size == 14
        )
      },
      test("explicit topN=2 matches default generate") {
        val players = ratings((1 to 8).map(i => s"P$i").toList)
        val default = BracketGen.generate(players)
        val explicit = BracketGen.generate(players, topN = 2)
        assertTrue(
          default.size == explicit.size,
          default.matches.map(m => (m.id, m.playerA, m.playerB, m.state)) ==
            explicit.matches.map(m => (m.id, m.playerA, m.playerB, m.state))
        )
      },
      test("first round winners bracket pairs top seed vs bottom seed") {
        val players = ratings((1 to 8).map(i => s"P$i").toList)
        val bracket = BracketGen.generate(players)
        val first = findMatch(bracket, "wb-1-1")
        assertTrue(
          first.playerA.contains(Player("P1")),
          first.playerB.contains(Player("P8"))
        )
      },
      test("12 players in 16 bracket auto-advances bye matches") {
        val players = ratings((1 to 12).map(i => s"P$i").toList)
        val bracket = BracketGen.generate(players)
        val byeMatches = bracket.matches.filter(m =>
          m.id.startsWith("wb-1-") && m.state == BracketMatchState.Completed
        )
        val byeWinners =
          byeMatches.flatMap(m => m.playerA.orElse(m.playerB)).map(_.name)
        assertTrue(
          bracket.size == 16,
          byeMatches.size == 4,
          byeWinners.sorted == List("P1", "P2", "P3", "P4")
        )
      },
      test("3 players in 4 bracket auto-advances bye match") {
        val players = ratings(List("P1", "P2", "P3"))
        val bracket = BracketGen.generate(players)
        val byeMatches = bracket.matches.filter(m =>
          m.id.startsWith("wb-1-") && m.state == BracketMatchState.Completed
        )
        assertTrue(
          bracket.size == 4,
          bracket.matches.size == 6,
          byeMatches.size == 1,
          byeMatches.head.id == "wb-1-1",
          byeMatches.head.playerA.contains(Player("P1"))
        )
      },
      test("3 players in 4 bracket stops bye cascade at wb-2") {
        val players = ratings(List("P1", "P2", "P3"))
        val bracket = BracketGen.generate(players)
        val bye = findMatch(bracket, "wb-1-1")
        val wb2 = wb2Matches(bracket)
        val p1Slots = wb2.count(playerInMatch(_, "P1"))
        val ready = bracket.matches.filter(_.state == BracketMatchState.Ready)
        assertTrue(
          bye.state == BracketMatchState.Completed,
          bye.isBye,
          bye.result.contains(MatchResult(scoreA = 1, scoreB = 0)),
          p1Slots == 1,
          wb2.forall(_.state == BracketMatchState.Pending),
          ready.size == 1,
          ready.head.id == "wb-1-2",
          findMatch(bracket, "gf-1").playerA.isEmpty,
          findMatch(bracket, "gf-1").playerB.isEmpty
        )
      },
      test("12 players in 16 bracket marks R1 byes and stops cascade") {
        val players = ratings((1 to 12).map(i => s"P$i").toList)
        val bracket = BracketGen.generate(players)
        val byeMatches = bracket.matches.filter(m =>
          m.id.startsWith("wb-1-") && m.state == BracketMatchState.Completed
        )
        val byeWinners =
          byeMatches.flatMap(m => m.playerA.orElse(m.playerB)).map(_.name)
        val wb2 = wb2Matches(bracket)
        val winnersInWb2 = List("P1", "P2", "P3", "P4").map { name =>
          wb2.count(playerInMatch(_, name))
        }
        assertTrue(
          bracket.size == 16,
          byeMatches.size == 4,
          byeMatches.forall(_.isBye),
          byeWinners.sorted == List("P1", "P2", "P3", "P4"),
          winnersInWb2 == List(1, 1, 1, 1),
          wb2.forall(_.state == BracketMatchState.Pending),
          findMatch(bracket, "gf-1").playerA.isEmpty,
          findMatch(bracket, "gf-1").playerB.isEmpty
        )
      },
      test("8 players in 8 bracket has no bye matches") {
        val players = ratings((1 to 8).map(i => s"P$i").toList)
        val bracket = BracketGen.generate(players)
        assertTrue(bracket.matches.forall(!_.isBye))
      },
      test(
        "3 players: losers structural bye after wb-1-2 leaves lb-2-1 ready after wb-2-1"
      ) {
        val players = ratings(List("P1", "P2", "P3"))
        val afterWb12 =
          Advancement
            .advance(BracketGen.generate(players), "wb-1-2", Player("P2"))
            .toOption
            .get
            .bracket
        val lb11 = findMatch(afterWb12, "lb-1-1")
        val lb21AfterWb12 = findMatch(afterWb12, "lb-2-1")
        val afterWb21 =
          Advancement
            .advance(afterWb12, "wb-2-1", Player("P1"))
            .toOption
            .get
            .bracket
        val lb21 = findMatch(afterWb21, "lb-2-1")
        assertTrue(
          lb11.state == BracketMatchState.Completed,
          lb11.isBye,
          lb21AfterWb12.playerA.contains(Player("P3")),
          lb21.state == BracketMatchState.Ready,
          lb21.playerA.contains(Player("P3")),
          lb21.playerB.contains(Player("P2")),
          findMatch(afterWb21, "gf-1").playerA.contains(Player("P1"))
        )
      },
      test(
        "5 players: ghost lb-1-2 marked bye when both WB R1 feeders are byes"
      ) {
        val bracket =
          BracketGen.generate(ratings((1 to 5).map(i => s"P$i").toList))
        val lb12 = findMatch(bracket, "lb-1-2")
        assertTrue(
          bracket.size == 8,
          findMatch(bracket, "wb-1-3").isBye,
          findMatch(bracket, "wb-1-4").isBye,
          lb12.state == BracketMatchState.Completed,
          lb12.isBye,
          lb12.playerA.isEmpty,
          lb12.playerB.isEmpty
        )
      },
      test(
        "5 players: after wb-2-2, lone lb-2-2 player advances past ghost lb-1-2"
      ) {
        val players = ratings((1 to 5).map(i => s"P$i").toList)
        val seeded = BracketGen.generate(players)
        // wb-1-2 is the only real R1 match (P4 vs P5); play it and wb-2 matches
        val afterR1 =
          Advancement
            .advance(seeded, "wb-1-2", Player("P4"))
            .toOption
            .get
            .bracket
        val afterWb21 =
          Advancement
            .advance(afterR1, "wb-2-1", Player("P1"))
            .toOption
            .get
            .bracket
        val afterWb22 =
          Advancement
            .advance(afterWb21, "wb-2-2", Player("P2"))
            .toOption
            .get
            .bracket
        val lb22 = findMatch(afterWb22, "lb-2-2")
        assertTrue(
          findMatch(afterWb22, "lb-1-2").isBye,
          lb22.state == BracketMatchState.Completed,
          lb22.isBye,
          findMatch(afterWb22, "lb-3-1").playerB.contains(Player("P3"))
        )
      },
      test("partial fills 3..64: play-out reaches ready grand final") {
        val stuck = (3 to 64).flatMap { n =>
          val played =
            playOutTournament(ratings((1 to n).map(i => s"P$i").toList), 500, 2)
          val gf = findMatch(played, "gf-1")
          if (gf.state == BracketMatchState.Ready) None
          else Some(n)
        }
        assertTrue(stuck.isEmpty)
      },
      test("supports bracket sizes 4, 8, 16, 32, and 64") {
        val sizes = List(4, 8, 16, 32, 64)
        val checks = sizes.map { size =>
          val players = ratings((1 to size).map(i => s"P$i").toList)
          val bracket = BracketGen.generate(players)
          bracket.size == size && bracket.matches.size == 2 * size - 2
        }
        assertTrue(checks.forall(identity))
      }
    ),
    suite("Advancement")(
      test("winners bracket advancement marks next match ready") {
        val players = ratings((1 to 8).map(i => s"P$i").toList)
        val bracket = BracketGen.generate(players)
        val after1 =
          Advancement.advance(bracket, "wb-1-4", Player("P3")).toOption.get
        val after2 =
          Advancement
            .advance(after1.bracket, "wb-1-3", Player("P2"))
            .toOption
            .get
        val next = findMatch(after2.bracket, "wb-2-2")
        assertTrue(
          next.playerA.contains(Player("P2")),
          next.playerB.contains(Player("P3")),
          next.state == BracketMatchState.Ready
        )
      },
      test("losers drop from winners bracket into losers bracket") {
        val players = ratings((1 to 8).map(i => s"P$i").toList)
        val bracket = BracketGen.generate(players)
        val after =
          Advancement.advance(bracket, "wb-1-4", Player("P3")).toOption.get
        val lb1 = findMatch(after.bracket, "lb-1-2")
        assertTrue(lb1.playerB.contains(Player("P6")))
      },
      test("advance without placeholder result completes without scores") {
        val players = ratings((1 to 8).map(i => s"P$i").toList)
        val bracket = BracketGen.generate(players)
        val after = Advancement
          .advance(
            bracket,
            "wb-1-4",
            Player("P3"),
            recordPlaceholderResult = false
          )
          .toOption
          .get
        val completed = findMatch(after.bracket, "wb-1-4")
        assertTrue(
          completed.state == BracketMatchState.Completed,
          completed.result.isEmpty
        )
      },
      test("grand final becomes ready when both brackets resolve") {
        val players = ratings((1 to 8).map(i => s"P$i").toList)
        val played = playOutTournament(players, maxSteps = 100, topN = 2)
        val gf = findMatch(played, "gf-1")
        assertTrue(
          gf.state == BracketMatchState.Ready,
          gf.playerA.nonEmpty,
          gf.playerB.nonEmpty
        )
      }
    ),
    suite("Top 1 reset grand final")(
      test("topN=1 generates same match graph as topN=2") {
        val players = ratings((1 to 8).map(i => s"P$i").toList)
        val top1 = BracketGen.generate(players, topN = 1)
        val top2 = BracketGen.generate(players, topN = 2)
        assertTrue(
          top1.topN == 1,
          top2.topN == 2,
          top1.matches.map(m => (m.id, m.playerA, m.playerB, m.state)) ==
            top2.matches.map(m => (m.id, m.playerA, m.playerB, m.state))
        )
      },
      test("losers drop from winners bracket into losers bracket") {
        val players = ratings((1 to 8).map(i => s"P$i").toList)
        val bracket = BracketGen.generate(players, topN = 1)
        val after =
          Advancement.advance(bracket, "wb-1-4", Player("P3")).toOption.get
        val lb1 = findMatch(after.bracket, "lb-1-2")
        assertTrue(lb1.playerB.contains(Player("P6")))
      },
      test("LB winning gf-1 creates ready gf-2 with fixed seating") {
        val players = ratings((1 to 8).map(i => s"P$i").toList)
        val atGf1 = playOutToGrandFinal(players, topN = 1)
        val gf1 = findMatch(atGf1, "gf-1")
        val lbChamp = gf1.playerB.get
        val wbChamp = gf1.playerA.get
        val after =
          Advancement.advance(atGf1, "gf-1", lbChamp).toOption.get
        val gf2 = findMatch(after.bracket, "gf-2")
        assertTrue(
          gf2.state == BracketMatchState.Ready,
          gf2.playerA.contains(wbChamp),
          gf2.playerB.contains(lbChamp),
          after.newlyReady.contains("gf-2")
        )
      },
      test("WB winning gf-1 does not create gf-2") {
        val players = ratings((1 to 8).map(i => s"P$i").toList)
        val atGf1 = playOutToGrandFinal(players, topN = 1)
        val gf1 = findMatch(atGf1, "gf-1")
        val wbChamp = gf1.playerA.get
        val after =
          Advancement.advance(atGf1, "gf-1", wbChamp).toOption.get
        assertTrue(
          after.bracket.matches.forall(_.id != "gf-2"),
          after.newlyReady.forall(_ != "gf-2")
        )
      },
      test("resolveRaceTo uses gf scope for gf-2") {
        val players = ratings((1 to 8).map(i => s"P$i").toList)
        val atGf1 = playOutToGrandFinal(players, topN = 1)
        val gf1 = findMatch(atGf1, "gf-1")
        val after =
          Advancement.advance(atGf1, "gf-1", gf1.playerB.get).toOption.get
        val state = TournamentState(
          name = "test",
          players = players.map(_.player),
          bracket = Some(after.bracket),
          raceToByScope = Map("gf" -> 9)
        )
        assertTrue(MatchLifecycle.resolveRaceTo(state, "gf-2") == Right(9))
      }
    ),
    suite("Full single elimination")(
      test("topN=field generates se-only matches for power-of-two roster") {
        val players = ratings((1 to 8).map(i => s"P$i").toList)
        val bracket = BracketGen.generate(players, topN = 8)
        val ids = bracket.matches.map(_.id)
        assertTrue(
          bracket.topN == 8,
          bracket.size == 8,
          bracket.matches.size == 7,
          ids.forall(_.startsWith("se-")),
          !ids.exists(_.startsWith("lb-")),
          !ids.contains("gf-1")
        )
      },
      test("first round pairs top seed vs bottom seed") {
        val players = ratings((1 to 8).map(i => s"P$i").toList)
        val bracket = BracketGen.generate(players, topN = 8)
        val first = findMatch(bracket, "se-1-1")
        assertTrue(
          first.playerA.contains(Player("P1")),
          first.playerB.contains(Player("P8"))
        )
      },
      test("advancement flows through single elimination only") {
        val players = ratings((1 to 8).map(i => s"P$i").toList)
        val bracket = BracketGen.generate(players, topN = 8)
        val after1 =
          Advancement.advance(bracket, "se-1-4", Player("P3")).toOption.get
        val after2 =
          Advancement
            .advance(after1.bracket, "se-1-3", Player("P2"))
            .toOption
            .get
        val next = findMatch(after2.bracket, "se-2-2")
        assertTrue(
          next.playerA.contains(Player("P2")),
          next.playerB.contains(Player("P3")),
          next.state == BracketMatchState.Ready,
          after2.bracket.matches.forall(!_.id.startsWith("lb-"))
        )
      },
      test("play-out completes through se final with no gf-1") {
        val players = ratings((1 to 8).map(i => s"P$i").toList)
        val played = playOutSingleElim(players, topN = 8)
        val finalRound = log2(8)
        val finalMatch = findMatch(played, s"se-$finalRound-1")
        assertTrue(
          finalMatch.state == BracketMatchState.Completed,
          played.matches.forall(!_.id.startsWith("lb-")),
          played.matches.forall(_.id != "gf-1")
        )
      },
      test("supports bracket sizes 4, 8, 16, 32, and 64") {
        val sizes = List(4, 8, 16, 32, 64)
        val checks = sizes.map { size =>
          val players = ratings((1 to size).map(i => s"P$i").toList)
          val bracket = BracketGen.generate(players, topN = size)
          bracket.size == size &&
          bracket.matches.size == size - 1 &&
          bracket.matches.forall(_.id.startsWith("se-"))
        }
        assertTrue(checks.forall(identity))
      },
      test("non-power-of-two roster cannot use N=field as full SE") {
        val illegal = TopN.legalTopNs(12).contains(12)
        assertTrue(!illegal)
      }
    ),
    suite("Top 4+ cut double elimination")(
      test("12 locked topN=8 uses 16-slot DE with SE skeleton and no gf") {
        val players = ratings((1 to 12).map(i => s"P$i").toList)
        val bracket = BracketGen.generate(players, topN = 8)
        val ids = bracket.matches.map(_.id)
        val wbRounds =
          ids
            .filter(_.startsWith("wb-"))
            .map(_.split("-")(1).toInt)
            .distinct
            .sorted
        val lbRounds =
          ids
            .filter(_.startsWith("lb-"))
            .map(_.split("-")(1).toInt)
            .distinct
            .sorted
        val seMatches = bracket.matches.filter(_.id.startsWith("se-"))
        assertTrue(
          bracket.topN == 8,
          bracket.size == 16,
          ids.exists(_.startsWith("wb-")),
          ids.exists(_.startsWith("lb-")),
          !ids.contains("gf-1"),
          wbRounds == List(1, 2),
          lbRounds == List(1, 2),
          seMatches.size == 7,
          seMatches.forall(m => m.playerA.isEmpty && m.playerB.isEmpty),
          seMatches.forall(_.state == BracketMatchState.Pending)
        )
      },
      test("opening byes unchanged for cut format") {
        val players = ratings((1 to 12).map(i => s"P$i").toList)
        val bracket = BracketGen.generate(players, topN = 8)
        val wb1 = bracket.matches.filter(_.id.startsWith("wb-1-"))
        val byeMatches = wb1.count(m => m.playerA.isEmpty ^ m.playerB.isEmpty)
        assertTrue(
          wb1.size == 8,
          byeMatches == 4
        )
      },
      test("truncated LB has no rounds beyond cut") {
        val topology = BracketTopology(Seeding.bracketSize(12), topN = 8)
        val maxLbRound =
          topology.matches.keys
            .filter(_.startsWith("lb-"))
            .map(_.split("-")(1).toInt)
            .max
        val maxWbRound =
          topology.matches.keys
            .filter(_.startsWith("wb-"))
            .map(_.split("-")(1).toInt)
            .max
        assertTrue(maxWbRound == 2, maxLbRound == 2)
      },
      test("every cut DE loserTo target exists in the match graph") {
        val cases =
          for {
            size <- List(8, 16, 32, 64)
            topN <- List(4, 8, 16, 32).filter(n => n >= 4 && n < size)
          } yield (size, topN)

        val missing = cases.flatMap { case (size, topN) =>
          val topology = BracketTopology(size, topN)
          val matchIds = topology.matches.keySet
          topology.loserTo.collect {
            case (src, (dest, _)) if !matchIds.contains(dest) =>
              s"size=$size topN=$topN $src->$dest"
          }
        }
        assertTrue(missing.isEmpty)
      },
      test("16-slot Top 4 LB is deep enough for WB R3 drops") {
        val topology = BracketTopology(16, topN = 4)
        val matchIds = topology.matches.keySet
        val lbRounds =
          matchIds
            .filter(_.startsWith("lb-"))
            .map(_.split("-")(1).toInt)
            .toSet
        assertTrue(
          topology.loserTo.get("wb-3-1").exists { case (dest, _) =>
            matchIds.contains(dest)
          },
          topology.loserTo.get("wb-3-2").exists { case (dest, _) =>
            matchIds.contains(dest)
          },
          lbRounds == Set(1, 2, 3, 4)
        )
      },
      test("SE skeleton has internal advancement wiring only") {
        val topology = BracketTopology(16, topN = 8)
        val deWinnerDestinations = topology.winnerTo.collect {
          case (src, (dest, _))
              if src.startsWith("wb-") || src.startsWith("lb-") =>
            dest
        }.toSet
        val seFeeders = topology.matches.collect {
          case (id, defn) if id.startsWith("se-") =>
            List(defn.feederA, defn.feederB)
        }.flatten
        assertTrue(
          topology.winnerTo
            .get("se-1-1")
            .contains(("se-2-1", BracketTopology.Slot.A)),
          topology.winnerTo
            .get("se-2-1")
            .contains(("se-3-1", BracketTopology.Slot.A)),
          !topology.matches.contains("wb-3-1"),
          !topology.matches.contains("wb-4-1"),
          !topology.matches.contains("lb-3-1"),
          !topology.winnerTo.contains("wb-2-1"),
          !topology.winnerTo.contains("lb-2-1"),
          !deWinnerDestinations.exists(_.startsWith("se-")),
          seFeeders.forall {
            case BracketTopology.Feeder.Empty        => true
            case BracketTopology.Feeder.WinnerOf(id) => id.startsWith("se-")
            case _                                   => false
          }
        )
      },
      test("30-player Top 8 never opens wb-4 or lb-5 before cut") {
        val players = ratings((1 to 30).map(i => s"P$i").toList)
        val context = cutReseedContext(players)

        def loop(
            bracket: Bracket,
            steps: Int,
            sawLateDeReady: Boolean
        ): (Bracket, Boolean) =
          if (steps <= 0 || CutReseed.cutAlreadyDone(bracket)) {
            (bracket, sawLateDeReady)
          } else {
            val lateReady = bracket.matches.exists { m =>
              m.state == BracketMatchState.Ready &&
              (m.id.startsWith("wb-4-") ||
                m.id.startsWith("wb-5-") ||
                m.id.startsWith("lb-5-") ||
                m.id.startsWith("lb-6-"))
            }
            advanceReadyCut(bracket, context) match {
              case None       => (bracket, sawLateDeReady || lateReady)
              case Some(next) =>
                loop(next, steps - 1, sawLateDeReady || lateReady)
            }
          }

        val seeded = BracketGen.generate(players, topN = 8)
        val ids = seeded.matches.map(_.id).toSet
        val (played, sawLateDeReady) =
          loop(seeded, steps = 2000, sawLateDeReady = false)
        assertTrue(
          !ids.exists(_.startsWith("wb-4-")),
          !ids.exists(_.startsWith("wb-5-")),
          !ids.exists(_.startsWith("lb-5-")),
          !ids.exists(_.startsWith("lb-6-")),
          !sawLateDeReady,
          CutReseed.cutAlreadyDone(played)
        )
      },
      test("30-player Top 8 play-out completes through se final") {
        val players = ratings((1 to 30).map(i => s"P$i").toList)
        val played = playOutCutTournament(players, topN = 8)
        val finalMatch = findMatch(played, "se-3-1")
        assertTrue(finalMatch.state == BracketMatchState.Completed)
      },
      test("12 locked topN=8 play-out completes through se final") {
        val players = ratings((1 to 12).map(i => s"P$i").toList)
        val played = playOutCutTournament(players, topN = 8)
        val finalMatch = findMatch(played, "se-3-1")
        assertTrue(finalMatch.state == BracketMatchState.Completed)
      },
      test("after cut completed DE matches keep both players") {
        val afterCut =
          playUntilCut(
            ratings((1 to 12).map(i => s"P$i").toList),
            topN = 8,
            steps = 500
          )
        val completedDe = afterCut.matches.filter(m =>
          !m.id.startsWith("se-") &&
            m.state == BracketMatchState.Completed &&
            !m.isBye &&
            m.result.nonEmpty
        )
        assertTrue(
          CutReseed.cutAlreadyDone(afterCut),
          completedDe.nonEmpty,
          completedDe.forall(m => m.playerA.nonEmpty && m.playerB.nonEmpty)
        )
      },
      test("cut play-out leaves every match Completed for period emission") {
        val players = ratings((1 to 12).map(i => s"P$i").toList)
        val played = playOutCutTournament(players, topN = 8)
        val incomplete =
          played.matches.filter(_.state != BracketMatchState.Completed)
        assertTrue(incomplete.isEmpty)
      },
      test(
        "LB forfeit at cut counts elimination when forfeit is set before advance"
      ) {
        val players = ratings((1 to 12).map(i => s"P$i").toList)
        val context = cutReseedContext(players)
        val topN = 8

        def playUntilForfeitCandidate(bracket: Bracket, steps: Int): Bracket =
          if (steps <= 0 || CutReseed.cutAlreadyDone(bracket)) {
            bracket
          } else {
            val alive = CutReseed.survivors(bracket, context.players)
            val candidate = bracket.matches.find(m =>
              m.id.startsWith("lb-") &&
                m.state == BracketMatchState.Ready &&
                m.playerA.nonEmpty &&
                m.playerB.nonEmpty &&
                alive.size == topN + 1
            )
            candidate match {
              case Some(_) => bracket
              case None    =>
                advanceReadyCut(bracket, context) match {
                  case None       => bracket
                  case Some(next) =>
                    playUntilForfeitCandidate(next, steps - 1)
                }
            }
          }

        val before = playUntilForfeitCandidate(
          BracketGen.generate(players, topN = topN),
          steps = 500
        )
        val lb = before.matches
          .find(m =>
            m.id.startsWith("lb-") &&
              m.state == BracketMatchState.Ready &&
              m.playerA.nonEmpty &&
              m.playerB.nonEmpty
          )
          .get
        val winner = lb.playerB.get
        val withoutForfeit = Advancement
          .advance(
            before,
            lb.id,
            winner,
            recordPlaceholderResult = false,
            completedResult = None,
            reseedContext = Some(context)
          )
          .toOption
          .get
        val prepared = before.copy(
          matches = before.matches.map { current =>
            if (current.id == lb.id) {
              current.copy(
                forfeit = Some(
                  MatchForfeitInfo(forfeitingSide = "A", reason = "no-show")
                )
              )
            } else {
              current
            }
          }
        )
        val advanced = Advancement
          .advance(
            prepared,
            lb.id,
            winner,
            recordPlaceholderResult = false,
            completedResult = None,
            reseedContext = Some(context)
          )
          .toOption
          .get
        assertTrue(
          CutReseed.survivors(before, context.players).size == topN + 1,
          !CutReseed.cutAlreadyDone(withoutForfeit.bracket),
          CutReseed.cutAlreadyDone(advanced.bracket),
          findMatch(advanced.bracket, lb.id).forfeit.nonEmpty
        )
      },
      test("cut reseeds survivors once into se-1") {
        val played =
          playUntilCut(
            ratings((1 to 12).map(i => s"P$i").toList),
            topN = 8,
            steps = 500
          )
        val seRound1 = played.matches.filter(_.id.startsWith("se-1-"))
        assertTrue(
          CutReseed.cutAlreadyDone(played),
          seRound1.forall(m => m.playerA.nonEmpty && m.playerB.nonEmpty)
        )
      },
      test("after cut no losers bracket match becomes ready") {
        val players = ratings((1 to 12).map(i => s"P$i").toList)
        val context = cutReseedContext(players)

        def loop(
            bracket: Bracket,
            steps: Int,
            afterCut: Boolean,
            lbReadyAfterCut: Boolean
        ): (Bracket, Boolean, Boolean) =
          if (steps <= 0) {
            (bracket, afterCut, lbReadyAfterCut)
          } else if (CutReseed.cutAlreadyDone(bracket)) {
            val lbReady =
              lbReadyAfterCut ||
                bracket.matches.exists(m =>
                  m.id.startsWith("lb-") && m.state == BracketMatchState.Ready
                )
            advanceReadyCut(bracket, context) match {
              case None       => (bracket, true, lbReady)
              case Some(next) =>
                loop(next, steps - 1, afterCut = true, lbReady)
            }
          } else {
            advanceReadyCut(bracket, context) match {
              case None       => (bracket, afterCut, lbReadyAfterCut)
              case Some(next) =>
                loop(next, steps - 1, afterCut, lbReadyAfterCut)
            }
          }

        val (_, afterCut, lbReadyAfterCut) =
          loop(
            BracketGen.generate(players, topN = 8),
            steps = 500,
            afterCut = false,
            lbReadyAfterCut = false
          )
        assertTrue(afterCut, !lbReadyAfterCut)
      },
      test("handicapped match earned racks excluded from cut reseed input") {
        val ratings12 = ratings(List("P11", "P12"))
        val handicapped =
          BracketMatch(
            id = "wb-1-8",
            playerA = Some(Player("P11")),
            playerB = Some(Player("P12")),
            state = BracketMatchState.Completed,
            handicapApplied = Some(3),
            result = Some(MatchResult(scoreA = 7, scoreB = 4))
          )
        val bracket = Bracket(
          size = 16,
          matches = List(handicapped),
          topN = 8
        )
        val played =
          CutReseed.playedMatches(
            bracket,
            cutReseedContext(ratings12).frozenRatings
          )
        assertTrue(
          played.size == 1,
          played.head.handicapApplied == 3,
          EarnedRacks.earnedScores(played.head) == ((7, 1))
        )
      }
    )
  )

  /** Play every ready match, always picking playerA as winner, until grand
    * final is ready.
    */
  private def playOutTournament(
      players: List[PlayerRating],
      maxSteps: Int,
      topN: Int
  ): Bracket = {
    def loop(bracket: Bracket, steps: Int): Bracket = {
      val gf = findMatch(bracket, "gf-1")
      if (gf.state == BracketMatchState.Ready || steps <= 0) {
        bracket
      } else {
        bracket.matches.find(_.state == BracketMatchState.Ready) match {
          case None       => bracket
          case Some(next) =>
            val winner = next.playerA.orElse(next.playerB).get
            Advancement.advance(bracket, next.id, winner) match {
              case Left(_)       => bracket
              case Right(result) => loop(result.bracket, steps - 1)
            }
        }
      }
    }
    loop(BracketGen.generate(players, topN = topN), maxSteps)
  }

  private def playOutToGrandFinal(
      players: List[PlayerRating],
      topN: Int
  ): Bracket =
    playOutTournament(players, maxSteps = 500, topN = topN)

  private def log2(n: Int): Int =
    (math.log(n) / math.log(2)).toInt

  /** Play every ready match until the single-elimination final is completed. */
  private def playOutSingleElim(
      players: List[PlayerRating],
      topN: Int
  ): Bracket = {
    val rounds = log2(topN)
    val finalId = s"se-$rounds-1"

    def loop(bracket: Bracket, steps: Int): Bracket = {
      val finalMatch = findMatch(bracket, finalId)
      if (finalMatch.state == BracketMatchState.Completed || steps <= 0) {
        bracket
      } else {
        bracket.matches.find(_.state == BracketMatchState.Ready) match {
          case None       => bracket
          case Some(next) =>
            val winner = next.playerA.orElse(next.playerB).get
            Advancement.advance(bracket, next.id, winner) match {
              case Left(_)       => bracket
              case Right(result) => loop(result.bracket, steps - 1)
            }
        }
      }
    }
    loop(BracketGen.generate(players, topN = topN), steps = 500)
  }

  private def cutReseedContext(players: List[PlayerRating]): CutReseed.Context =
    CutReseed.Context(
      players = players.map(_.player),
      frozenRatings = players.map(r => r.player -> r).toMap
    )

  /** Advance the next Ready match with cut reseed context; `None` if none. */
  private def advanceReadyCut(
      bracket: Bracket,
      context: CutReseed.Context
  ): Option[Bracket] =
    bracket.matches.find(_.state == BracketMatchState.Ready).flatMap { next =>
      val winner = next.playerA.orElse(next.playerB).get
      Advancement
        .advance(
          bracket,
          next.id,
          winner,
          recordPlaceholderResult = true,
          completedResult = None,
          reseedContext = Some(context)
        )
        .toOption
        .map(_.bracket)
    }

  /** Play Ready matches with cut reseed until the SE cut has been applied. */
  private def playUntilCut(
      players: List[PlayerRating],
      topN: Int,
      steps: Int
  ): Bracket = {
    val context = cutReseedContext(players)
    def loop(bracket: Bracket, remaining: Int): Bracket =
      if (CutReseed.cutAlreadyDone(bracket) || remaining <= 0) {
        bracket
      } else {
        advanceReadyCut(bracket, context) match {
          case None       => bracket
          case Some(next) => loop(next, remaining - 1)
        }
      }
    loop(BracketGen.generate(players, topN = topN), steps)
  }

  /** Play every ready match until the single-elimination final is completed. */
  private def playOutCutTournament(
      players: List[PlayerRating],
      topN: Int
  ): Bracket = {
    val context = cutReseedContext(players)
    val finalId = s"se-${log2(topN)}-1"

    def loop(bracket: Bracket, remaining: Int): Bracket = {
      val finalMatch = findMatch(bracket, finalId)
      if (finalMatch.state == BracketMatchState.Completed || remaining <= 0) {
        bracket
      } else {
        advanceReadyCut(bracket, context) match {
          case None       => bracket
          case Some(next) => loop(next, remaining - 1)
        }
      }
    }
    loop(BracketGen.generate(players, topN = topN), remaining = 500)
  }

}
