package ph.samson.atbp.liga.bracket

import ph.samson.atbp.liga.model.Player

/** Earned-rack totals and cut reseed ordering for Top N single elimination. */
object EarnedRacks {

  enum MatchKind {
    case Played, Bye, Forfeit
  }

  final case class PlayedMatch(
      playerA: Player,
      playerB: Player,
      scoreA: Int,
      scoreB: Int,
      handicapApplied: Int,
      weaker: Option[Player],
      kind: MatchKind
  )

  final case class SurvivorProfile(
      player: Player,
      ratingSeed: Double,
      drawOrder: Int
  )

  def earnedScores(played: PlayedMatch): (Int, Int) =
    played.kind match {
      case MatchKind.Played =>
        played.weaker match {
          case Some(weaker) if weaker == played.playerA =>
            (math.max(0, played.scoreA - played.handicapApplied), played.scoreB)
          case Some(weaker) if weaker == played.playerB =>
            (played.scoreA, math.max(0, played.scoreB - played.handicapApplied))
          case _ =>
            (played.scoreA, played.scoreB)
        }
      case MatchKind.Bye | MatchKind.Forfeit =>
        (0, 0)
    }

  def rackDifferential(
      matches: List[PlayedMatch],
      player: Player
  ): Int = {
    val (won, lost) = matches.foldLeft((0, 0)) {
      case ((wonTotal, lostTotal), played) =>
        val (scoreA, scoreB) = earnedScores(played)
        if (played.playerA == player) {
          (wonTotal + scoreA, lostTotal + scoreB)
        } else if (played.playerB == player) {
          (wonTotal + scoreB, lostTotal + scoreA)
        } else {
          (wonTotal, lostTotal)
        }
    }
    won - lost
  }

  def rankSurvivors(
      survivors: List[Player],
      matches: List[PlayedMatch],
      profiles: Map[Player, SurvivorProfile]
  ): Either[String, List[Player]] =
    survivors.find(player => !profiles.contains(player)) match {
      case Some(missing) =>
        Left(s"missing survivor profile for ${missing.name}")
      case None =>
        Right(
          survivors.sortBy { player =>
            val profile = profiles(player)
            (
              -rackDifferential(matches, player),
              -profile.ratingSeed,
              profile.drawOrder
            )
          }
        )
    }

  /** Ranked best-first players placed into standard SE bracket slot order. */
  def assignSingleElimSlots(rankedPlayers: List[Player]): List[Player] = {
    val size = rankedPlayers.size
    require(
      isPowerOfTwo(size),
      s"single elimination cut requires a power-of-two survivor count: $size"
    )
    val slotSeeds = bracketSlotSeeds(size)
    slotSeeds.map(seed => rankedPlayers(seed - 1))
  }

  private def bracketSlotSeeds(bracketSize: Int): List[Int] = {
    require(
      isPowerOfTwo(bracketSize),
      s"bracket size must be a power of two: $bracketSize"
    )
    val rounds = log2(bracketSize)
    (1 until rounds).foldLeft(List(1, 2)) { (seeds, _) =>
      val nextSize = seeds.length * 2
      seeds.flatMap(seed => List(seed, nextSize + 1 - seed))
    }
  }

  private def isPowerOfTwo(n: Int): Boolean =
    n > 0 && (n & (n - 1)) == 0

  private def log2(n: Int): Int =
    (math.log(n) / math.log(2)).toInt
}
