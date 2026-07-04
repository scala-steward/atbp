package ph.samson.atbp.liga.bracket

/** Internal double-elimination match graph for a fixed bracket size. */
private[bracket] object BracketTopology {

  enum Slot {
    case A, B
  }

  enum Feeder {
    case Empty
    case Seed(number: Int)
    case WinnerOf(matchId: String)
    case LoserOf(matchId: String)
  }

  final case class MatchDef(
      id: String,
      feederA: Feeder,
      feederB: Feeder
  )

  final case class Topology(
      size: Int,
      matches: Map[String, MatchDef],
      winnerTo: Map[String, (String, Slot)],
      loserTo: Map[String, (String, Slot)]
  )

  private final case class BracketMaps(
      matches: Map[String, MatchDef],
      winnerTo: Map[String, (String, Slot)],
      loserTo: Map[String, (String, Slot)]
  )

  def apply(size: Int): Topology = {
    require(isPowerOfTwo(size), s"bracket size must be a power of two: $size")
    val winnersRounds = log2(size)
    val wb = buildWinnersBracket(size, winnersRounds)
    val lb = buildLosersBracket(size, winnersRounds)
    val gfMatch = buildGrandFinal(winnersRounds)

    Topology(
      size = size,
      matches = wb.matches ++ lb.matches + gfMatch,
      winnerTo = wb.winnerTo ++ lb.winnerTo,
      loserTo = wb.loserTo
    )
  }

  private def buildWinnersBracket(
      size: Int,
      winnersRounds: Int
  ): BracketMaps = {
    val seedOrder = Seeding.bracketSlotSeeds(size)
    val slots =
      for {
        round <- 1 to winnersRounds
        index <- 1 to (size >> round)
      } yield (round, index)

    slots.foldLeft(BracketMaps(Map.empty, Map.empty, Map.empty)) {
      case (maps, (round, index)) =>
        val id = wbId(round, index)
        val (feederA, feederB) =
          if (round == 1) {
            val slotA = seedOrder((index - 1) * 2)
            val slotB = seedOrder((index - 1) * 2 + 1)
            (Feeder.Seed(slotA), Feeder.Seed(slotB))
          } else {
            (
              Feeder.WinnerOf(wbId(round - 1, index * 2 - 1)),
              Feeder.WinnerOf(wbId(round - 1, index * 2))
            )
          }

        val winnerTo =
          if (round < winnersRounds) {
            val nextId = wbId(round + 1, (index + 1) / 2)
            val nextSlot = if (index % 2 == 1) Slot.A else Slot.B
            maps.winnerTo.updated(id, (nextId, nextSlot))
          } else {
            maps.winnerTo.updated(id, (gfId, Slot.A))
          }

        BracketMaps(
          matches = maps.matches.updated(id, MatchDef(id, feederA, feederB)),
          winnerTo = winnerTo,
          loserTo =
            maps.loserTo.updated(id, losersDropTarget(winnersRounds, round, index))
        )
    }
  }

  private def losersDropTarget(
      winnersRounds: Int,
      round: Int,
      index: Int
  ): (String, Slot) = {
    if (round == 1) {
      val lbMatch = (index + 1) / 2
      val slot = if (index % 2 == 1) Slot.A else Slot.B
      (lbId(1, lbMatch), slot)
    } else if (round < winnersRounds) {
      (lbId(round * 2 - 2, index), Slot.A)
    } else {
      (lbId((winnersRounds - 1) * 2, 1), Slot.B)
    }
  }

  private def buildLosersBracket(
      size: Int,
      winnersRounds: Int
  ): BracketMaps = {
    val losersRounds = (winnersRounds - 1) * 2
    val slots =
      for {
        round <- 1 to losersRounds
        index <- 1 to losersMatchCount(size, round, losersRounds)
      } yield (round, index)

    slots.foldLeft(BracketMaps(Map.empty, Map.empty, Map.empty)) {
      case (maps, (round, index)) =>
        val id = lbId(round, index)
        val (feederA, feederB) =
          if (round == 1) {
            (Feeder.Empty, Feeder.Empty)
          } else if (round % 2 == 0) {
            (
              Feeder.LoserOf(wbId(round / 2 + 1, index)),
              Feeder.WinnerOf(lbId(round - 1, index))
            )
          } else {
            (
              Feeder.WinnerOf(lbId(round - 1, index * 2 - 1)),
              Feeder.WinnerOf(lbId(round - 1, index * 2))
            )
          }

        val winnerTo =
          if (round < losersRounds) {
            val (nextRound, nextIndex, slot) =
              lbWinnerDestination(round, index, losersRounds)
            maps.winnerTo.updated(id, (lbId(nextRound, nextIndex), slot))
          } else {
            maps.winnerTo.updated(id, (gfId, Slot.B))
          }

        BracketMaps(
          matches = maps.matches.updated(id, MatchDef(id, feederA, feederB)),
          winnerTo = winnerTo,
          loserTo = maps.loserTo
        )
    }
  }

  private def lbWinnerDestination(
      round: Int,
      index: Int,
      losersRounds: Int
  ): (Int, Int, Slot) = {
    if (round % 2 == 1) {
      if (round == losersRounds - 1) {
        (round + 1, 1, Slot.A)
      } else {
        (round + 1, index, Slot.B)
      }
    } else {
      val nextIndex = (index + 1) / 2
      val slot = if (index % 2 == 1) Slot.A else Slot.B
      (round + 1, nextIndex, slot)
    }
  }

  private def buildGrandFinal(winnersRounds: Int): (String, MatchDef) = {
    val matchDef = MatchDef(
      id = gfId,
      feederA = Feeder.WinnerOf(wbId(winnersRounds, 1)),
      feederB = Feeder.WinnerOf(lbId((winnersRounds - 1) * 2, 1))
    )
    gfId -> matchDef
  }

  private def losersMatchCount(size: Int, round: Int, totalRounds: Int): Int =
    if (round == totalRounds) {
      1
    } else {
      size >> ((round + 1) / 2 + 1)
    }

  private def wbId(round: Int, index: Int): String = s"wb-$round-$index"

  private def lbId(round: Int, index: Int): String = s"lb-$round-$index"

  private val gfId: String = "gf-1"

  private def isPowerOfTwo(n: Int): Boolean =
    n > 0 && (n & (n - 1)) == 0

  private def log2(n: Int): Int =
    (math.log(n) / math.log(2)).toInt
}
