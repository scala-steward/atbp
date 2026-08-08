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
      topN: Int,
      matches: Map[String, MatchDef],
      winnerTo: Map[String, (String, Slot)],
      loserTo: Map[String, (String, Slot)]
  )

  private final case class BracketMaps(
      matches: Map[String, MatchDef],
      winnerTo: Map[String, (String, Slot)],
      loserTo: Map[String, (String, Slot)]
  )

  def apply(size: Int): Topology =
    apply(size, topN = 2)

  def apply(size: Int, topN: Int): Topology = {
    val shape = BracketFormat.forBracket(size, topN)
    shape.kind match {
      case BracketFormat.Kind.FullSingleElimination =>
        buildFullSingleElimination(shape)
      case BracketFormat.Kind.CutDoubleElimination =>
        buildCutDoubleElimination(shape)
      case BracketFormat.Kind.ClassicDoubleElimination =>
        val wb = buildWinnersBracket(size, shape.winnersRounds)
        val lb = buildLosersBracket(size, shape.winnersRounds)
        val gfMatch = buildGrandFinal(shape.winnersRounds)

        Topology(
          size = size,
          topN = topN,
          matches = wb.matches ++ lb.matches + gfMatch,
          winnerTo = wb.winnerTo ++ lb.winnerTo,
          loserTo = wb.loserTo
        )
    }
  }

  private def buildCutDoubleElimination(
      shape: BracketFormat.Shape
  ): Topology = {
    val wb =
      buildWinnersBracketForCut(
        shape.bracketSize,
        shape.winnersRounds,
        shape.losersRounds
      )
    val lb =
      buildTruncatedLosersBracket(shape.bracketSize, shape.losersRounds)
    val se = buildSeSkeleton(shape.seRounds)

    Topology(
      size = shape.bracketSize,
      topN = shape.topN,
      matches = wb.matches ++ lb.matches ++ se.matches,
      winnerTo = wb.winnerTo ++ lb.winnerTo ++ se.winnerTo,
      loserTo = wb.loserTo
    )
  }

  private def buildFullSingleElimination(
      shape: BracketFormat.Shape
  ): Topology = {
    val se = buildSingleElimBracket(shape.bracketSize, shape.seRounds)
    Topology(
      size = shape.bracketSize,
      topN = shape.topN,
      matches = se.matches,
      winnerTo = se.winnerTo,
      loserTo = Map.empty
    )
  }

  private def buildSingleElimBracket(
      size: Int,
      rounds: Int
  ): BracketMaps = {
    val seedOrder = Seeding.bracketSlotSeeds(size)
    val slots =
      for {
        round <- 1 to rounds
        index <- 1 to (size >> round)
      } yield (round, index)

    slots.foldLeft(BracketMaps(Map.empty, Map.empty, Map.empty)) {
      case (maps, (round, index)) =>
        val id = seId(round, index)
        val (feederA, feederB) =
          if (round == 1) {
            val slotA = seedOrder((index - 1) * 2)
            val slotB = seedOrder((index - 1) * 2 + 1)
            (Feeder.Seed(slotA), Feeder.Seed(slotB))
          } else {
            (
              Feeder.WinnerOf(seId(round - 1, index * 2 - 1)),
              Feeder.WinnerOf(seId(round - 1, index * 2))
            )
          }

        val winnerTo =
          if (round < rounds) {
            val nextId = seId(round + 1, (index + 1) / 2)
            val nextSlot = if (index % 2 == 1) Slot.A else Slot.B
            maps.winnerTo.updated(id, (nextId, nextSlot))
          } else {
            maps.winnerTo
          }

        BracketMaps(
          matches = maps.matches.updated(id, MatchDef(id, feederA, feederB)),
          winnerTo = winnerTo,
          loserTo = maps.loserTo
        )
    }
  }

  private def buildSeSkeleton(rounds: Int): BracketMaps = {
    val slots =
      for {
        round <- 1 to rounds
        index <- 1 to (1 << (rounds - round))
      } yield (round, index)

    slots.foldLeft(BracketMaps(Map.empty, Map.empty, Map.empty)) {
      case (maps, (round, index)) =>
        val id = seId(round, index)
        val (feederA, feederB) =
          if (round == 1) {
            (Feeder.Empty, Feeder.Empty)
          } else {
            (
              Feeder.WinnerOf(seId(round - 1, index * 2 - 1)),
              Feeder.WinnerOf(seId(round - 1, index * 2))
            )
          }

        val winnerTo =
          if (round < rounds) {
            val nextId = seId(round + 1, (index + 1) / 2)
            val nextSlot = if (index % 2 == 1) Slot.A else Slot.B
            maps.winnerTo.updated(id, (nextId, nextSlot))
          } else {
            maps.winnerTo
          }

        BracketMaps(
          matches = maps.matches.updated(id, MatchDef(id, feederA, feederB)),
          winnerTo = winnerTo,
          loserTo = maps.loserTo
        )
    }
  }

  private def buildWinnersBracket(
      size: Int,
      winnersRounds: Int
  ): BracketMaps =
    buildWinnersBracketImpl(size, winnersRounds, cutLbRounds = None)

  private def buildWinnersBracketForCut(
      size: Int,
      winnersRounds: Int,
      cutLbRounds: Int
  ): BracketMaps =
    buildWinnersBracketImpl(
      size,
      winnersRounds,
      cutLbRounds = Some(cutLbRounds)
    )

  private def buildWinnersBracketImpl(
      size: Int,
      winnersRounds: Int,
      cutLbRounds: Option[Int]
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
          } else if (cutLbRounds.isEmpty) {
            maps.winnerTo.updated(id, (gfId, Slot.A))
          } else {
            maps.winnerTo
          }

        BracketMaps(
          matches = maps.matches.updated(id, MatchDef(id, feederA, feederB)),
          winnerTo = winnerTo,
          loserTo = maps.loserTo
            .updated(
              id,
              losersDropTarget(winnersRounds, round, index, cutLbRounds)
            )
        )
    }
  }

  private def losersDropTarget(
      winnersRounds: Int,
      round: Int,
      index: Int,
      cutLbRounds: Option[Int]
  ): (String, Slot) = {
    if (round == 1) {
      val lbMatch = (index + 1) / 2
      val slot = if (index % 2 == 1) Slot.A else Slot.B
      (lbId(1, lbMatch), slot)
    } else if (round < winnersRounds || cutLbRounds.isDefined) {
      // Cut finals have topN/2 matches — same even-LB drop as mid rounds.
      // Classic WB final (one match) still uses Slot.B of the LB final below.
      (lbId(round * 2 - 2, index), Slot.A)
    } else {
      val lbRound = (winnersRounds - 1) * 2
      (lbId(lbRound, 1), Slot.B)
    }
  }

  private def buildLosersBracket(
      size: Int,
      winnersRounds: Int
  ): BracketMaps =
    buildLosersBracketImpl(
      size,
      (winnersRounds - 1) * 2,
      feedGrandFinal = true,
      singleMatchFinal = true
    )

  private def buildTruncatedLosersBracket(
      size: Int,
      lbRounds: Int
  ): BracketMaps =
    buildLosersBracketImpl(
      size,
      lbRounds,
      feedGrandFinal = false,
      singleMatchFinal = false
    )

  private def buildLosersBracketImpl(
      size: Int,
      losersRounds: Int,
      feedGrandFinal: Boolean,
      singleMatchFinal: Boolean
  ): BracketMaps = {
    val slots =
      for {
        round <- 1 to losersRounds
        index <- 1 to losersMatchCount(
          size,
          round,
          losersRounds,
          singleMatchFinal
        )
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
              lbWinnerDestination(round, index, losersRounds, singleMatchFinal)
            maps.winnerTo.updated(id, (lbId(nextRound, nextIndex), slot))
          } else if (!feedGrandFinal) {
            maps.winnerTo
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
      losersRounds: Int,
      singleMatchFinal: Boolean
  ): (Int, Int, Slot) = {
    if (round % 2 == 1) {
      if (singleMatchFinal && round == losersRounds - 1) {
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

  private def losersMatchCount(
      size: Int,
      round: Int,
      totalRounds: Int,
      singleMatchFinal: Boolean
  ): Int =
    BracketFormat.losersMatchCount(size, round, totalRounds, singleMatchFinal)

  private def wbId(round: Int, index: Int): String = s"wb-$round-$index"

  private def seId(round: Int, index: Int): String = s"se-$round-$index"

  private def lbId(round: Int, index: Int): String = s"lb-$round-$index"

  private val gfId: String = "gf-1"
}
