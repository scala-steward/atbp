package ph.samson.atbp.liga.bracket

/** Client-side cascade rules for the director race-to wizard. */
object RaceToWizard {

  val DefaultRaceTo: Int = 7

  final case class State(
      raceToByScope: Map[String, Int],
      gfPinned: Boolean,
      topN: Int
  )

  def initialState(playerCount: Int): State =
    initialState(playerCount, TopN.defaultTopN(playerCount))

  def initialState(playerCount: Int, topN: Int): State = {
    val keys = RaceToScopes.requiredKeys(playerCount, topN)
    State(buildDefaultMap(keys), gfPinned = false, topN)
  }

  def loadState(
      serverMap: Map[String, Int],
      playerCount: Int,
      topN: Int
  ): State = {
    val keys = RaceToScopes.requiredKeys(playerCount, topN)
    val scrubbed = keys.flatMap(key => serverMap.get(key).map(key -> _)).toMap
    val merged = mergeNewScopes(
      newKeys = keys,
      kept = scrubbed,
      defaultGfFromWbN = false
    )
    val wbN = winnersKeys(keys).lastOption
    val gfPinned =
      keys.contains("gf") && wbN.exists(n => merged.get("gf") != merged.get(n))
    State(merged, gfPinned, topN)
  }

  def changeTopN(state: State, newTopN: Int, playerCount: Int): State = {
    val oldKeys = RaceToScopes.requiredKeys(playerCount, state.topN).toSet
    val newKeys = RaceToScopes.requiredKeys(playerCount, newTopN)
    val kept =
      newKeys.flatMap(key => state.raceToByScope.get(key).map(key -> _)).toMap
    val merged = mergeNewScopes(
      newKeys = newKeys,
      kept = kept,
      defaultGfFromWbN =
        newTopN <= 2 && newKeys.contains("gf") && !oldKeys.contains("gf")
    )
    val gfPinned =
      if (newTopN <= 2 && newKeys.contains("gf") && oldKeys.contains("gf")) {
        state.gfPinned
      } else {
        false
      }
    State(merged, gfPinned, newTopN)
  }

  def applyEdit(
      state: State,
      scope: String,
      raceTo: Int,
      playerCount: Int
  ): State = {
    val keys = RaceToScopes.requiredKeys(playerCount, state.topN)
    val wbKeys = winnersKeys(keys)
    val lbKeys = losersKeys(keys)
    val seKeys = singleElimKeys(keys)
    val wbN = wbKeys.last

    scope match {
      case "wb-1" =>
        val withWinners = cascadeSection(state.raceToByScope, wbKeys, 1, raceTo)
        val withLosers = cascadeSection(withWinners, lbKeys, 1, raceTo)
        val withGf =
          if (state.gfPinned || !keys.contains("gf")) {
            withLosers
          } else {
            withLosers.updated("gf", withLosers(wbN))
          }
        state.copy(raceToByScope = withGf)

      case s"wb-$round" =>
        val fromRound = round.toIntOption.getOrElse(1)
        val withWinners =
          cascadeSection(state.raceToByScope, wbKeys, fromRound, raceTo)
        val withGf =
          if (state.gfPinned || !keys.contains("gf")) {
            withWinners
          } else {
            withWinners.updated("gf", withWinners(wbN))
          }
        state.copy(raceToByScope = withGf)

      case s"lb-$round" =>
        val fromRound = round.toIntOption.getOrElse(1)
        val withLosers =
          cascadeSection(state.raceToByScope, lbKeys, fromRound, raceTo)
        state.copy(raceToByScope = withLosers)

      case s"se-$round" =>
        val fromRound = round.toIntOption.getOrElse(1)
        val withSe =
          cascadeSection(state.raceToByScope, seKeys, fromRound, raceTo)
        state.copy(raceToByScope = withSe)

      case "gf" =>
        state.copy(
          raceToByScope = state.raceToByScope.updated("gf", raceTo),
          gfPinned = true
        )

      case _ =>
        state.copy(raceToByScope = state.raceToByScope.updated(scope, raceTo))
    }
  }

  private def buildDefaultMap(keys: List[String]): Map[String, Int] = {
    val wbKeys = winnersKeys(keys)
    val lbKeys = losersKeys(keys)
    val seKeys = singleElimKeys(keys)
    val base = keys.map(_ -> DefaultRaceTo).toMap

    val withLosers =
      if (lbKeys.nonEmpty && wbKeys.nonEmpty) {
        cascadeSection(base, lbKeys, fromRound = 1, base(wbKeys.head))
      } else {
        base
      }

    val withSe =
      if (seKeys.nonEmpty) {
        cascadeSection(
          withLosers,
          seKeys,
          fromRound = 1,
          withLosers.getOrElse(seKeys.head, DefaultRaceTo)
        )
      } else {
        withLosers
      }

    wbKeys.lastOption.filter(_ => keys.contains("gf")) match {
      case Some(wbN) => withSe.updated("gf", withSe(wbN))
      case None      => withSe
    }
  }

  private def mergeNewScopes(
      newKeys: List[String],
      kept: Map[String, Int],
      defaultGfFromWbN: Boolean
  ): Map[String, Int] = {
    val added = newKeys.filterNot(kept.contains)
    val withDefaults = added.foldLeft(kept) { (current, key) =>
      current.updated(key, DefaultRaceTo)
    }

    val wb1 = withDefaults.get("wb-1")
    val withLosers = wb1.fold(withDefaults) { value =>
      cascadeSection(
        withDefaults,
        losersKeys(added).filterNot(kept.contains),
        fromRound = 1,
        value
      )
    }

    val se1 = withLosers.get("se-1")
    val withSe = se1.fold(withLosers) { value =>
      cascadeSection(
        withLosers,
        singleElimKeys(added).filterNot(kept.contains),
        fromRound = 1,
        value
      )
    }

    val ordered = newKeys.flatMap(key => withSe.get(key).map(key -> _)).toMap
    if (defaultGfFromWbN) {
      winnersKeys(newKeys).lastOption.fold(ordered) { wbN =>
        ordered.updated("gf", ordered(wbN))
      }
    } else {
      ordered
    }
  }

  private def winnersKeys(keys: List[String]): List[String] =
    keys.filter(_.startsWith("wb-"))

  private def losersKeys(keys: List[String]): List[String] =
    keys.filter(_.startsWith("lb-"))

  private def singleElimKeys(keys: List[String]): List[String] =
    keys.filter(_.startsWith("se-"))

  private def cascadeSection(
      map: Map[String, Int],
      keys: List[String],
      fromRound: Int,
      value: Int
  ): Map[String, Int] =
    keys.foldLeft(map) { (current, key) =>
      val round = sectionRound(key)
      if (round >= fromRound) {
        current.updated(key, value)
      } else {
        current
      }
    }

  private def sectionRound(scope: String): Int =
    scope match {
      case s"wb-$round" => round.toIntOption.getOrElse(0)
      case s"lb-$round" => round.toIntOption.getOrElse(0)
      case s"se-$round" => round.toIntOption.getOrElse(0)
      case _            => 0
    }
}
