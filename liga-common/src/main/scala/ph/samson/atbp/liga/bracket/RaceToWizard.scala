package ph.samson.atbp.liga.bracket

/** Client-side cascade rules for the director race-to wizard. */
object RaceToWizard {

  val DefaultRaceTo: Int = 7

  final case class State(
      raceToByScope: Map[String, Int],
      gfPinned: Boolean
  )

  def initialState(playerCount: Int): State = {
    val keys = RaceToScopes.requiredKeys(playerCount)
    val wbKeys = winnersKeys(keys)
    val lbKeys = losersKeys(keys)
    val wbN = wbKeys.last

    val base = keys.map(_ -> DefaultRaceTo).toMap
    val withLosers = cascadeSection(base, lbKeys, fromRound = 1, base("wb-1"))
    val withGf = withLosers.updated("gf", withLosers(wbN))

    State(withGf, gfPinned = false)
  }

  def loadState(serverMap: Map[String, Int], playerCount: Int): State = {
    val keys = RaceToScopes.requiredKeys(playerCount)
    val wbN = winnersKeys(keys).last
    val gfPinned = serverMap.get("gf") != serverMap.get(wbN)
    State(serverMap, gfPinned)
  }

  def applyEdit(
      state: State,
      scope: String,
      raceTo: Int,
      playerCount: Int
  ): State = {
    val keys = RaceToScopes.requiredKeys(playerCount)
    val wbKeys = winnersKeys(keys)
    val lbKeys = losersKeys(keys)
    val wbN = wbKeys.last

    scope match {
      case "wb-1" =>
        val withWinners = cascadeSection(state.raceToByScope, wbKeys, 1, raceTo)
        val withLosers = cascadeSection(withWinners, lbKeys, 1, raceTo)
        val withGf =
          if (state.gfPinned) {
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
          if (state.gfPinned) {
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

      case "gf" =>
        state.copy(
          raceToByScope = state.raceToByScope.updated("gf", raceTo),
          gfPinned = true
        )

      case _ =>
        state.copy(raceToByScope = state.raceToByScope.updated(scope, raceTo))
    }
  }

  private def winnersKeys(keys: List[String]): List[String] =
    keys.filter(_.startsWith("wb-"))

  private def losersKeys(keys: List[String]): List[String] =
    keys.filter(_.startsWith("lb-"))

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
      case _            => 0
    }
}
