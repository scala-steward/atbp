package ph.samson.atbp.liga.bracket

import ph.samson.atbp.liga.model.*

/** Format-specific steps after core place-and-bye advancement (reset GF, cut
  * reseed).
  */
private[bracket] object FormatAdvancement {

  def afterCore(
      bracket: Bracket,
      matchId: String,
      winner: Player,
      reseedContext: Option[CutReseed.Context]
  ): Either[String, (Bracket, List[String])] = {
    val (withReset, resetReady) =
      maybeCreateResetGrandFinal(bracket, bracket.topN, matchId, winner)
    maybeReseedAfterCut(withReset, reseedContext).map {
      case (reseeded, cutReady) =>
        (reseeded, resetReady.toList ++ cutReady)
    }
  }

  private def maybeReseedAfterCut(
      bracket: Bracket,
      reseedContext: Option[CutReseed.Context]
  ): Either[String, (Bracket, List[String])] =
    reseedContext match {
      case None          => Right((bracket, Nil))
      case Some(context) =>
        val beforeCut = CutReseed.cutAlreadyDone(bracket)
        CutReseed.reseed(bracket, context).map { reseeded =>
          val afterCut = CutReseed.cutAlreadyDone(reseeded)
          val newlyReady =
            if (afterCut && !beforeCut) {
              reseeded.matches
                .filter(m =>
                  m.id.startsWith("se-1-") && m.state == BracketMatchState.Ready
                )
                .map(_.id)
            } else {
              Nil
            }
          (reseeded, newlyReady)
        }
    }

  private def maybeCreateResetGrandFinal(
      bracket: Bracket,
      topN: Int,
      matchId: String,
      winner: Player
  ): (Bracket, Option[String]) =
    if (topN != 1 || matchId != resetGrandFinalTriggerId) {
      (bracket, None)
    } else {
      bracket.matches.find(_.id == resetGrandFinalTriggerId) match {
        case None      => (bracket, None)
        case Some(gf1) =>
          val lbChamp = gf1.playerB
          if (lbChamp.contains(winner)) {
            val wbChamp = gf1.playerA.get
            val gf2 = BracketMatch(
              id = resetGrandFinalId,
              playerA = Some(wbChamp),
              playerB = lbChamp,
              state = BracketMatchState.Ready
            )
            (
              bracket.copy(matches = bracket.matches :+ gf2),
              Some(resetGrandFinalId)
            )
          } else {
            (bracket, None)
          }
      }
    }

  private val resetGrandFinalTriggerId: String = "gf-1"
  private val resetGrandFinalId: String = "gf-2"
}
