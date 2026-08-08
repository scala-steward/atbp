package ph.samson.atbp.liga.bracket

import ph.samson.atbp.liga.model.*

/** Earned-rack cut reseed for Top \(4+\) double elimination → single
  * elimination.
  */
object CutReseed {

  final case class Context(
      players: List[Player],
      frozenRatings: Map[Player, PlayerRating]
  )

  def isCutFormat(bracket: Bracket): Boolean =
    bracket.topN >= 4 && bracket.topN < bracket.size

  def cutAlreadyDone(bracket: Bracket): Boolean =
    bracket.matches.exists(m => m.id.startsWith("se-1-") && m.playerA.nonEmpty)

  def survivors(bracket: Bracket, players: List[Player]): List[Player] = {
    val eliminated = eliminatedPlayers(bracket)
    players.filterNot(eliminated.contains)
  }

  def eliminatedPlayers(bracket: Bracket): Set[Player] =
    bracket.matches.flatMap(eliminatedFromMatch).toSet

  private def eliminatedFromMatch(matchDef: BracketMatch): Option[Player] =
    if (
      !matchDef.id.startsWith("lb-") ||
      matchDef.state != BracketMatchState.Completed ||
      matchDef.isBye ||
      matchDef.playerA.isEmpty ||
      matchDef.playerB.isEmpty
    ) {
      None
    } else {
      matchWinner(matchDef).flatMap { winner =>
        if (matchDef.playerA.contains(winner)) {
          matchDef.playerB
        } else {
          matchDef.playerA
        }
      }
    }

  def playedMatches(
      bracket: Bracket,
      frozenRatings: Map[Player, PlayerRating]
  ): List[EarnedRacks.PlayedMatch] =
    bracket.matches.flatMap(toPlayedMatch(_, frozenRatings))

  def reseed(
      bracket: Bracket,
      context: Context
  ): Either[String, Bracket] = {
    if (!isCutFormat(bracket) || cutAlreadyDone(bracket)) {
      Right(bracket)
    } else {
      val alive = survivors(bracket, context.players)
      if (alive.size != bracket.topN) {
        Right(bracket)
      } else {
        val profiles = survivorProfiles(context)
        EarnedRacks
          .rankSurvivors(
            alive,
            playedMatches(bracket, context.frozenRatings),
            profiles
          )
          .map { ranked =>
            val seated = EarnedRacks.assignSingleElimSlots(ranked)
            applySeating(bracket, seated)
          }
      }
    }
  }

  private def survivorProfiles(
      context: Context
  ): Map[Player, EarnedRacks.SurvivorProfile] = {
    val drawOrder = context.players.zipWithIndex.map { case (player, index) =>
      player -> (index + 1)
    }.toMap
    context.frozenRatings.map { case (player, rating) =>
      player -> EarnedRacks.SurvivorProfile(
        player = player,
        ratingSeed = rating.rating,
        drawOrder = drawOrder.getOrElse(player, Int.MaxValue)
      )
    }
  }

  private def applySeating(bracket: Bracket, seated: List[Player]): Bracket = {
    val abandoned = abandonUnfinishedDe(bracket)
    val seRound1 =
      abandoned.matches.filter(_.id.startsWith("se-1-")).sortBy(_.id)
    val paired =
      seated
        .grouped(2)
        .zip(seRound1)
        .map { case (pair, matchDef) =>
          val playerA = pair.headOption
          val playerB = pair.lift(1)
          val updated = matchDef.copy(
            playerA = playerA,
            playerB = playerB,
            state =
              if (playerA.nonEmpty && playerB.nonEmpty) {
                BracketMatchState.Ready
              } else {
                BracketMatchState.Pending
              }
          )
          matchDef.id -> updated
        }
        .toMap
    abandoned.copy(
      matches = abandoned.matches.map { matchDef =>
        paired.getOrElse(matchDef.id, matchDef)
      }
    )
  }

  /** Leave completed DE history intact; mark unfinished DE matches completed
    * byes so period emission and UI do not treat them as still live.
    */
  private def abandonUnfinishedDe(bracket: Bracket): Bracket =
    bracket.copy(matches = bracket.matches.map { matchDef =>
      if (matchDef.id.startsWith("se-")) {
        matchDef
      } else if (matchDef.state == BracketMatchState.Completed) {
        matchDef
      } else {
        matchDef.copy(
          playerA = None,
          playerB = None,
          state = BracketMatchState.Completed,
          result = None,
          forfeit = None,
          isBye = true
        )
      }
    })

  private def toPlayedMatch(
      matchDef: BracketMatch,
      frozenRatings: Map[Player, PlayerRating]
  ): Option[EarnedRacks.PlayedMatch] =
    if (matchDef.state != BracketMatchState.Completed) {
      None
    } else if (matchDef.isBye) {
      playedKind(
        matchDef,
        frozenRatings,
        EarnedRacks.MatchKind.Bye
      )
    } else if (matchDef.forfeit.nonEmpty) {
      playedKind(
        matchDef,
        frozenRatings,
        EarnedRacks.MatchKind.Forfeit
      )
    } else {
      matchDef.result.flatMap { result =>
        playedKind(
          matchDef.copy(result = Some(result)),
          frozenRatings,
          EarnedRacks.MatchKind.Played
        )
      }
    }

  private def playedKind(
      matchDef: BracketMatch,
      frozenRatings: Map[Player, PlayerRating],
      kind: EarnedRacks.MatchKind
  ): Option[EarnedRacks.PlayedMatch] =
    for {
      playerA <- matchDef.playerA
      playerB <- matchDef.playerB
      ratingA <- frozenRatings.get(playerA)
      ratingB <- frozenRatings.get(playerB)
    } yield {
      val weaker = weakerPlayer(ratingA, ratingB)
      val (scoreA, scoreB) =
        matchDef.result match {
          case Some(result) => (result.scoreA, result.scoreB)
          case None         => (0, 0)
        }
      EarnedRacks.PlayedMatch(
        playerA = playerA,
        playerB = playerB,
        scoreA = scoreA,
        scoreB = scoreB,
        handicapApplied = matchDef.handicapApplied.getOrElse(0),
        weaker = Some(weaker),
        kind = kind
      )
    }

  private def weakerPlayer(a: PlayerRating, b: PlayerRating): Player =
    if (a.rating != b.rating) {
      if (a.rating < b.rating) a.player else b.player
    } else {
      List(a, b).minBy(_.player.name).player
    }

  private def matchWinner(matchDef: BracketMatch): Option[Player] =
    matchDef.forfeit
      .flatMap { info =>
        MatchSide.parse(info.forfeitingSide).flatMap { side =>
          val winnerSide = MatchSide.winnerFromForfeiting(side)
          MatchSide.select(winnerSide, matchDef.playerA, matchDef.playerB)
        }
      }
      .orElse {
        matchDef.result.flatMap { result =>
          if (result.scoreA > result.scoreB) {
            matchDef.playerA
          } else if (result.scoreB > result.scoreA) {
            matchDef.playerB
          } else {
            None
          }
        }
      }
}
