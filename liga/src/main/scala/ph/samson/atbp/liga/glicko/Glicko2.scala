package ph.samson.atbp.liga.glicko

import dimos.glicko2.Player as GlickoPlayer
import dimos.glicko2.Result
import dimos.glicko2.Tuning as GlickoTuning
import ph.samson.atbp.liga.model.*

/** Internal Glicko2 state including volatility (not shown in CLI output). */
final case class InternalRating(
    player: Player,
    rating: Double,
    rd: Double,
    volatility: Double,
    wins: Int,
    losses: Int
) {

  def toPlayerRating: PlayerRating =
    PlayerRating(player, rating, rd, wins, losses)

  def toGlickoPlayer: GlickoPlayer =
    GlickoPlayer(rating, rd, volatility)
}

object Glicko2 {

  type Snapshot = Map[Player, InternalRating]

  val empty: Snapshot = Map.empty

  private val tuning: GlickoTuning = Tuning.Default

  def newPlayerRating(player: Player): PlayerRating =
    newInternal(player).toPlayerRating

  def snapshot(player: Player, rating: Double, rd: Double): Snapshot =
    snapshot(player, rating, rd, tuning.initVolatility)

  def snapshot(
      player: Player,
      rating: Double,
      rd: Double,
      volatility: Double
  ): Snapshot =
    Map(
      player -> InternalRating(
        player,
        rating,
        rd,
        volatility,
        wins = 0,
        losses = 0
      )
    )

  def updateAfterGame(
      state: Snapshot,
      playerA: Player,
      playerB: Player,
      winner: GameWinner
  ): Snapshot = {
    val a = getOrNew(state, playerA)
    val b = getOrNew(state, playerB)
    val aGlicko = a.toGlickoPlayer
    val bGlicko = b.toGlickoPlayer

    val (aUpdated, bUpdated, aWins, aLosses, bWins, bLosses) = winner match {
      case GameWinner.PlayerA =>
        (
          aGlicko.afterPeriod(Seq(Result.WonAgainst(bGlicko)), tuning),
          bGlicko.afterPeriod(Seq(Result.DefeatedBy(aGlicko)), tuning),
          a.wins + 1,
          a.losses,
          b.wins,
          b.losses + 1
        )
      case GameWinner.PlayerB =>
        (
          aGlicko.afterPeriod(Seq(Result.DefeatedBy(bGlicko)), tuning),
          bGlicko.afterPeriod(Seq(Result.WonAgainst(aGlicko)), tuning),
          a.wins,
          a.losses + 1,
          b.wins + 1,
          b.losses
        )
    }

    state
      .updated(
        playerA,
        InternalRating(
          playerA,
          aUpdated.rating,
          aUpdated.deviation,
          aUpdated.volatility,
          aWins,
          aLosses
        )
      )
      .updated(
        playerB,
        InternalRating(
          playerB,
          bUpdated.rating,
          bUpdated.deviation,
          bUpdated.volatility,
          bWins,
          bLosses
        )
      )
  }

  /** Apply all expanded games in one rating period (order-independent within
    * the match).
    */
  def updateAfterMatch(state: Snapshot, periodMatch: PeriodMatch): Snapshot = {
    val a = getOrNew(state, periodMatch.playerA)
    val b = getOrNew(state, periodMatch.playerB)
    val aGlicko = a.toGlickoPlayer
    val bGlicko = b.toGlickoPlayer

    val games =
      ScoreExpansion.expandGames(periodMatch.scoreA, periodMatch.scoreB)
    val aResults = games.map {
      case GameWinner.PlayerA => Result.WonAgainst(bGlicko)
      case GameWinner.PlayerB => Result.DefeatedBy(bGlicko)
    }
    val bResults = games.map {
      case GameWinner.PlayerA => Result.DefeatedBy(aGlicko)
      case GameWinner.PlayerB => Result.WonAgainst(aGlicko)
    }

    val aUpdated = aGlicko.afterPeriod(aResults, tuning)
    val bUpdated = bGlicko.afterPeriod(bResults, tuning)

    state
      .updated(
        periodMatch.playerA,
        InternalRating(
          periodMatch.playerA,
          aUpdated.rating,
          aUpdated.deviation,
          aUpdated.volatility,
          a.wins + periodMatch.scoreA,
          a.losses + periodMatch.scoreB
        )
      )
      .updated(
        periodMatch.playerB,
        InternalRating(
          periodMatch.playerB,
          bUpdated.rating,
          bUpdated.deviation,
          bUpdated.volatility,
          b.wins + periodMatch.scoreB,
          b.losses + periodMatch.scoreA
        )
      )
  }

  /** Deterministic player ordering (alphabetical by display name). */
  def leaderboard(state: Map[Player, PlayerRating]): List[PlayerRating] =
    state.values.toList.sortBy(_.player.name)

  def ratingOf(state: Snapshot, player: Player): InternalRating =
    state(player)

  private def newInternal(player: Player): InternalRating =
    InternalRating(
      player,
      tuning.initRating,
      tuning.maxDeviation,
      tuning.initVolatility,
      wins = 0,
      losses = 0
    )

  private def getOrNew(state: Snapshot, player: Player): InternalRating =
    state.getOrElse(player, newInternal(player))
}
