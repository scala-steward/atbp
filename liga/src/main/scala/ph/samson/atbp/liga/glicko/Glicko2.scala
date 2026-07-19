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

  /** Apply one Glicko2 rating period from a period-start snapshot.
    *
    * Within a period, the order of `period.matches` is presentation-only.
    * Shuffling match rows must produce identical ratings, RD, volatility, and
    * W–L for every player.
    *
    * @param priorSnapshot
    *   cumulative state after all earlier period files
    * @param period
    *   one `.liga` file's matches and metadata
    */
  def updateAfterPeriod(priorSnapshot: Snapshot, period: Period): Snapshot = {
    require(
      period.matches.nonEmpty,
      s"Period '${period.name}' has zero matches; a period must contain at least one match"
    )

    val frozen = priorSnapshot
    val participants =
      period.matches.flatMap(m => List(m.playerA, m.playerB)).toSet

    val (resultsByPlayer, winsByPlayer, lossesByPlayer) =
      period.matches.foldLeft(
        (
          Map.empty[Player, List[Result]],
          Map.empty[Player, Int],
          Map.empty[Player, Int]
        )
      ) { case ((results, wins, losses), periodMatch) =>
        val aFrozen = getOrNew(frozen, periodMatch.playerA)
        val bFrozen = getOrNew(frozen, periodMatch.playerB)
        val aGlicko = aFrozen.toGlickoPlayer
        val bGlicko = bFrozen.toGlickoPlayer

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

        def appendResults(
            current: Map[Player, List[Result]],
            player: Player,
            newResults: List[Result]
        ): Map[Player, List[Result]] =
          current.updatedWith(player) {
            case Some(existing) => Some(existing ++ newResults)
            case None           => Some(newResults)
          }

        val updatedResults =
          appendResults(
            appendResults(results, periodMatch.playerA, aResults),
            periodMatch.playerB,
            bResults
          )

        (
          updatedResults,
          wins
            .updated(
              periodMatch.playerA,
              wins.getOrElse(periodMatch.playerA, 0) + periodMatch.scoreA
            )
            .updated(
              periodMatch.playerB,
              wins.getOrElse(periodMatch.playerB, 0) + periodMatch.scoreB
            ),
          losses
            .updated(
              periodMatch.playerA,
              losses.getOrElse(periodMatch.playerA, 0) + periodMatch.scoreB
            )
            .updated(
              periodMatch.playerB,
              losses.getOrElse(periodMatch.playerB, 0) + periodMatch.scoreA
            )
        )
      }

    val priorUpdated = priorSnapshot.map { case (player, internal) =>
      if (participants.contains(player)) {
        val results = resultsByPlayer.getOrElse(player, Nil)
        val updated = internal.toGlickoPlayer.afterPeriod(results, tuning)
        player -> InternalRating(
          player,
          updated.rating,
          updated.deviation,
          updated.volatility,
          internal.wins + winsByPlayer.getOrElse(player, 0),
          internal.losses + lossesByPlayer.getOrElse(player, 0)
        )
      } else {
        val updated = internal.toGlickoPlayer.afterPeriod(Nil, tuning)
        player -> InternalRating(
          player,
          updated.rating,
          updated.deviation,
          updated.volatility,
          internal.wins,
          internal.losses
        )
      }
    }

    val debuts = participants.diff(priorSnapshot.keySet).map { player =>
      val results = resultsByPlayer.getOrElse(player, Nil)
      val internal = newInternal(player)
      val updated = internal.toGlickoPlayer.afterPeriod(results, tuning)
      player -> InternalRating(
        player,
        updated.rating,
        updated.deviation,
        updated.volatility,
        winsByPlayer.getOrElse(player, 0),
        lossesByPlayer.getOrElse(player, 0)
      )
    }

    priorUpdated ++ debuts
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
