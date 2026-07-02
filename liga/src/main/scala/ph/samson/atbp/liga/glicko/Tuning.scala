package ph.samson.atbp.liga.glicko

import dimos.glicko2.Tuning as GlickoTuning

/** Repo-wide Glicko2 tuning (1500 / 350 / 0.06 / τ 0.5). */
object Tuning {

  val Default: GlickoTuning = GlickoTuning.default(
    initRating = 1500,
    maxDeviation = 350,
    minDeviation = 0,
    initVolatility = 0.06,
    tau = 0.5,
    tolerance = 0.000001
  )
}
