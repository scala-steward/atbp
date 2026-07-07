package ph.samson.atbp.liga.js.glicko

import dimos.glicko2.Tuning as GlickoTuning

/** Repo-wide Glicko2 tuning (1500 / 350 / 0.06 / τ 0.5). */
object Tuning {
  val Default: GlickoTuning = GlickoTuning.default()
}
