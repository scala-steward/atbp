package ph.samson.atbp.liga.js.audience

import ph.samson.atbp.liga.js.api.Models.BracketMatch
import ph.samson.atbp.liga.js.api.Models.BracketMatchState
import ph.samson.atbp.liga.js.director.BracketLayout
import ph.samson.atbp.liga.js.director.BracketLayout.Section

/** Pure spatial column/band layout for the audience chalkboard bracket. */
object AudienceSpatialLayout {

  final case class SpatialColumn(
      round: Int,
      matches: List[BracketMatch]
  )

  final case class SpatialBand(
      section: Section,
      columns: List[SpatialColumn]
  )

  private final case class PreparedColumn(
      section: Section,
      round: Int,
      allMatches: List[BracketMatch],
      shownMatches: List[BracketMatch]
  )

  private def roundFullyCompleted(matches: List[BracketMatch]): Boolean =
    matches.forall(_.state == BracketMatchState.Completed)

  private def spatialBandOrder(section: Section): Int =
    section match {
      case Section.GrandFinal        => 0
      case Section.SingleElimination => 1
      case Section.Winners           => 2
      case Section.Losers            => 3
    }

  /** Column sort: unfinished left, completed right; higher rounds left within
    * each completion bucket.
    */
  private def columnTimelineKey(
      round: Int,
      allMatches: List[BracketMatch]
  ): (Int, Int) =
    (
      if (roundFullyCompleted(allMatches)) 1 else 0,
      -round
    )

  /** Bands stacked top→bottom (Winners/GF/SE above Losers); columns left→right
    * on a shared current-left timeline.
    */
  def layout(
      matches: List[BracketMatch],
      bracketSize: Int
  ): List[SpatialBand] =
    matches
      .groupBy(m =>
        (
          BracketLayout.sectionOf(m.id),
          BracketLayout.roundOf(m.id, bracketSize)
        )
      )
      .toList
      .map { case ((section, round), grouped) =>
        PreparedColumn(
          section = section,
          round = round,
          allMatches = grouped,
          shownMatches = grouped
            .filter(BracketLayout.showForAudience)
            .sortBy(m => BracketLayout.matchSeedIndex(m.id))
        )
      }
      .filter(_.shownMatches.nonEmpty)
      .groupBy(_.section)
      .toList
      .sortBy { case (section, _) => spatialBandOrder(section) }
      .map { case (section, prepared) =>
        SpatialBand(
          section = section,
          columns = prepared
            .sortBy(p => columnTimelineKey(p.round, p.allMatches))
            .map(p => SpatialColumn(p.round, p.shownMatches))
        )
      }
}
