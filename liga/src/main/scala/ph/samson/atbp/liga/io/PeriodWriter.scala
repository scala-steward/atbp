package ph.samson.atbp.liga.io

import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigRenderOptions
import com.typesafe.config.ConfigValueFactory
import ph.samson.atbp.liga.model.*

import scala.jdk.CollectionConverters.*

object PeriodWriter {

  private val RenderOptions =
    ConfigRenderOptions
      .defaults()
      .setOriginComments(false)
      .setComments(false)
      .setFormatted(true)
      .setJson(false)

  private val MatchObjectOpen = """(?m)^(\s*)\{""".r
  private val MatchesSection = """(?s)(matches\s*=\s*\[)(.*?)(\])""".r

  def write(period: Period): String =
    toConfig(period).root().render(RenderOptions) + "\n"

  /** Emission-only render: `# <matchId>` as the first line inside each match
    * object. `matchIds` must align 1:1 with `period.matches` order.
    */
  def write(period: Period, matchIds: List[String]): Either[String, String] =
    if (period.matches.size != matchIds.size) {
      Left(
        s"match ID count ${matchIds.size} != matches ${period.matches.size}"
      )
    } else {
      injectMatchIdComments(write(period), matchIds)
    }

  /** Emission-only; visible for tests that assert matches-scoped injection. */
  private[liga] def injectMatchIdComments(
      hocon: String,
      matchIds: List[String]
  ): Either[String, String] =
    MatchesSection.findFirstMatchIn(hocon) match {
      case Some(section) =>
        injectMatchObjectComments(section.group(2), matchIds).map { injected =>
          val replacement = section.group(1) + injected + section.group(3)
          hocon.patch(section.start, replacement, section.matched.length)
        }
      case None =>
        Left("matches array not found in HOCON")
    }

  private def injectMatchObjectComments(
      matchesBody: String,
      matchIds: List[String]
  ): Either[String, String] = {
    val opens = MatchObjectOpen.findAllMatchIn(matchesBody).toList
    if (opens.size != matchIds.size) {
      Left(s"match object count ${opens.size} != match IDs ${matchIds.size}")
    } else {
      Right(
        // Insert from the end so earlier offsets stay valid.
        opens.zip(matchIds).reverse.foldLeft(matchesBody) {
          case (acc, (m, id)) =>
            val indent = m.group(1)
            val insertion = s"${m.matched}\n$indent    # $id"
            acc.patch(m.start, insertion, m.matched.length)
        }
      )
    }
  }

  private def toConfig(period: Period) = {
    val withCore =
      ConfigFactory
        .empty()
        .withValue("name", ConfigValueFactory.fromAnyRef(period.name))
        .withValue(
          "completed",
          ConfigValueFactory.fromAnyRef(period.completed.toString)
        )

    val withFormat = period.format match {
      case Some(format) =>
        withCore.withValue("format", ConfigValueFactory.fromAnyRef(format))
      case None => withCore
    }

    val withRaceTo = period.raceTo match {
      case Some(raceTo) =>
        withFormat.withValue(
          "race-to",
          ConfigValueFactory.fromAnyRef(raceTo: Integer)
        )
      case None => withFormat
    }

    withRaceTo.withValue(
      "matches",
      ConfigValueFactory.fromIterable(
        period.matches.map(matchValues).asJava
      )
    )
  }

  private def matchValues(
      periodMatch: PeriodMatch
  ): java.util.Map[String, AnyRef] =
    Map[String, AnyRef](
      "player-a" -> periodMatch.playerA.name,
      "player-b" -> periodMatch.playerB.name,
      "score-a" -> Int.box(periodMatch.scoreA),
      "score-b" -> Int.box(periodMatch.scoreB),
      "race-to" -> Int.box(periodMatch.raceTo),
      "handicap-suggested" -> Int.box(periodMatch.handicapSuggested),
      "handicap-applied" -> Int.box(periodMatch.handicapApplied)
    ).asJava
}
