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

  def write(period: Period): String =
    toConfig(period).root().render(RenderOptions) + "\n"

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
