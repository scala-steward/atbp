package ph.samson.atbp.liga.io

import better.files.File
import ph.samson.atbp.liga.model.*
import zio.Config
import zio.ConfigProvider
import zio.Task
import zio.ZIO
import zio.config.magnolia.deriveConfig
import zio.config.typesafe.*

import java.time.LocalDate

object PeriodCodec {

  private final case class PeriodMatchConfig(
      playerA: String,
      playerB: String,
      scoreA: Int,
      scoreB: Int,
      raceTo: Int,
      handicapSuggested: Int,
      handicapApplied: Int
  )

  private final case class PeriodConfig(
      name: String,
      completed: String,
      format: Option[String] = None,
      raceTo: Option[Int] = None,
      matches: List[PeriodMatchConfig] = Nil
  )

  private val Descriptor: Config[PeriodConfig] = deriveConfig[PeriodConfig]

  def parseString(hocon: String): Task[Period] =
    for {
      provider <- ConfigProvider.fromHoconStringZIO(hocon)
      config <- provider.kebabCase.load(Descriptor)
      period <- toPeriod(config)
    } yield period

  def parseFile(file: File): Task[Period] =
    for {
      content <- ZIO.attemptBlockingIO(file.contentAsString)
      period <- parseString(content)
    } yield period

  private def toPeriod(config: PeriodConfig): Task[Period] =
    ZIO
      .attempt(LocalDate.parse(config.completed))
      .mapBoth(
        _ =>
          new IllegalArgumentException(
            s"Invalid completed date: expected ISO-8601 YYYY-MM-DD, got '${config.completed}'"
          ),
        completed =>
          Period(
            name = config.name,
            completed = completed,
            format = config.format,
            raceTo = config.raceTo,
            matches = config.matches.map(toMatch)
          )
      )

  private def toMatch(config: PeriodMatchConfig): PeriodMatch =
    PeriodMatch(
      playerA = Player(config.playerA),
      playerB = Player(config.playerB),
      scoreA = config.scoreA,
      scoreB = config.scoreB,
      raceTo = config.raceTo,
      handicapSuggested = config.handicapSuggested,
      handicapApplied = config.handicapApplied
    )
}
