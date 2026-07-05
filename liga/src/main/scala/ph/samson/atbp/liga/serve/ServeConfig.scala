package ph.samson.atbp.liga.serve

import better.files.File
import zio.Config
import zio.ConfigProvider
import zio.Task
import zio.ZIO
import zio.config.magnolia.deriveConfig
import zio.config.typesafe.*

/** HTTP serve settings (bind address, poll interval, etc.). */
final case class ServeConfig(
    host: String = "127.0.0.1",
    port: Int = 5442,
    audiencePollIntervalSeconds: Int = 5
)

object ServeConfig {
  val ConfFileName = "liga.conf"
  val Descriptor: Config[ServeConfig] = deriveConfig[ServeConfig]
  val default: ServeConfig = ServeConfig()

  def parseHocon(hocon: String): Task[ServeConfig] =
    for {
      provider <- ConfigProvider.fromHoconStringZIO(hocon)
      config <- provider.kebabCase.load(Descriptor)
    } yield config

  /** Load `liga.conf` from `dataDir` when present; otherwise defaults. */
  def load(dataDir: File): Task[ServeConfig] = {
    val confFile = dataDir / ConfFileName
    if (confFile.isRegularFile) {
      for {
        content <- ZIO.attemptBlockingIO(confFile.contentAsString)
        config <- parseHocon(content)
      } yield config
    } else {
      ZIO.succeed(default)
    }
  }

  def withOverrides(base: ServeConfig, host: Option[String], port: Option[Int]): ServeConfig =
    base.copy(
      host = host.getOrElse(base.host),
      port = port.getOrElse(base.port)
    )
}
