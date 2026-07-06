package ph.samson.atbp.liga.serve

import zio.http.Request

/** Network bind mode for `liga serve` (localhost-only vs LAN audience). */
final case class BindConfig(
    host: String = "127.0.0.1",
    port: Int = 5442,
    lan: Boolean = false
) {

  /** Socket bind address: all interfaces when `--lan`, otherwise `host`. */
  def bindHost: String =
    if (lan) "0.0.0.0" else host

  def isLocalDirector(req: Request): Boolean =
    BindConfig.isLocalDirector(req)
}

object BindConfig {

  def from(config: ServeConfig, lan: Boolean): BindConfig =
    BindConfig(host = config.host, port = config.port, lan = lan)

  def isLocalDirector(req: Request): Boolean =
    req.remoteAddress.forall(_.isLoopbackAddress)
}
