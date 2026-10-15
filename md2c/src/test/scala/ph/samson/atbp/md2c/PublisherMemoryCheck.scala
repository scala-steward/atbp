package ph.samson.atbp.md2c

import zio.Console
import zio.Runtime
import zio.ZIO
import zio.ZIOAppDefault

/** Run in a separate JVM with -Xmx128m. ZTestLogger retains lazy log message
  * closures (and their page bodies), so use the application runtime here.
  */
object PublisherMemoryCheck extends ZIOAppDefault {
  override val bootstrap = Runtime.removeDefaultLoggers

  override def run = for {
    result <- ZIO.scoped(PublisherLinksSpec.largeDocumentSet(1024))
    _ <- ZIO.attempt(require(result, "Not all pages were published"))
    _ <- Console.printLine("Published 1024 documents (273 MiB of Markdown).")
  } yield ()
}
