package ph.samson.atbp.liga.serve

import zio.*
import zio.http.*

import java.io.InputStream

/** Classpath-backed director and audience SPA shells plus compiled Scala.js
  * bundles.
  */
object StaticAssets {

  private val JsRoot = "liga/web/js"

  val directorScriptName: String = "director.js"
  val audienceScriptName: String = "audience.js"

  val directorHtml: String = htmlShell("Liga Director", directorScriptName)
  val audienceHtml: String = htmlShell("Liga Audience", audienceScriptName)

  def jsAssetPath(fileName: String): String = s"/assets/js/$fileName"

  def assetRoutes: Routes[Any, Response] =
    zio.http.Routes(
      Method.GET / "assets" / "js" / string("file") -> handler {
        (fileName: String, _: Request) =>
          serveClasspathResource(s"$JsRoot/$fileName")
      }
    )

  private def htmlShell(title: String, scriptFile: String): String =
    s"""<!DOCTYPE html>
       |<html lang="en">
       |<head>
       |  <meta charset="utf-8">
       |  <title>$title</title>
       |</head>
       |<body>
       |  <div id="app"></div>
       |  <script type="module">
       |    import { start } from "/assets/js/$scriptFile";
       |    start();
       |  </script>
       |</body>
       |</html>""".stripMargin

  private def serveClasspathResource(
      path: String
  ): ZIO[Any, Nothing, Response] =
    ZIO.succeed {
      resourceStream(path) match {
        case None =>
          Response.notFound
        case Some(stream) =>
          try {
            val bytes = stream.readAllBytes()
            Response(
              headers = Headers(
                Header.ContentType(MediaType.application.javascript)
              ),
              body = Body.fromArray(bytes)
            )
          } finally stream.close()
      }
    }

  private def resourceStream(path: String): Option[InputStream] = {
    val normalized =
      if (path.startsWith("/")) path.drop(1)
      else path
    Option(getClass.getClassLoader.getResourceAsStream(normalized))
  }
}
