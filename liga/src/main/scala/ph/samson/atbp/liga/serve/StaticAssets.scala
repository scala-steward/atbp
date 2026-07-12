package ph.samson.atbp.liga.serve

import zio.*
import zio.http.*

import java.io.InputStream

/** Classpath-backed director and audience SPA shells plus compiled Scala.js
  * bundles.
  */
object StaticAssets {

  private val JsRoot = "liga/web/js"
  private val CssRoot = "liga/web/css"

  val directorScriptName: String = "director.js"
  val audienceScriptName: String = "audience.js"
  val directorCssName: String = "director.css"

  val directorHtml: String =
    htmlShell("Liga Director", directorScriptName, Some(directorCssName))
  val audienceHtml: String = htmlShell("Liga Audience", audienceScriptName)

  def jsAssetPath(fileName: String): String = s"/assets/js/$fileName"
  def cssAssetPath(fileName: String): String = s"/assets/css/$fileName"

  def assetRoutes(bind: BindConfig): Routes[Any, Response] =
    zio.http.Routes(
      Method.GET / "assets" / "js" / string("file") -> handler {
        (fileName: String, req: Request) =>
          if (
            bind.lan && fileName == directorScriptName && !bind
              .isLocalDirector(req)
          ) {
            ZIO.succeed(Response.text("forbidden").status(Status.Forbidden))
          } else if (fileName.contains("..") || fileName.contains("/")) {
            ZIO.succeed(Response.notFound)
          } else {
            serveClasspathResource(
              s"$JsRoot/$fileName",
              MediaType.application.javascript
            )
          }
      },
      Method.GET / "assets" / "css" / string("file") -> handler {
        (fileName: String, _: Request) =>
          if (fileName.contains("..") || fileName.contains("/")) {
            ZIO.succeed(Response.notFound)
          } else {
            serveClasspathResource(
              s"$CssRoot/$fileName",
              MediaType.text.`css`
            )
          }
      }
    )

  private def htmlShell(title: String, scriptFile: String): String =
    htmlShell(title, scriptFile, None)

  private def htmlShell(
      title: String,
      scriptFile: String,
      cssFile: Option[String]
  ): String = {
    val cssLink = cssFile
      .map(file => s"""  <link rel="stylesheet" href="/assets/css/$file">""")
      .getOrElse("")
    s"""<!DOCTYPE html>
       |<html lang="en">
       |<head>
       |  <meta charset="utf-8">
       |  <title>$title</title>
       |$cssLink
       |</head>
       |<body>
       |  <div id="app"></div>
       |  <script type="module">
       |    import { start } from "/assets/js/$scriptFile";
       |    start();
       |  </script>
       |</body>
       |</html>""".stripMargin
  }

  private def serveClasspathResource(
      path: String,
      mediaType: MediaType
  ): ZIO[Any, Nothing, Response] =
    ZIO.succeed {
      resourceStream(path) match {
        case None =>
          Response.notFound
        case Some(stream) =>
          try {
            val bytes = stream.readAllBytes()
            Response(
              headers = Headers(Header.ContentType(mediaType)),
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
