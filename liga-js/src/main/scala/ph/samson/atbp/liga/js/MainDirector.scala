package ph.samson.atbp.liga.js

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import ph.samson.atbp.liga.js.api.ApiClient
import ph.samson.atbp.liga.js.director.DirectorApp

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.annotation.JSExportTopLevel

object MainDirector {

  @JSExportTopLevel("start", moduleID = "director")
  def start(): Unit = {
    val app = DirectorApp(ApiClient())
    val root = dom.document.getElementById("app")
    val _ = render(root, app)
  }
}
