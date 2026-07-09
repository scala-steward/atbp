package ph.samson.atbp.liga.js

import com.raquo.laminar.api.L.*
import org.scalajs.dom
import ph.samson.atbp.liga.js.api.ApiClient
import ph.samson.atbp.liga.js.audience.AudienceApp

import scala.concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.annotation.JSExportTopLevel

object MainAudience {

  @JSExportTopLevel("start", moduleID = "audience")
  def start(): Unit = {
    val app = AudienceApp(ApiClient())
    val root = dom.document.getElementById("app")
    val _ = render(root, app)
  }
}
