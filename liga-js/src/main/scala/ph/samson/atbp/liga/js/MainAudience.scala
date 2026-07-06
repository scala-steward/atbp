package ph.samson.atbp.liga.js

import com.raquo.laminar.api.L.*
import org.scalajs.dom

import scala.scalajs.js.annotation.JSExportTopLevel

object MainAudience {

  @JSExportTopLevel("start", moduleID = "audience")
  def start(): Unit = {
    val app = div(
      h1("Liga Audience"),
      p("Live bracket display")
    )
    val _ = render(dom.document.getElementById("app"), app)
  }
}
