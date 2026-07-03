package ph.samson.atbp.liga.io

import ph.samson.atbp.liga.model.*

object PeriodWriter {

  def write(period: Period): String = {
    val lines = scala.collection.mutable.ArrayBuffer.empty[String]

    lines += s"""name = ${quote(period.name)}"""
    lines += s"""completed = ${quote(period.completed.toString)}"""
    period.format.foreach(format => lines += s"""format = ${quote(format)}""")
    period.raceTo.foreach(raceTo => lines += s"""race-to = $raceTo""")

    lines += ""
    lines += renderMatches(period.matches)

    lines.mkString("\n") + "\n"
  }

  private def renderMatches(matches: List[PeriodMatch]): String =
    if (matches.isEmpty) {
      "matches = []"
    } else {
      val entries = matches.map(renderMatch).mkString("\n")
      s"matches = [\n$entries\n]"
    }

  private def renderMatch(periodMatch: PeriodMatch): String =
    s"""  {
       |    player-a = ${quote(periodMatch.playerA.name)}
       |    player-b = ${quote(periodMatch.playerB.name)}
       |    score-a = ${periodMatch.scoreA}
       |    score-b = ${periodMatch.scoreB}
       |    race-to = ${periodMatch.raceTo}
       |    handicap-suggested = ${periodMatch.handicapSuggested}
       |    handicap-applied = ${periodMatch.handicapApplied}
       |  }""".stripMargin

  private def quote(value: String): String =
    "\"" + value.replace("\\", "\\\\").replace("\"", "\\\"") + "\""
}
