package ph.samson.atbp.liga.testsupport

object PeriodHoconTestSupport {

  private val MatchObjectBody = """(?s)\{\s*(.*?)\s*\}""".r

  /** First non-empty interior line of each `{ ... }` block in HOCON text. */
  def firstInteriorLines(hocon: String): List[String] =
    MatchObjectBody
      .findAllMatchIn(hocon)
      .map(_.group(1))
      .map { body =>
        body.linesIterator.map(_.trim).find(_.nonEmpty).getOrElse("")
      }
      .toList

  /** True when no match object's first interior line is a bracket ID comment.
    */
  def matchObjectsLackBracketIdComments(hocon: String): Boolean =
    firstInteriorLines(hocon).forall(line => !line.startsWith("# "))
}
