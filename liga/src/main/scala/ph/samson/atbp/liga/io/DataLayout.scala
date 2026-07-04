package ph.samson.atbp.liga.io

import better.files.File

object DataLayout {

  val TournamentDirPrefix = "tournament-"

  def discoverPeriodFiles(root: File): List[File] =
    if (!root.exists) Nil
    else
      root.listRecursively
        .filter(_.isRegularFile)
        .filter(_.extension(includeDot = false).contains("liga"))
        .filterNot(isUnderTournamentDir(_, root))
        .toList
        .sortBy(_.pathAsString)

  def isUnderTournamentDir(file: File, root: File): Boolean =
    relativeSegments(file, root).exists(_.startsWith(TournamentDirPrefix))

  private def relativeSegments(file: File, root: File): List[String] = {
    val relative = root.relativize(file)
    (0 until relative.getNameCount).map(relative.getName(_).toString).toList
  }
}
