package ph.samson.atbp.liga.cli

object MarkdownTable {

  enum Alignment {
    case Left, Right
  }

  final case class Column(header: String, align: Alignment = Alignment.Left)

  def render(columns: List[Column], rows: List[List[String]]): String = {
    require(columns.nonEmpty)
    require(rows.forall(_.length == columns.length))

    val widths = columns.indices.map { index =>
      (columns(index).header :: rows.map(_(index))).map(_.length).max
    }.toList
    val aligns = columns.map(_.align)

    val header = formatRow(columns.map(_.header), widths, aligns)
    val separator = formatRow(
      columns.zip(widths).zip(aligns).map { case ((_, width), align) =>
        separatorCell(width, align)
      },
      widths,
      aligns
    )
    val dataRows = rows.map(formatRow(_, widths, aligns))
    (header :: separator :: dataRows).mkString("\n") + "\n"
  }

  private def formatRow(
      cells: List[String],
      widths: List[Int],
      aligns: List[Alignment]
  ): String =
    "| " + cells
      .zip(widths)
      .zip(aligns)
      .map { case ((cell, width), align) =>
        pad(cell, width, align)
      }
      .mkString(" | ") + " |"

  private def pad(text: String, width: Int, align: Alignment): String =
    align match {
      case Alignment.Left =>
        if (text.length >= width) text else text + (" " * (width - text.length))
      case Alignment.Right =>
        if (text.length >= width) text else (" " * (width - text.length)) + text
    }

  private def separatorCell(width: Int, align: Alignment): String =
    align match {
      case Alignment.Left  => "-" * width
      case Alignment.Right => "-" * math.max(1, width - 1) + ":"
    }
}
