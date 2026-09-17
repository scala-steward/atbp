package ph.samson.atbp.md2c

import better.files.File
import com.atlassian.adf.markdown.MarkdownParser
import com.atlassian.adf.model.node.Doc
import com.atlassian.adf.model.node.`type`.DocContent
import org.commonmark.ext.gfm.strikethrough.StrikethroughExtension
import org.commonmark.ext.gfm.tables.TablesExtension
import org.commonmark.ext.image.attributes.ImageAttributesExtension
import org.commonmark.ext.ins.InsExtension
import org.commonmark.node.AbstractVisitor
import org.commonmark.node.Link
import org.commonmark.parser.Parser

import java.net.URI
import scala.jdk.CollectionConverters.*
import scala.util.Try

/** Resolve links before ADF conversion, which discards relative link marks. */
private[md2c] object DocumentLinks {
  // Match the extensions used by adf-builder's MarkdownParser.
  private val parser = Parser
    .builder()
    .extensions(
      List(
        ImageAttributesExtension.create(),
        InsExtension.create(),
        StrikethroughExtension.create(),
        TablesExtension.create()
      ).asJava
    )
    .build()

  def parse(
      markdown: String,
      source: File,
      pages: Map[File, String]
  ): (Doc, List[String]) = {
    val destinations = pages.map { case (file, url) =>
      file.path.toAbsolutePath.normalize() -> url
    }
    val base = source.parent.path
    val resolved = List.newBuilder[String]
    val document = parser.parse(markdown)
    document.accept(new AbstractVisitor {
      override def visit(link: Link): Unit = {
        val destination = for {
          uri <- Try(new URI(link.getDestination.replace(" ", "%20"))).toOption
          if !uri.isAbsolute && Option(uri.getRawAuthority).isEmpty
          path <- Option(uri.getPath).filter(p =>
            p.nonEmpty && !p.startsWith("/")
          )
          target <- Try(base.resolve(path).toAbsolutePath.normalize()).toOption
          url <- destinations.get(target)
        } yield {
          url + Option(uri.getRawQuery).fold("")("?" + _) +
            Option(uri.getRawFragment).fold("")("#" + _)
        }
        destination.foreach { url =>
          link.setDestination(url)
          resolved += url
        }
        visitChildren(link)
      }
    })
    Doc.doc(MarkdownParser.children(document, classOf[DocContent])) -> resolved
      .result()
  }
}
