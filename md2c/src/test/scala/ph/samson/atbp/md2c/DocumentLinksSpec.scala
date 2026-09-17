package ph.samson.atbp.md2c

import better.files.File
import com.atlassian.adf.model.mark.Link
import com.atlassian.adf.model.node.Doc
import com.atlassian.adf.model.node.Text
import zio.test.*

import scala.jdk.StreamConverters.*

object DocumentLinksSpec extends ZIOSpecDefault {
  private val source = File("/docs/guide/README.md")
  private val target = File("/docs/Reference + café.md")
  private val targetUrl =
    "https://example.atlassian.net/wiki/spaces/DOC/pages/42"

  override def spec = suite("DocumentLinks")(
    test(
      "resolves encoded paths, literal plus signs and query/fragment suffixes"
    ) {
      val (doc, resolved) = DocumentLinks.parse(
        "[details](../guide/../Reference%20+%20caf%C3%A9.md?mode=read#some%20heading)",
        source,
        Map(target -> targetUrl)
      )
      val expected = s"$targetUrl?mode=read#some%20heading"
      assertTrue(
        links(doc).map(_.href()) == List(expected),
        resolved == List(expected)
      )
    },
    test(
      "resolves reference links and preserves labels, formatting and titles"
    ) {
      val (doc, _) = DocumentLinks.parse(
        """See [**the reference**][ref].
          |
          |[ref]: <../Reference + café.md> "Reference title"
          |""".stripMargin,
        source,
        Map(target -> targetUrl)
      )
      val text = doc
        .allNodesOfType(classOf[Text])
        .toScala(List)
        .find(_.text() == "the reference")
        .get
      assertTrue(
        links(doc).map(_.href()) == List(targetUrl),
        links(doc).head.title().orElse("") == "Reference title",
        text.markTypes().contains("strong")
      )
    },
    test("rewrites links nested in tables and lists") {
      val (doc, _) = DocumentLinks.parse(
        """- [list](../Reference%20+%20caf%C3%A9.md)
          |
          || Page |
          || --- |
          || [table](../Reference%20+%20caf%C3%A9.md) |
          |""".stripMargin,
        source,
        Map(target -> targetUrl)
      )
      assertTrue(links(doc).map(_.href()) == List(targetUrl, targetUrl))
    },
    test(
      "keeps external links, unresolved destinations, images and code behavior"
    ) {
      val markdown = """[external](https://example.org/Reference.md)
                       |[mail](mailto:docs@example.org)
                       |[host](//example.org/Reference.md)
                       |[anchor](#details)
                       |[root](/Reference.md)
                       |[missing](Missing.md)
                       |[outside](../../Outside.md)
                       |[invalid](bad%GG.md)
                       |[data](data.csv)
                       |![image](../Reference%20+%20caf%C3%A9.md)
                       |`[inline](../Reference%20+%20caf%C3%A9.md)`
                       |
                       |```markdown
                       |[code](../Reference%20+%20caf%C3%A9.md)
                       |```
                       |""".stripMargin
      for {
        baseline <- Parser.parseMarkdown(markdown)
        (actual, resolved) = DocumentLinks.parse(
          markdown,
          source,
          Map(target -> targetUrl)
        )
      } yield assertTrue(actual == baseline, resolved.isEmpty)
    }
  )

  private def links(doc: Doc): List[Link] =
    doc
      .allNodesOfType(classOf[Text])
      .toScala(List)
      .flatMap(
        _.marks(classOf[Link]).toScala(List)
      )
}
