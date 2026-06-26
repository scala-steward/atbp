package ph.samson.atbp.confluence.model

import zio.http.URL
import zio.test.*

object MultiEntityLinksSpec extends ZIOSpecDefault {

  override def spec = suite("MultiEntityLinks")(
    test("returns base when next is absent") {
      val links =
        MultiEntityLinks("https://example.atlassian.net/wiki/api/v2", None)
      assertTrue(
        links.url == URL.decode("https://example.atlassian.net/wiki/api/v2")
      )
    },
    test("deduplicates overlapping path prefix in next") {
      val links = MultiEntityLinks(
        base = "https://example.atlassian.net/wiki/api/v2",
        next = Some("/wiki/api/v2/pages?cursor=abc")
      )
      assertTrue(
        links.url == URL.decode(
          "https://example.atlassian.net/wiki/api/v2/pages?cursor=abc"
        )
      )
    },
    test("deduplicates overlapping path suffix in base") {
      val links = MultiEntityLinks(
        base = "https://example.atlassian.net/wiki/api/v2/pages/123/children",
        next = Some("children?cursor=abc")
      )
      assertTrue(
        links.url == URL.decode(
          "https://example.atlassian.net/wiki/api/v2/pages/123/children?cursor=abc"
        )
      )
    },
    test("returns Left for invalid base") {
      val links = MultiEntityLinks("not a url", Some("/pages"))
      assertTrue(links.url.isLeft)
    },
    test("returns Left for invalid next") {
      val links = MultiEntityLinks(
        "https://example.atlassian.net/wiki/api/v2",
        Some("://bad")
      )
      assertTrue(links.url.isLeft)
    }
  )
}
