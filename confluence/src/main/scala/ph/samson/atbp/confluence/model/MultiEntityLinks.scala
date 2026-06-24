package ph.samson.atbp.confluence.model

import zio.http.Path
import zio.http.URL

import java.net.MalformedURLException

case class MultiEntityLinks(
    base: String,
    next: Option[String]
) {
  def url: Either[MalformedURLException, URL] =
    for {
      baseUrl <- URL.decode(base)
      result <- next match {
        case None       => Right(baseUrl)
        case Some(next) => URL.decode(next).map(mergeUrls(baseUrl, _))
      }
    } yield result

  private def mergeUrls(baseUrl: URL, nextUrl: URL): URL = {
    val overlap = pathOverlap(baseUrl.path, nextUrl.path)
    val mergedPath =
      nextUrl.path.segments.drop(overlap).foldLeft(baseUrl.path)(_ / _)
    val kind = if (nextUrl.isAbsolute) nextUrl.kind else baseUrl.kind
    URL(
      path = mergedPath,
      kind = kind,
      queryParams = nextUrl.queryParams,
      fragment = nextUrl.fragment.orElse(baseUrl.fragment)
    )
  }

  private def pathOverlap(base: Path, next: Path): Int = {
    val b = base.segments
    val n = next.segments
    if (b.isEmpty || n.isEmpty) {
      0
    } else {
      (1 to math.min(b.size, n.size))
        .findLast(i => b.takeRight(i) == n.take(i))
        .getOrElse(0)
    }
  }
}
