package ph.samson.atbp.jira

import zio.Task
import zio.ZIO
import zio.http.Method
import zio.http.Response
import zio.http.URL
import zio.json.*
import zio.json.ast.Json
import zio.schema.codec.BinaryCodec

import java.nio.charset.StandardCharsets.UTF_8
import scala.annotation.tailrec
import scala.util.matching.Regex

private[jira] object ResponseDecoding {
  private val ExcerptLimit = 4096
  private val PathSegment = """\.([^\.\[\]()]+)|\[(\d+)\]""".r
  private val DiagnosticHeaders =
    Set("content-type", "x-arequestid", "x-request-id", "atl-traceid")
  private val PageFields = Set("startAt", "maxResults", "total", "isLast")

  def decode[A: BinaryCodec](
      response: Response,
      method: Method,
      url: URL,
      expected: String,
      context: String
  ): Task[A] =
    response.body.asChunk.flatMap { bytes =>
      ZIO.fromEither(summon[BinaryCodec[A]].decode(bytes)).mapError { cause =>
        val body = new String(bytes.toArray, UTF_8)
        val headers = response.headers.iterator
          .filter(h =>
            DiagnosticHeaders
              .contains(h.headerName.toLowerCase(java.util.Locale.ROOT))
          )
          .map(h => s"${h.headerName}: ${h.renderedValue}")
          .mkString("; ")
        new Exception(
          s"Failed to decode Jira response as $expected\n" +
            s"Request: ${method.name} ${url.encode}\n" +
            Option
              .when(context.nonEmpty)(s"Context: $context\n")
              .getOrElse("") +
            s"Response: ${response.status.code} ${response.status.reasonPhrase}; ${bytes.length} bytes\n" +
            Option.when(headers.nonEmpty)(s"$headers\n").getOrElse("") +
            s"Decoder: ${cause.getMessage}\n" +
            excerpt(body, cause.getMessage),
          cause
        )
      }
    }

  private def bounded(value: String): String = {
    val printable = value.flatMap {
      case c if c.isControl => f"\\u${c.toInt}%04x"
      case c                => c.toString
    }
    if (printable.length <= ExcerptLimit) printable
    else printable.take(ExcerptLimit) + "… [truncated]"
  }

  @tailrec
  private def nearestObject(
      value: Json,
      location: String,
      segments: List[Regex.Match]
  ): (String, Json) = segments match {
    case Nil             => (location, value)
    case segment :: rest =>
      val next = value match {
        case obj: Json.Obj if Option(segment.group(1)).nonEmpty =>
          obj.fields.find(_._1 == segment.group(1)).map(_._2)
        case arr: Json.Arr if Option(segment.group(2)).nonEmpty =>
          segment.group(2).toIntOption.flatMap(arr.elements.lift)
        case _ => None
      }
      next match {
        case Some(obj: Json.Obj) =>
          nearestObject(obj, location + segment.matched, rest)
        case Some(arr: Json.Arr) =>
          nearestObject(arr, location + segment.matched, rest)
        case _ => (location, value)
      }
  }

  private def excerpt(body: String, error: String): String =
    body.fromJson[Json] match {
      case Left(_)     => s"Response body excerpt: ${bounded(body)}"
      case Right(root) =>
        // DecodeError exposes the JSON path in its message, not as structured
        // data. Follow only its leading path, keeping the nearest object when
        // a field is missing or has the wrong type. Unknown formats fall back
        // to the response root without replacing the original decoder error.
        val path = error.takeWhile(_ != '(')
        val segments = PathSegment.findAllMatchIn(path).toList
        val (location, value) = if (segments.map(_.matched).mkString == path) {
          nearestObject(root, "$", segments)
        } else ("$", root)
        val page = root match {
          case obj: Json.Obj =>
            val fields = obj.fields.filter(f => PageFields.contains(f._1))
            if (fields.isEmpty) ""
            else s"Page: ${bounded(Json.Obj(fields).toJson)}\n"
          case _ => ""
        }
        s"${page}JSON at $location: ${bounded(value.toJson)}"
    }
}
