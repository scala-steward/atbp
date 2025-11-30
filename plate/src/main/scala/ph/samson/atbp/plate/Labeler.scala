package ph.samson.atbp.plate

import better.files.File
import ph.samson.atbp.jira.Client
import ph.samson.atbp.plate.JiraOps.*
import zio.Cause
import zio.Task
import zio.ZIO
import zio.ZLayer

trait Labeler {
  def label(
      source: File,
      value: String,
      excludeProjects: List[String]
  ): Task[Int]
}

object Labeler {

  private val JiraKey = """\(https://.*/browse/([A-Z]+-\d+)\)""".r

  private class LiveImpl(client: Client) extends Labeler {
    override def label(
        source: File,
        value: String,
        excludeProjects: List[String]
    ): Task[Int] = {

      def excludeKey(key: String) =
        excludeProjects.contains(key.substring(0, key.indexOf('-')))

      ZIO.logSpan(s"label $value") {
        for {
          searchLabeled <- client.search(s"labels = $value").fork

          content <- ZIO.attemptBlockingIO(source.contentAsString)
          sourceKeys = JiraKey.findAllMatchIn(content).map(_.group(1)).toList
          (ancestors, descendants) <-
            client.getAncestors(sourceKeys) <&>
              client.getDescendants(sourceKeys)
          localKeys = sourceKeys ++
            ancestors.map(_.key) ++
            descendants.map(_.key)

          alreadyLabeled <- searchLabeled.join
          currentKeys = alreadyLabeled.map(_.key)

          toAdd = localKeys.filterNot(key =>
            currentKeys.contains(key) || excludeKey(key)
          )
          _ <- ZIO
            .logInfo(s"toAdd ${toAdd.length}: $toAdd")
            .when(toAdd.nonEmpty)
          add <- ZIO
            .foreachParDiscard(toAdd)(key =>
              client.addLabel(key, value).catchAll { error =>
                ZIO.logWarningCause(s"Failed labeling $key", Cause.fail(error))
              }
            )
            .fork

          toRemove = currentKeys.filterNot(localKeys.contains)
          _ <- ZIO
            .logInfo(s"toRemove ${toRemove.length}: $toRemove")
            .when(toRemove.nonEmpty)
          _ <- ZIO.foreachParDiscard(toRemove)(key =>
            client.removeLabel(key, value)
          )

          _ <- add.join
        } yield 1
      }
    }

  }

  def layer() = ZLayer {
    for {
      client <- ZIO.service[Client]
    } yield LiveImpl(client): Labeler
  }
}
