package ph.samson.atbp.plate

import better.files.File
import ph.samson.atbp.jira.Client
import ph.samson.atbp.jira.model.Changelog
import ph.samson.atbp.jira.model.Changelog.ChangeDetails
import ph.samson.atbp.jira.model.Comment
import ph.samson.atbp.jira.model.Issue
import ph.samson.atbp.jira.model.UserDetails
import zio.Ref
import zio.Task
import zio.ZIO
import zio.ZLayer
import zio.test.*

import java.time.Instant
import java.time.ZoneId

object CookingExcludeSpec extends ZIOSpecDefault {

  private val testInstant = Instant.parse("2024-06-15T12:00:00Z")
  private val testTime = testInstant.atZone(ZoneId.systemDefault())

  private final case class RecordingClient(
      getIssueKeys: Ref[List[String]],
      searchJql: Ref[List[String]],
      getChangelogKeys: Ref[List[String]],
      getCommentKeys: Ref[List[String]],
      issues: Map[String, Issue],
      children: Map[String, List[Issue]],
      changelogs: Map[String, List[Changelog]],
      comments: Map[String, List[Comment]]
  ) extends Client {

    private def unexpected(method: String) =
      ZIO.fail(new Exception(s"unexpected Client.$method"))

    override def getIssue(key: String): Task[Issue] =
      getIssueKeys.update(_ :+ key) *>
        ZIO
          .fromOption(issues.get(key))
          .orElseFail(new Exception(s"no fixture issue for $key"))

    override def getChangelogs(key: String): Task[List[Changelog]] =
      getChangelogKeys.update(_ :+ key) *>
        ZIO.succeed(changelogs.getOrElse(key, Nil))

    override def getComments(key: String): Task[List[Comment]] =
      getCommentKeys.update(_ :+ key) *>
        ZIO.succeed(comments.getOrElse(key, Nil))

    override def search(jql: String): Task[List[Issue]] =
      searchJql.update(_ :+ jql).as {
        if (jql.startsWith("parent IN (")) {
          val keysPart = jql.stripPrefix("parent IN (").takeWhile(_ != ')')
          keysPart
            .split(',')
            .toList
            .flatMap(k => children.getOrElse(k.trim, Nil))
        } else {
          Nil
        }
      }

    override def addLabel(key: String, label: String): Task[Unit] =
      unexpected("addLabel")

    override def removeLabel(key: String, label: String): Task[Unit] =
      unexpected("removeLabel")

    override def rankIssuesBefore(
        issues: List[String],
        reference: String,
        rankCustomFieldId: Option[Int]
    ): Task[Unit] = unexpected("rankIssuesBefore")

    override def rankIssuesAfter(
        issues: List[String],
        reference: String,
        rankCustomFieldId: Option[Int]
    ): Task[Unit] = unexpected("rankIssuesAfter")
  }

  private object RecordingClient {
    def layer(
        issues: Map[String, Issue],
        children: Map[String, List[Issue]],
        changelogs: Map[String, List[Changelog]],
        comments: Map[String, List[Comment]]
    ): ZLayer[Any, Nothing, Client & RecordingClient] =
      ZLayer {
        for {
          getIssueKeys <- Ref.make(List.empty[String])
          searchJql <- Ref.make(List.empty[String])
          getChangelogKeys <- Ref.make(List.empty[String])
          getCommentKeys <- Ref.make(List.empty[String])
        } yield RecordingClient(
          getIssueKeys,
          searchJql,
          getChangelogKeys,
          getCommentKeys,
          issues,
          children,
          changelogs,
          comments
        ): Client & RecordingClient
      }
  }

  private def cookingIssue(key: String): Issue = {
    val status = Issue.Status(
      id = "3",
      name = "In Progress",
      description = "",
      statusCategory = Issue.Status.Category(
        id = 4,
        key = "indeterminate",
        name = "In Progress"
      )
    )
    val fields = Issue.Fields(
      summary = "Summary",
      statuscategorychangedate = testTime,
      created = testTime.minusDays(30),
      updated = testTime,
      duedate = None,
      resolution = None,
      versions = Nil,
      fixVersions = Nil,
      labels = Nil,
      priority = None,
      issuetype = Issue.IssueType(
        self = "https://example.atlassian.net/rest/api/3/issuetype/1",
        id = "1",
        name = "Task",
        hierarchyLevel = 0
      ),
      status = status,
      parent = None
    )
    Issue(
      id = "10001",
      key = key,
      self = "https://example.atlassian.net/rest/api/3/issue/10001",
      fields = fields
    )
  }

  private def progressChangelog: Changelog = {
    val author = UserDetails(
      self = "https://example.atlassian.net/rest/api/3/user?accountId=x",
      accountId = "x",
      emailAddress = None,
      displayName = "Alice",
      active = true,
      timeZone = "UTC",
      accountType = "atlassian",
      key = None,
      name = None
    )
    Changelog(
      id = "1",
      author = author,
      created = testTime,
      items = List(
        ChangeDetails(
          field = "status",
          fieldType = Some("jira"),
          fieldId = Some("status"),
          from = Some("1"),
          fromString = Some("To Do"),
          to = Some("3"),
          toAsString = Some("In Progress")
        )
      ),
      historyMetadata = None
    )
  }

  override def spec = suite("CookingExclude")(
    test("rejectedStatus rejects stale with non-empty exclude") {
      val message = CookingExclude.rejectedStatus("stale", List("ABC"))
      assertTrue(message.exists(_.contains("stale")))
    },
    test("rejectedStatus rejects done with non-empty exclude") {
      val message = CookingExclude.rejectedStatus("done", List("ABC"))
      assertTrue(message.exists(_.contains("done")))
    },
    test("rejectedStatus allows cooking with non-empty exclude") {
      assertTrue(CookingExclude.rejectedStatus("cooking", List("ABC")).isEmpty)
    },
    test("rejectedStatus allows stale with empty exclude") {
      assertTrue(CookingExclude.rejectedStatus("stale", Nil).isEmpty)
    },
    test("empty exclude still reports cooking") {
      val key = "ABC-123"
      val plateLine =
        s"- [ABC-123 Summary](https://example.atlassian.net/browse/$key)"
      for {
        _ <- TestClock.setTime(testInstant)
        source <- ZIO.attemptBlockingIO {
          val file = File.newTemporaryFile("plate", ".md").deleteOnExit()
          file.overwrite(s"# Section\n$plateLine\n")
          file
        }
        recording <- ZIO.service[RecordingClient]
        result <- ZIO
          .serviceWithZIO[Inspector](_.cooking(source, None, Nil))
          .provideSome[RecordingClient](Inspector.layer())
        getIssueKeys <- recording.getIssueKeys.get
        content <- ZIO.attemptBlockingIO(result.contentAsString)
        expected = source.sibling(
          source.nameWithoutExtension(includeAll = false) + ".done.md"
        )
      } yield assertTrue(
        result.path == expected.path,
        content.contains(plateLine),
        getIssueKeys == List(key)
      )
    },
    test("excluded plate line with progress is kept") {
      val key = "ABC-1"
      val plateLine =
        s"- [ABC-1 Summary](https://example.atlassian.net/browse/$key)"
      for {
        _ <- TestClock.setTime(testInstant)
        source <- ZIO.attemptBlockingIO {
          val file = File.newTemporaryFile("plate", ".md").deleteOnExit()
          file.overwrite(s"# Section\n$plateLine\n")
          file
        }
        recording <- ZIO.service[RecordingClient]
        result <- ZIO
          .serviceWithZIO[Inspector](_.cooking(source, None, List("ABC")))
          .provideSome[RecordingClient](Inspector.layer())
        getIssueKeys <- recording.getIssueKeys.get
        searchJql <- recording.searchJql.get
        content <- ZIO.attemptBlockingIO(result.contentAsString)
      } yield assertTrue(
        content.contains("# Section"),
        content.contains(plateLine),
        content.contains("status -> In Progress"),
        getIssueKeys == List(key),
        searchJql.exists(_.contains(s"parent IN ($key)"))
      )
    },
    test("excluded descendant is not fattened") {
      val parentKey = "ABC-1"
      val descendantKey = "ABC-2"
      val plateLine =
        s"- [ABC-1 Summary](https://example.atlassian.net/browse/$parentKey)"
      for {
        _ <- TestClock.setTime(testInstant)
        source <- ZIO.attemptBlockingIO {
          val file = File.newTemporaryFile("plate", ".md").deleteOnExit()
          file.overwrite(s"# Section\n$plateLine\n")
          file
        }
        recording <- ZIO.service[RecordingClient]
        result <- ZIO
          .serviceWithZIO[Inspector](_.cooking(source, None, List("ABC")))
          .provideSome[RecordingClient](Inspector.layer())
        getIssueKeys <- recording.getIssueKeys.get
        getChangelogKeys <- recording.getChangelogKeys.get
        getCommentKeys <- recording.getCommentKeys.get
        content <- ZIO.attemptBlockingIO(result.contentAsString)
      } yield assertTrue(
        content.contains(plateLine),
        !content.contains(descendantKey),
        getIssueKeys == List(parentKey),
        getChangelogKeys == List(parentKey),
        !getCommentKeys.contains(descendantKey)
      )
    },
    test("included cooking line after excluded line is reported") {
      val excludedKey = "ABC-1"
      val includedKey = "XYZ-2"
      val excludedLine =
        s"- [ABC-1 Excluded](https://example.atlassian.net/browse/$excludedKey)"
      val includedLine =
        s"- [XYZ-2 Included](https://example.atlassian.net/browse/$includedKey)"
      for {
        _ <- TestClock.setTime(testInstant)
        source <- ZIO.attemptBlockingIO {
          val file = File.newTemporaryFile("plate", ".md").deleteOnExit()
          file.overwrite(s"# Section\n$excludedLine\n$includedLine\n")
          file
        }
        recording <- ZIO.service[RecordingClient]
        result <- ZIO
          .serviceWithZIO[Inspector](_.cooking(source, None, List("ABC")))
          .provideSome[RecordingClient](Inspector.layer())
        getIssueKeys <- recording.getIssueKeys.get
        content <- ZIO.attemptBlockingIO(result.contentAsString)
      } yield assertTrue(
        content.contains(excludedLine),
        content.contains(includedLine),
        getIssueKeys.sorted == List(excludedKey, includedKey).sorted
      )
    },
    test(
      "excluded plate line with only excluded descendant progress is omitted"
    ) {
      val parentKey = "ABC-3"
      val descendantKey = "ABC-4"
      val plateLine =
        s"- [ABC-3 Summary](https://example.atlassian.net/browse/$parentKey)"
      for {
        _ <- TestClock.setTime(testInstant)
        source <- ZIO.attemptBlockingIO {
          val file = File.newTemporaryFile("plate", ".md").deleteOnExit()
          file.overwrite(s"# Section\n$plateLine\n")
          file
        }
        recording <- ZIO.service[RecordingClient]
        result <- ZIO
          .serviceWithZIO[Inspector](_.cooking(source, None, List("ABC")))
          .provideSome[RecordingClient](Inspector.layer())
        getIssueKeys <- recording.getIssueKeys.get
        searchJql <- recording.searchJql.get
        getChangelogKeys <- recording.getChangelogKeys.get
        getCommentKeys <- recording.getCommentKeys.get
        content <- ZIO.attemptBlockingIO(result.contentAsString)
      } yield assertTrue(
        !content.contains("# Section"),
        !content.contains(plateLine),
        !content.contains(descendantKey),
        getIssueKeys == List(parentKey),
        searchJql.exists(_.contains(s"parent IN ($parentKey)")),
        getChangelogKeys == List(parentKey),
        !getChangelogKeys.contains(descendantKey),
        !getCommentKeys.contains(descendantKey)
      )
    },
    test("excluded descendant does not keep parent") {
      val parentKey = "INC-3"
      val descendantKey = "EXC-2"
      val plateLine =
        s"- [INC-3 Parent](https://example.atlassian.net/browse/$parentKey)"
      for {
        _ <- TestClock.setTime(testInstant)
        source <- ZIO.attemptBlockingIO {
          val file = File.newTemporaryFile("plate", ".md").deleteOnExit()
          file.overwrite(s"# Section\n$plateLine\n")
          file
        }
        recording <- ZIO.service[RecordingClient]
        result <- ZIO
          .serviceWithZIO[Inspector](_.cooking(source, None, List("EXC")))
          .provideSome[RecordingClient](Inspector.layer())
        getIssueKeys <- recording.getIssueKeys.get
        searchJql <- recording.searchJql.get
        content <- ZIO.attemptBlockingIO(result.contentAsString)
      } yield assertTrue(
        !content.contains(plateLine),
        !content.contains(descendantKey),
        getIssueKeys == List(parentKey),
        searchJql.exists(_.contains(s"parent IN ($parentKey)"))
      )
    },
    test("parent progress remains; excluded descendant omitted") {
      val parentKey = "INC-1"
      val descendantKey = "EXC-2"
      val plateLine =
        s"- [INC-1 Parent](https://example.atlassian.net/browse/$parentKey)"
      for {
        _ <- TestClock.setTime(testInstant)
        source <- ZIO.attemptBlockingIO {
          val file = File.newTemporaryFile("plate", ".md").deleteOnExit()
          file.overwrite(s"# Section\n$plateLine\n")
          file
        }
        recording <- ZIO.service[RecordingClient]
        result <- ZIO
          .serviceWithZIO[Inspector](_.cooking(source, None, List("EXC")))
          .provideSome[RecordingClient](Inspector.layer())
        getIssueKeys <- recording.getIssueKeys.get
        content <- ZIO.attemptBlockingIO(result.contentAsString)
      } yield assertTrue(
        content.contains(plateLine),
        content.contains("status -> In Progress"),
        !content.contains(descendantKey),
        getIssueKeys == List(parentKey)
      )
    },
    test("included descendant keeps parent when sibling excluded") {
      val parentKey = "INC-5"
      val includedDescendantKey = "INC-2"
      val excludedDescendantKey = "EXC-3"
      val plateLine =
        s"- [INC-5 Parent](https://example.atlassian.net/browse/$parentKey)"
      for {
        _ <- TestClock.setTime(testInstant)
        source <- ZIO.attemptBlockingIO {
          val file = File.newTemporaryFile("plate", ".md").deleteOnExit()
          file.overwrite(s"# Section\n$plateLine\n")
          file
        }
        recording <- ZIO.service[RecordingClient]
        result <- ZIO
          .serviceWithZIO[Inspector](_.cooking(source, None, List("EXC")))
          .provideSome[RecordingClient](Inspector.layer())
        getIssueKeys <- recording.getIssueKeys.get
        searchJql <- recording.searchJql.get
        content <- ZIO.attemptBlockingIO(result.contentAsString)
      } yield assertTrue(
        content.contains(plateLine),
        content.contains(includedDescendantKey),
        content.contains("status -> In Progress"),
        !content.contains(excludedDescendantKey),
        getIssueKeys == List(parentKey),
        searchJql.exists(_.contains(s"parent IN ($parentKey)"))
      )
    },
    test("included descendant keeps excluded plate line") {
      val parentKey = "ABC-5"
      val includedDescendantKey = "XYZ-3"
      val excludedDescendantKey = "ABC-6"
      val plateLine =
        s"- [ABC-5 Summary](https://example.atlassian.net/browse/$parentKey)"
      for {
        _ <- TestClock.setTime(testInstant)
        source <- ZIO.attemptBlockingIO {
          val file = File.newTemporaryFile("plate", ".md").deleteOnExit()
          file.overwrite(s"# Section\n$plateLine\n")
          file
        }
        recording <- ZIO.service[RecordingClient]
        result <- ZIO
          .serviceWithZIO[Inspector](_.cooking(source, None, List("ABC")))
          .provideSome[RecordingClient](Inspector.layer())
        getIssueKeys <- recording.getIssueKeys.get
        searchJql <- recording.searchJql.get
        getChangelogKeys <- recording.getChangelogKeys.get
        getCommentKeys <- recording.getCommentKeys.get
        content <- ZIO.attemptBlockingIO(result.contentAsString)
      } yield assertTrue(
        content.contains("# Section"),
        content.contains(plateLine),
        content.contains(includedDescendantKey),
        content.contains("status -> In Progress"),
        !content.contains(excludedDescendantKey),
        getIssueKeys == List(parentKey),
        searchJql.exists(_.contains(s"parent IN ($parentKey)")),
        getChangelogKeys == List(parentKey, includedDescendantKey),
        !getChangelogKeys.contains(excludedDescendantKey),
        !getCommentKeys.contains(excludedDescendantKey)
      )
    }
  ).provide(
    RecordingClient.layer(
      issues = Map(
        "ABC-123" -> cookingIssue("ABC-123"),
        "ABC-1" -> cookingIssue("ABC-1"),
        "ABC-2" -> cookingIssue("ABC-2"),
        "ABC-3" -> cookingIssue("ABC-3"),
        "ABC-4" -> cookingIssue("ABC-4"),
        "XYZ-2" -> cookingIssue("XYZ-2"),
        "INC-1" -> cookingIssue("INC-1"),
        "INC-2" -> cookingIssue("INC-2"),
        "INC-3" -> cookingIssue("INC-3"),
        "INC-5" -> cookingIssue("INC-5"),
        "ABC-5" -> cookingIssue("ABC-5"),
        "ABC-6" -> cookingIssue("ABC-6"),
        "XYZ-3" -> cookingIssue("XYZ-3"),
        "EXC-2" -> cookingIssue("EXC-2"),
        "EXC-3" -> cookingIssue("EXC-3")
      ),
      children = Map(
        "ABC-1" -> List(cookingIssue("ABC-2")),
        "ABC-3" -> List(cookingIssue("ABC-4")),
        "ABC-5" -> List(cookingIssue("XYZ-3"), cookingIssue("ABC-6")),
        "INC-1" -> List(cookingIssue("EXC-2")),
        "INC-3" -> List(cookingIssue("EXC-2")),
        "INC-5" -> List(cookingIssue("INC-2"), cookingIssue("EXC-3"))
      ),
      changelogs = Map(
        "ABC-123" -> List(progressChangelog),
        "ABC-1" -> List(progressChangelog),
        "ABC-2" -> List(progressChangelog),
        "ABC-4" -> List(progressChangelog),
        "ABC-6" -> List(progressChangelog),
        "XYZ-2" -> List(progressChangelog),
        "XYZ-3" -> List(progressChangelog),
        "INC-1" -> List(progressChangelog),
        "INC-2" -> List(progressChangelog),
        "EXC-2" -> List(progressChangelog),
        "EXC-3" -> List(progressChangelog)
      ),
      comments = Map.empty
    )
  )
}
