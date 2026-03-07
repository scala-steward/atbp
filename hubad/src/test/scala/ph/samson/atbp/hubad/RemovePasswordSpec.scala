package ph.samson.atbp.hubad

import better.files.File
import ph.samson.xdg.basedir.*
import zio.Task
import zio.test.*

object RemovePasswordSpec extends ZIOSpecDefault {

  def spec = suite("RemovePassword")(
    test("generate simple") {
      val result = RemovePassword.generate("pass([0-9]{4})word")
      assertTrue(result.contains("pass1890word"))
    },

    fileTest(
      "Maya.PersonalGoal.withPassword.2026-03-01.pdf"
    ) { (file, pattern) =>
      for {
        result <- RemovePassword.tryPattern(file, pattern)
      } yield assertTrue(result.getNumberOfPages > 0)
    }
  )

  val TestDir: Option[File] = {
    val path = "hubad.test.dir"
    val conf = config("atbp").load
    if (conf.hasPath(path)) {
      Some(File(conf.getString(path)))
    } else {
      None
    }
  }
  val TestPattern: Option[String] = {
    val path = "hubad.test.pattern"
    val conf = config("atbp").load
    if (conf.hasPath(path)) {

      Some(conf.getString(path))
    } else { None }
  }

  def fileTest(
      statementFileName: String
  )(assertion: (File, String) => Task[TestResult]): Spec[Any, Throwable] = {
    (TestDir, TestPattern) match {
      case (Some(testDir), Some(testPattern)) =>
        val file = testDir / statementFileName
        test(s"hubad $statementFileName") {
          assertion(file, testPattern)
        }
      case (None, Some(_)) =>
        test(s"missing test file $statementFileName") {
          assertTrue(true)
        } @@ TestAspect.ignore
      case (_, None) =>
        test(s"missing test pattern $statementFileName") {
          assertTrue(true)
        } @@ TestAspect.ignore
    }
  }
}
