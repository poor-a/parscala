import org.scalatest.flatspec.AnyFlatSpec

class ASTTestSuite extends AnyFlatSpec {

  val file : java.nio.file.Path = TestConfiguration.testInputDir.resolve("ASTTestInput.scala")
  "analyse" should "be able to build AST of an example Scala source file" in {
    parscala.ParScala.analyse(List(file), None) match {
      case Right((programGraph @ _, warnings)) =>
        assert(warnings.isEmpty)
      case Left(err) =>
        fail(err)
    }
  }
}
