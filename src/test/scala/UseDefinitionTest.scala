import org.scalatest.flatspec.AnyFlatSpec

import parscala.SLabel
import parscala.controlflow.CFGraph
import parscala.tree.{Block, AppInfix}
import parscala.tree.Defn.Var
import parscala.df.UseDefinition

class UseDefinitionTestSuite extends AnyFlatSpec {
  behavior of "Use-definition analysis"

  val testInput : java.nio.file.Path = TestConfiguration.testInputDir.resolve("UseDefinitionTestInput.scala")

  parscala.ParScala.analyse(List(testInput), None) match {
    case Right((pgraph, warnings @ _)) =>
      val Some(o) = pgraph.objects.find(_.name == meta.Term.Name("UseDefinitionTestInput"))
      val List(Right(f)) = o.methods
      val Block(_, List(xdef_, res_), _) = f.body

      val Some(xdef @ Var(_, _, _, List(_), _, x_oRhs)) = xdef_.toDefn
      val Some(AppInfix(_, xref, _, _, _)) = res_.toExpr

      val usedefs : Map[(SLabel, String), Set[UseDefinition.Assignment]] = Map(
          (x_oRhs.get.label, "xdef") -> Set()
        , (xref.label, "xref")       -> Set(Left(xdef.label))
      )

      val cfg : CFGraph = CFGraph.fromMethod(f, pgraph)
      val ud : UseDefinition = UseDefinition.fromCFGraph(cfg)

      it should "correctly discover use-definition relationships over a control flow graph" in {
        for (((k, name), v) <- usedefs)
          assertResult(v, name)(ud(k))
      }
    case _ =>
      fail("could not create a ProgramGraph from a source file")
  }
}
