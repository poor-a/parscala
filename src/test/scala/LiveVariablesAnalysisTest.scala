import org.scalatest.flatspec.AnyFlatSpec

import parscala.SLabel
import parscala.controlflow.CFGraph
import parscala.tree.{Block, While, If}
import parscala.tree.Defn.Var
import parscala.{tree => tr}
import parscala.df.LiveVariablesAnalysis

class LiveVariablesAnalysisTestSuite extends AnyFlatSpec {
  behavior of "LiveVariablesAnalysis"

  val testInput : java.nio.file.Path = TestConfiguration.testInputDir.resolve("LiveVariablesAnalysisTestInput.scala")

  parscala.ParScala.analyse(List(testInput), None) match {
    case Right((pgraph, warnings @ _)) =>
      val Some(obj) = pgraph.objects.find(_.name == meta.Term.Name("LiveVariablesAnalysisTestInput"))
      val List(Right(f)) = obj.methods

      val Block(_, List(xdef_, ydef_, wdef_, zdef_, adef_, loop_, ifExpr_), _) = f.body
      val Some(While(_, pred, Block(_, loopBody, _), _)) = loop_.toExpr
      val List(xass, yass, wass) = loopBody.map(_.toExpr.get)
      val Some(If(_, ifPred, aass, _)) = ifExpr_.toExpr

      val Some(Var(_, _, _, List(x), _, x_oRhs)) = xdef_.toDefn
      val Some(Var(_, _, _, List(y), _, y_oRhs)) = ydef_.toDefn
      val Some(Var(_, _, _, _, _, w_oRhs)) = wdef_.toDefn
      val Some(Var(_, _, _, _, _, z_oRhs)) = zdef_.toDefn
      val Some(Var(_, _, _, List(a), _, a_oRhs)) = adef_.toDefn

      val live : Map[(SLabel, String), Set[LiveVariablesAnalysis.LV]] = Map(
          (x_oRhs.get.label, "xdef")     -> Set(x)
        , (y_oRhs.get.label, "ydef")     -> Set(x, y)
        , (w_oRhs.get.label, "wdef")     -> Set(x, y)
        , (z_oRhs.get.label, "zdef")     -> Set(x, y)
        , (a_oRhs.get.label, "adef")     -> Set(x, y, a)
        , (pred.label, "pred")     -> Set(x, y, a)
        , (xass.label, "xass")     -> Set(x, y, a)
        , (yass.label, "yass")     -> Set(x, y, a)
        , (wass.label, "wass")     -> Set(x, y, a)
        , (ifPred.label, "ifPred") -> Set(a)
        , (aass.label, "aass")     -> Set()
      )

      val cfg : CFGraph = CFGraph.fromMethod(f, pgraph)
      val lva : LiveVariablesAnalysis = LiveVariablesAnalysis.fromCFGraph(cfg)

      it should "correctly discover live variables in a test source file" in {
        for (((k, name), v) <- live)
          assertResult(Some(v), name)(lva.get(k))
      }
    case _ =>
      fail("could not create a ProgramGraph from a source file")
  }
}
