import org.scalatest.FunSuite

import parscala.SLabel
import parscala.controlflow.CFGraph
import parscala.tree.{Block, While, If, PatDef, IdentPat}
import parscala.{tree => tr}
import parscala.df.LiveVariablesAnalysis

class LivenessVariablesSuite extends FunSuite {
  import parscala.compiler

  val run : compiler.Run = new compiler.Run()
  compiler.phase = run.typerPhase

  val ast : tr.NodeTree = tr.Node.fromTree(
    parscala.ParScala.astOfExpr("""
      var x = 5
      var y = 1
      var w = 2
      var z = 0
      var a = 4
      while (x > 0) {
        y = y * x
        x = x - 1
        w = 1
      }
      if (y > 1)
        a = a + 2
    """
    )
  )

  val Block(_, ls@List(xdef, ydef, wdef, zdef, adef, loop, ifExpr), _) = ast.root
  val While(_, pred, Block(_, List(xass, yass, wass), _), _) = loop
  val If(_, ifPred, aass, _) = ifExpr

  val PatDef(_, IdentPat(_, x), _, _) = xdef
  val PatDef(_, IdentPat(_, y), _, _) = ydef
  val PatDef(_, IdentPat(_, w), _, _) = wdef
  val PatDef(_, IdentPat(_, z), _, _) = zdef
  val PatDef(_, IdentPat(_, a), _, _) = adef

  val live : Map[SLabel, Set[LiveVariablesAnalysis.LV]] = Map(
      xdef.label   -> Set.empty[LiveVariablesAnalysis.LV]
    , ydef.label   -> Set(x)
    , wdef.label   -> Set(x, y)
    , zdef.label   -> Set(x, y)
    , adef.label   -> Set(x, y)
    , pred.label   -> Set(x, y, a)
    , xass.label   -> Set(x, y, a)
    , yass.label   -> Set(x, y, a)
    , wass.label   -> Set(x, y, a)
    , ifPred.label -> Set(y, a)
    , aass.label   -> Set(a)
  )

  println(ls map (_.label))

  val lva : LiveVariablesAnalysis = LiveVariablesAnalysis.fromCFGraph(CFGraph.fromExpression(ast))

  for ((k, v) <- live)
    assertResult(Some(v), k)(lva.get(k))
}
