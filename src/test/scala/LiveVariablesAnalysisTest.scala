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

  val Some(ast) : Option[compiler.Tree] = parscala.ParScala.astOfExpr("""
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
  val programtree : tr.NodeTree = tr.Node.fromTree(ast)

  val Block(_, ls@List(xdef, ydef, wdef, zdef, adef, loop, ifExpr), _) = programtree.root
  val While(_, pred, Block(_, as@List(xass, yass, wass), _), _) = loop
  val If(_, ifPred, aass, _) = ifExpr

  val PatDef(_, IdentPat(_, x), _, _) = xdef
  val PatDef(_, IdentPat(_, y), _, _) = ydef
  val PatDef(_, IdentPat(_, w), _, _) = wdef
  val PatDef(_, IdentPat(_, z), _, _) = zdef
  val PatDef(_, IdentPat(_, a), _, _) = adef

  val live : Map[(SLabel, String), Set[LiveVariablesAnalysis.LV]] = Map(
      (xdef.label, "xdef")     -> Set(x)
    , (ydef.label, "ydef")     -> Set(x, y)
    , (wdef.label, "wdef")     -> Set(x, y)
    , (zdef.label, "zdef")     -> Set(x, y)
    , (adef.label, "adef")     -> Set(x, y, a)
    , (pred.label, "pred")     -> Set(x, y, a)
    , (xass.label, "xass")     -> Set(x, y, a)
    , (yass.label, "yass")     -> Set(x, y, a)
    , (wass.label, "wass")     -> Set(x, y, a)
    , (ifPred.label, "ifPred") -> Set(a)
    , (aass.label, "aass")     -> Set()
  )

  val cfg : CFGraph = CFGraph.fromExpression(programtree)
  val lva : LiveVariablesAnalysis = LiveVariablesAnalysis.fromCFGraph(cfg)

  for (((k, name), v) <- live)
    assertResult(Some(v), name)(lva.get(k))
}
