import org.scalatest.FunSuite

import parscala.SLabel
import parscala.controlflow.CFGraph
import parscala.tree.{Block, Select, App, PatDef, IdentPat}
import parscala.{tree => tr}
import parscala.df.UseDefinition

class UseDefinitionSuite extends FunSuite {
  import parscala.compiler

  val run : compiler.Run = new compiler.Run()
  compiler.phase = run.typerPhase

  val Some(ast) : Option[compiler.Tree] = parscala.ParScala.astOfExpr("""
    var x = 5 + 1
    x + 1
  """
  )

  tr.Node.fromTree(ast) match {
    case (pgraph, Some(programtree)) => 
      val Block(_, List(xdef, res), _) = programtree.root

      val PatDef(_, IdentPat(_, x), _, _) = xdef
      val App(_, Select(_, xref, _, _), _, _, _) = res

      val usedefs : Map[(SLabel, String), Set[UseDefinition.Assignment]] = Map(
          (xdef.label, "xdef")     -> Set()
        , (xref.label, "xref")     -> Set((x, xdef.label))
      )

      val cfg : CFGraph = CFGraph.fromExpression(programtree.root, pgraph)
      val ud : UseDefinition = UseDefinition.fromCFGraph(cfg)

      for (((k, name), v) <- usedefs)
        assertResult(v, name)(ud(k))
    case _ =>
      fail("Could not create a NodeTree")
  }
}
