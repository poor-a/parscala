package parscala
package callgraph

import parscala.{tree => tr}
import parscala.dot._

object CallGraphVisualiser {
  private def formatMethod(m : Either[tr.Decl.TypedMethod, tr.Defn.TypedMethod]) : DotNode = {
    val name : String = m.fold(_.symbols, _.symbols).map(_.fullName).toString
    DotNode(name) !! List(DotAttr.shape(Shape.Rectangle), DotAttr.label(name))
  }

  private def formatExpr(expr : tr.TypedExpr) : DotNode =
    DotNode.record(expr.label, "Expression", expr.toString)

  private def formatEdge(e : Edge) : DotEdge = {
    val callee : DotNode = e.callee.fold(
        (m : tr.Decl.TypedMethod) => formatMethod(Left(m))  // method definition
      , (m : tr.Defn.TypedMethod) => formatMethod(Right(m)) // method declaration
      , (e : tr.TypedExpr) => formatExpr(e)                 // expression
    )
    DotEdge(formatMethod(Right(e.caller)), callee)
  }

  def format(g : CallGraph) : DotGraph = {
    println("call graph: " + g.methods + " " + g.calls)
    val nodes : Set[DotNode] = g.methods map formatMethod
    val edges : Set[DotEdge] = g.calls map formatEdge
    DotGraph("callgraph", nodes, edges)
  }
}
