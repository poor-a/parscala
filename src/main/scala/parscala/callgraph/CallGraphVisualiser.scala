package parscala
package callgraph

import parscala.{tree => tr}
import parscala.dot._

object CallGraphVisualiser {
  private def formatMethod(m : Either[tr.Decl.Method, tr.Defn.Method]) : DotNode = {
    val name : String = m.fold(_.symbol, _.symbol).fullName.toString
    DotNode(name) !! List(DotAttr.shape(Shape.Rectangle), DotAttr.label(name))
  }

  private def formatEdge(e : Edge) : DotEdge =
    DotEdge(formatMethod(Right(e.caller)), formatMethod(e.callee))

  def format(g : CallGraph) : DotGraph = {
    println("call graph: " + g.methods + " " + g.calls)
    val nodes : Set[DotNode] = g.methods map formatMethod
    val edges : Set[DotEdge] = g.calls map formatEdge
    DotGraph("callgraph", nodes, edges)
  }
}
