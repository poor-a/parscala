package parscala
package callgraph

import parscala.tree.Method
import parscala.dot._

object CallGraphVisualiser {
  private def formatMethod(m : Method) : DotNode = {
    val name = m.symbol.fullName.toString
    DotNode(name) !! List(DotAttr.shape("rectangle"), DotAttr.label(name))
  }

  private def formatEdge(e : Edge) : DotEdge =
    DotEdge(formatMethod(e.caller), formatMethod(e.callee))

  def format(g : CallGraph) : DotGraph = {
    println("call graph: " + g.methods + " " + g.calls)
    val nodes : Set[DotNode] = g.methods map formatMethod
    val edges : Set[DotEdge] = g.calls map formatEdge
    DotGraph("callgraph", nodes, edges)
  }
}
