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
    val methods : Set[Method] = g.calls.foldLeft(Set.empty[Method]){(acc, call) => acc + call.caller + call.callee}
    val nodes : Set[DotNode] = methods map formatMethod
    val edges : List[DotEdge] = g.calls map formatEdge
    DotGraph("callgraph", nodes, edges)
  }
}
