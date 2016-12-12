package parscala
package controlflow

class ControlDependency(val g : CFGraph, val entry : BLabel, val edges : List[(BLabel, BLabel, EdgeLabel.TagType)]) {
  def dependsOn(l : BLabel) : List[BLabel] =
    for ((s,t,_) <- edges if t == l) yield s

  def controls(l : BLabel) : List[BLabel] = 
    for ((s,t,_) <- edges if s == l) yield t

  override def equals(o : Any) : Boolean = 
    o match {
      case cd : ControlDependency => entry == cd.entry && edges.toSet == cd.edges.toSet
      case _ => false
    }

  override def toString : String =
    "ControlDependency(%s, %s, %s)".format(g, entry, edges)
}
