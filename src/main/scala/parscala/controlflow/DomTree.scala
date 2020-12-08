package parscala
package controlflow

class DomTree(val tree : Map[BLabel, Option[BLabel]]) {
  val root : Option[BLabel] =
    tree find {case (_, parent) => parent.isEmpty} map (_._1)

  def parent(l : BLabel) : Option[BLabel] =
    tree find {case (child, parent @ _) => child == l} map (_._2) getOrElse None

  def children(l : BLabel) : Iterable[BLabel] = {
    for ((child, Some(parent)) <- tree if (parent == l)) yield child
  }

  def postOrder[A](f : (BLabel, A) => A, x : A) : A = {
    def doTraverse(y : A, l : BLabel) : A =
      f(l, children(l).foldLeft(y)(doTraverse _))

    root map (doTraverse(x, _)) getOrElse x
  }

  override def equals(o : Any) : Boolean =
    o match {
      case t : DomTree => tree == t.tree
      case _ => false
    }

  override def toString : String = "DomTree(" + tree.toString + ")"
}
