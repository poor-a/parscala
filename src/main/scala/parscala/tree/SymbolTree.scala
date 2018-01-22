package parscala
package tree

/** Trees that introduce symbols (declarations and definitions). */
trait SymbolTree extends Statement {
  def label : DLabel
}

object SymbolTree {
  def kindCata[A]( defn : Defn => A
                 , decl : Decl => A
                 , t : SymbolTree
                 ) : A =
    t match {
      case d : Defn => defn(d)
      case d : Decl => decl(d)
    }
}
