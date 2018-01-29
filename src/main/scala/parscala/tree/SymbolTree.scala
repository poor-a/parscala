package parscala
package tree

/** Trees that introduce symbols (declarations and definitions). */
class SymbolTree (val tree : Either[Decl, Defn]) extends AnyVal {
  def label : DLabel = tree.fold(_.label, _.label)

  def unSymbolTree : Either[Decl, Defn] = tree
}
