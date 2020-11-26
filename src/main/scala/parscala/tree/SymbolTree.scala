package parscala
package tree

/** Trees that introduce symbols (declarations and definitions). */
class SymbolTree[IdentInfo, SemanticInfo] (val tree : Either[Decl[IdentInfo, SemanticInfo], Defn[IdentInfo, SemanticInfo]]) extends AnyVal {
  def label : DLabel = tree.fold(_.label, _.label)

  def unSymbolTree : Either[Decl[IdentInfo, SemanticInfo], Defn[IdentInfo, SemanticInfo]] = tree
}
