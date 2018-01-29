package parscala
package tree

import scalaz.Either3

/** Trees that can be used in statement context (terms, definitions). */
class Statement (val statement : Either3[Decl, Defn, Expr]) extends AnyVal {
  def label : Option[SLabel] = {
    val c1None : (Any) => Option[SLabel] = Function.const(None)
    statement.fold(c1None, c1None, expr => Some(expr.label))
  }

  def asSymbolTree : Option[SymbolTree] =
    statement.fold( decl => Some(new SymbolTree(Left(decl)))
                  , defn => Some(new SymbolTree(Right(defn)))
                  , _ => None
                  )

  def toDefn : Option[Defn] = statement.middleOr[Option[Defn]](None)(Some(_))

  def fold[A] : (Decl => A, Defn => A, Expr => A) => A = statement.fold _
}
