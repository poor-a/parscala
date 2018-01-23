package parscala
package tree

import scalaz.Either3

/** Trees that can be used in statement context (terms, definitions). */
class Statement (val statement : Either3[Decl, Defn, Expr]) extends AnyVal {
  def label : SLabel = statement.fold(_.sLabel, _.sLabel, _.sLabel)

  def asSymbolTree : Option[SymbolTree] =
    statement.fold( decl => Some(new SymbolTree(Left(decl)))
                  , defn => Some(new SymbolTree(Right(defn)))
                  , _ => None
                  )

  def toDefn : Option[Defn] = statement.middleOr[Option[Defn]](None)(Some(_))
}
