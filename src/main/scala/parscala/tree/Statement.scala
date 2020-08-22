package parscala
package tree

import scalaz.Either3

import org.typelevel.paiges.Doc

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

object Statement {
  def fromDecl(d : Decl) : Statement = new Statement(scalaz.Either3.left3(d))

  def fromDefn(d : Defn) : Statement = new Statement(scalaz.Either3.middle3(d))

  def fromExpr(e : Expr) : Statement = new Statement(scalaz.Either3.right3(e))

  def prettyPrint(stmt : Statement) : Doc =
    stmt.fold(Decl.prettyPrint, Defn.prettyPrint, Expr.prettyPrint)

  def prettyPrintLint(stmt : Statement) : Doc =
    stmt.fold(Decl.prettyPrint, Defn.prettyPrintLint, Expr.prettyPrintLint)
}
