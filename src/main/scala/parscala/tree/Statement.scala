package parscala
package tree

import scalaz.Either3

import org.typelevel.paiges.Doc

/** Trees that can be used in statement context (terms, definitions). */
class Statement[IdentInfo, SemanticInfo] (val statement : Either3[Decl[IdentInfo, SemanticInfo], Defn[IdentInfo, SemanticInfo], Expr[IdentInfo, SemanticInfo]]) extends AnyVal {
  def label : Option[SLabel] = {
    val c1None : (Any) => Option[SLabel] = Function.const(None)
    statement.fold(c1None, c1None, expr => Some(expr.label))
  }

  def asSymbolTree : Option[SymbolTree[IdentInfo, SemanticInfo]] =
    statement.fold( (decl : Decl[IdentInfo, SemanticInfo]) => Some(new SymbolTree(Left(decl)))
                  , (defn : Defn[IdentInfo, SemanticInfo]) => Some(new SymbolTree(Right(defn)))
                  , (_ : Expr[IdentInfo, SemanticInfo]) => None
                  )

  def toDefn : Option[Defn[IdentInfo, SemanticInfo]] = statement.middleOr[Option[Defn[IdentInfo, SemanticInfo]]](None)(Some(_))
  def toExpr : Option[Expr[IdentInfo, SemanticInfo]] = statement.rightOr[Option[Expr[IdentInfo, SemanticInfo]]](None)(Some(_))

  def fold[A] : (Decl[IdentInfo, SemanticInfo] => A, Defn[IdentInfo, SemanticInfo] => A, Expr[IdentInfo, SemanticInfo] => A) => A = statement.fold _
}

object Statement {
  def fromDecl[IdentInfo, SemanticInfo](d : Decl[IdentInfo, SemanticInfo]) : Statement[IdentInfo, SemanticInfo] = new Statement(scalaz.Either3.left3(d))

  def fromDefn[IdentInfo, SemanticInfo](d : Defn[IdentInfo, SemanticInfo]) : Statement[IdentInfo, SemanticInfo] = new Statement(scalaz.Either3.middle3(d))

  def fromExpr[IdentInfo, SemanticInfo](e : Expr[IdentInfo, SemanticInfo]) : Statement[IdentInfo, SemanticInfo] = new Statement(scalaz.Either3.right3(e))

  def prettyPrint[IdentInfo, SemanticInfo](stmt : Statement[IdentInfo, SemanticInfo]) : Doc =
    stmt.fold(Decl.prettyPrint, Defn.prettyPrint, Expr.prettyPrint)

  def prettyPrintLint(stmt : TypedStatement) : Doc =
    stmt.fold(Decl.prettyPrint, Defn.prettyPrintLint, Expr.prettyPrintLint)
}
