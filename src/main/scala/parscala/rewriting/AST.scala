package parscala
package rewriting

import scala.meta

object AST {
  case class RewriteRule[P <: Pattern](matchingPattern : P, replacementPattern : P)

  sealed trait Pattern

  sealed trait Statement extends Pattern
  sealed trait Term extends Statement

//  case class Meta[S](ast : S) extends Pattern[S]

  case object Wildcard extends Term
  case class Literal(lit : meta.Lit) extends Term
  case class Ident(ident : meta.Term.Name) extends Term
  case class MetaVariable(name : meta.Term.Name) extends Term {
    def this(name : String) = this(meta.Term.Name(name))
  }
  case class Assign(lhs : Term, rhs : Term) extends Term
  case class App(method : Term, args : List[Term]) extends Term
  case class AppInfix(lhs : Term, method : meta.Term.Name, args : List[Term]) extends Term
  case class Block(terms : List[Term]) extends Term
}
