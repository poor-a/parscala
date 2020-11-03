package parscala
package rewriting

import scala.meta

object AST {
  case class RewriteRule[S](matchingPattern : Pattern[S], replacementPattern : Pattern[S])

  sealed abstract class Pattern[S]

  sealed abstract class Term extends Pattern[Term]

//  case class Meta[S](ast : S) extends Pattern[S]

  case class Literal(lit : meta.Lit) extends Term
  case class Ident(ident : meta.Term.Name) extends Term
  case class MetaVariable(name : meta.Term.Name) extends Term
  case class Assign(lhs : Term, rhs : Term) extends Term
  case class App(method : Term, args : List[Term]) extends Term
  case class AppInfix(lhs : Term, method : meta.Term.Name, args : List[Term]) extends Term
}
