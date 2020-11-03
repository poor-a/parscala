package parscala
package rewriting

import scalaz.Monoid

sealed abstract class MatchResult
case class Success(context : List[(AST.MetaVariable, tree.Expr)]) extends MatchResult
case object Failure extends MatchResult

object MatchResult {
  val monoidInstance : Monoid[MatchResult] = new Monoid[MatchResult] {
    override def zero : MatchResult = Success(List())
    override def append(a : MatchResult, b : => MatchResult) : MatchResult =
      a match {
        case Success(contextA) => 
          b match {
            case Success(contextB) => Success(contextA ++ contextB)
            case _ => Failure
          }
        case Failure => Failure
      }
  }
}

object Matcher {
  def doesPatternMatch(e : tree.Expr, pat : AST.Term) : MatchResult = {
    import MatchResult.monoidInstance.append

    (e, pat) match {
      case (tree.Literal(_, lit, _), AST.Literal(lit2)) => 
        if (lit == lit2) Success(List()) else Failure
      case (tree.Ident(_, name, _, _), AST.Ident(name2)) =>
        if (name == name2.toString) Success(List()) else Failure
      case (tree.Assign(_, lhs, rhs, _), AST.Assign(lhs2, rhs2)) =>
        append(doesPatternMatch(lhs, lhs2), (doesPatternMatch(rhs, rhs2)))
      case (_, metaVar @ AST.MetaVariable(_)) => Success(List((metaVar, e)))
    }
  }

  def lookup[K, V](key : K, l : List[(K,V)], eq : (K, K) => Boolean) : Option[V] =
    l.find{case (k, v) => eq(k, key)}.map(_._2)

  def instantiateList(pats : List[AST.Term], context : List[(AST.MetaVariable, tree.Expr)], freshLabels : Stream[SLabel]) : Option[(List[tree.Expr], Stream[SLabel])] =
    pats.foldRight(Option((List[tree.Expr](), freshLabels)))( (p, acc) => {
      for ((exprsToRight, freshLabelsN) <- acc;
           (e, freshLabelsNPlus1) <- instantiate(p, context, freshLabelsN))
      yield (e :: exprsToRight, freshLabelsNPlus1)
    })

  def eqMetaVariables(v1 : AST.MetaVariable, v2 : AST.MetaVariable) : Boolean = {
      val (AST.MetaVariable(meta.Term.Name(name1)), AST.MetaVariable(meta.Term.Name(name2))) = (v1, v2)
      name1 == name2
    }

  def instantiate(pat : AST.Term, context : List[(AST.MetaVariable, tree.Expr)], freshLabels : Stream[SLabel]) : Option[(tree.Expr, Stream[SLabel])] = {
    pat match {
      case AST.Literal(lit) => Some(tree.Literal(freshLabels.head, lit, List()), freshLabels.tail)
      case AST.Ident(name) => Some(tree.Ident(freshLabels.head, name.toString, List(), List()), freshLabels.tail)
      case metaVar @ AST.MetaVariable(_) => lookup(metaVar, context, eqMetaVariables).map((_, freshLabels))
      case AST.Assign(lhs, rhs) =>
        instantiate(lhs, context, freshLabels) match {
          case Some((l, freshLabels2)) =>
            val instantiationResult = instantiate(rhs, context, freshLabels2)
            instantiationResult.map{case (r, freshLabels3) => (tree.Assign(freshLabels3.head, l, r, List()), freshLabels3.tail)}
          case None => None
        }
      case AST.App(method, args) =>
        instantiate(method, context, freshLabels) match {
          case Some((m, freshLabels2)) =>
            instantiateList(args, context, freshLabels2).map{ case (exprs, freshLabelsN) => (tree.App(freshLabelsN.head, m, exprs, List()), freshLabelsN.tail) }
          case None => None
        }
      case AST.AppInfix(lhs, method, args) =>
        for ((lExpr, freshLabels2) <- instantiate(lhs, context, freshLabels);
             (exprs, freshLabelsN) <- instantiateList(args, context, freshLabels2))
        yield (tree.AppInfix(freshLabelsN.head, lExpr, method, exprs, List()), freshLabelsN.tail)
    }
  }
}
