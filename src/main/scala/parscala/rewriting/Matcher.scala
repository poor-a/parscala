package parscala
package rewriting

import scalaz.Monoid

sealed abstract class MatchResult
case class Success(context : List[(AST.MetaVariable, tree.Expr[_, _])]) extends MatchResult
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
  def doesPatternMatch(e : tree.Expr[_, _], pat : AST.Term) : MatchResult = {
    import MatchResult.monoidInstance.append

    (e, pat) match {
      case (tree.Literal(_, lit, _), AST.Literal(lit2)) => 
        if (lit == lit2) Success(List()) else Failure
      case (tree.Ident(_, name, _, _), AST.Ident(name2)) =>
        if (name == name2.toString) Success(List()) else Failure
      case (tree.Assign(_, lhs, rhs, _), AST.Assign(lhs2, rhs2)) =>
        append(doesPatternMatch(lhs, lhs2), doesPatternMatch(rhs, rhs2))
      case (_, metaVar @ AST.MetaVariable(_)) => Success(List((metaVar, e)))
    }
  }

  def lookup[K, V](key : K, l : List[(K,V)], eq : (K, K) => Boolean) : Option[V] =
    l.find{case (k, v) => eq(k, key)}.map(_._2)

  def instantiateList(pats : List[AST.Term], context : List[(AST.MetaVariable, tree.Expr[_, _])], freshLabels : Stream[SLabel]) : Option[(List[tree.Expr[Unit, Unit]], Stream[SLabel])] =
    pats.foldRight(Option((List[tree.Expr[Unit, Unit]](), freshLabels)))( (p, acc) => {
      for ((exprsToRight, freshLabelsN) <- acc;
           (e, freshLabelsNPlus1) <- instantiate(p, context, freshLabelsN))
      yield (e :: exprsToRight, freshLabelsNPlus1)
    })

  def eqMetaVariables(v1 : AST.MetaVariable, v2 : AST.MetaVariable) : Boolean = {
      val (AST.MetaVariable(meta.Term.Name(name1)), AST.MetaVariable(meta.Term.Name(name2))) = (v1, v2)
      name1 == name2
    }

  def eraseSemanticInfo[IdentInfo, SemanticInfo](e : tree.Expr[IdentInfo, SemanticInfo]) : tree.Expr[Unit, Unit] =
    e.cata(
        (l, lit, _) => // literal
          tree.Literal(l, lit, ())
      , (l, ident, _, _) => // identifier reference
          tree.Ident(l, ident, List(), ())
      , (l, lhs, rhs, _) => // assignment
          tree.Assign(l, eraseSemanticInfo(lhs), eraseSemanticInfo(rhs), ())
      , (l, m, args, _) => // application
          tree.App(l, eraseSemanticInfo(m), args.map(eraseSemanticInfo(_)), ())
      , (l, lhs, op, args, _) => // infix application
          tree.AppInfix(l, eraseSemanticInfo(lhs), op, args.map(eraseSemanticInfo(_)), ())
      , (l, op, arg, _) => // unary application
          tree.AppUnary(l, op, eraseSemanticInfo(arg), ())
      , (l, class_, argss, _) => // new
          tree.New(l, class_, argss.map(args => args.map(eraseSemanticInfo(_))), ())
      , (l, obj, termName, _, _) => // selection
          tree.Select(l, eraseSemanticInfo(obj), termName, (), ())
      , (l, argss) => // this(...) application
          tree.ThisApply(l, argss.map(args => args.map(eraseSemanticInfo(_))))
      , (l, typeName, _) => // this
          tree.This(l, typeName, ())
      , (l, thisp, superp, _) => // super
          tree.Super(l, thisp, superp, ())
      , (l, comps, _) => // tuple
          tree.Tuple(l, comps.map(eraseSemanticInfo(_)), ())
      , (l, pred, thenE, _) => // if-then
          tree.If(l, eraseSemanticInfo(pred), eraseSemanticInfo(thenE), ())
      , (l, pred, thenE, elseE, _) => // if-then-else
          tree.IfElse(l, eraseSemanticInfo(pred), eraseSemanticInfo(thenE), eraseSemanticInfo(elseE), ())
      , (l, pred, body, _) => // while loop
          tree.While(l, eraseSemanticInfo(pred), eraseSemanticInfo(body), ())
      , (l, enums, body, _) => // for loop
          tree.For(l, enums, eraseSemanticInfo(body), ())
      , (l, enums, body, _t) => // for-yield loop
          tree.ForYield(l, enums, eraseSemanticInfo(body), ())
      , (l, _) => // return
          tree.ReturnUnit(l, ())
      , (l, expr, _) => // return with expr
          tree.Return(l, eraseSemanticInfo(expr), ())
      , (l, expr, _) => // throw
          tree.Throw(l, eraseSemanticInfo(expr), ())
      , (l, stmts, _) => // block
          tree.Block(l, stmts.map(eraseSemanticInfo(_)), ())
      , (l, expr, _) => // other expression
          tree.Other(l, expr, ())
      )

  def eraseSemanticInfo[IdentInfo, SemanticInfo](decl : tree.Decl[IdentInfo, SemanticInfo]) : tree.Decl[Unit, Unit] = ???

  def eraseSemanticInfo[IdentInfo, SemanticInfo](decl : tree.Defn[IdentInfo, SemanticInfo]) : tree.Defn[Unit, Unit] = ???

  def eraseSemanticInfo[IdentInfo, SemanticInfo](stmt : tree.Statement[IdentInfo, SemanticInfo]) : tree.Statement[Unit, Unit] =
    stmt.fold(
       (decl : tree.Decl[IdentInfo, SemanticInfo]) => tree.Statement.fromDecl(eraseSemanticInfo(decl))
     , (defn : tree.Defn[IdentInfo, SemanticInfo]) => tree.Statement.fromDefn(eraseSemanticInfo(defn))
     , (expr : tree.Expr[IdentInfo, SemanticInfo]) => tree.Statement.fromExpr(eraseSemanticInfo(expr))
     )

  def instantiate(pat : AST.Term, context : List[(AST.MetaVariable, tree.Expr[_, _])], freshLabels : Stream[SLabel]) : Option[(tree.Expr[Unit, Unit], Stream[SLabel])] = {
    pat match {
      case AST.Literal(lit) => Some(tree.Literal(freshLabels.head, lit, ()), freshLabels.tail)
      case AST.Ident(name) => Some(tree.Ident(freshLabels.head, name.toString, List(), ()), freshLabels.tail)
      case metaVar @ AST.MetaVariable(_) => lookup(metaVar, context, eqMetaVariables).map(e => (eraseSemanticInfo(e), freshLabels))
      case AST.Assign(lhs, rhs) =>
        instantiate(lhs, context, freshLabels) match {
          case Some((l, freshLabels2)) =>
            val instantiationResult = instantiate(rhs, context, freshLabels2)
            instantiationResult.map{case (r, freshLabels3) => (tree.Assign(freshLabels3.head, l, r, ()), freshLabels3.tail)}
          case None => None
        }
      case AST.App(method, args) =>
        instantiate(method, context, freshLabels) match {
          case Some((m, freshLabels2)) =>
            instantiateList(args, context, freshLabels2).map{ case (exprs, freshLabelsN) => (tree.App(freshLabelsN.head, m, exprs, ()), freshLabelsN.tail) }
          case None => None
        }
      case AST.AppInfix(lhs, method, args) =>
        for ((lExpr, freshLabels2) <- instantiate(lhs, context, freshLabels);
             (exprs, freshLabelsN) <- instantiateList(args, context, freshLabels2))
        yield (tree.AppInfix(freshLabelsN.head, lExpr, method, exprs, ()), freshLabelsN.tail)
    }
  }
}
