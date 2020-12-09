package parscala
package rewriting

import scalaz.Monoid

sealed trait MatchResult
case class Success(context : List[(AST.MetaVariable, Value)]) extends MatchResult
case object Failure extends MatchResult

sealed abstract class Value {
  def cata[A]( decl : tree.TypelessDecl => A
             , defn : tree.TypelessDefn => A
             , expr : tree.TypelessExpr => A
             ) : A =
    this match {
      case Decl(d) => decl(d)
      case Defn(d) => defn(d)
      case Expr(e) => expr(e)
    }
}
case class Decl(decl : tree.TypelessDecl) extends Value
case class Defn(defn : tree.TypelessDefn) extends Value
case class Expr(expr : tree.TypelessExpr) extends Value

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
      case (tree.Block(_, stmts, _), AST.Block(stmtPats)) =>
        doesPatternMatchBlock(stmts, stmtPats)
      case (_, metaVar @ AST.MetaVariable(_)) => Success(List((metaVar, Expr(eraseSemanticInfo(e)))))
      case (_, AST.Wildcard) => Success(List())
      case (_, _) => Failure
    }
  }

  def doesPatternMatch(stmt : tree.Statement[_, _], pat : AST.Statement) : MatchResult =
    stmt.fold(
        (d : tree.Decl[_, _]) =>
          pat match {
            case metaVar @ AST.MetaVariable(_) => Success(List((metaVar, Decl(eraseSemanticInfo(d)))))
            case AST.Wildcard => Success(List())
            case _ => Failure
          }
      , (d : tree.Defn[_, _]) =>
          pat match {
            case metaVar @ AST.MetaVariable(_) => Success(List((metaVar, Defn(eraseSemanticInfo(d)))))
            case AST.Wildcard => Success(List())
            case _ => Failure
          }
      , (e : tree.Expr[_, _]) =>
          pat match {
            case p : AST.Term => doesPatternMatch(e, p)
            case _ => Failure
          }
      )

  def doesPatternMatchBlock(stmts : List[tree.Statement[_, _]], pats : List[AST.Statement]) : MatchResult = {
    import MatchResult.monoidInstance.append
    Control.zipWith(doesPatternMatch((_ : tree.Statement[_, _]), (_ : AST.Statement)), stmts, pats).fold(Success(List()))((mr1 : MatchResult, mr2 : MatchResult) => append(mr1, mr2))
  }

  def eraseSemanticInfo[IdentInfo, SemanticInfo](e : tree.ThisApply[IdentInfo, SemanticInfo]) : tree.ThisApply[Unit, Unit] =
    e.copy(argss = e.argss.map(_.map(eraseSemanticInfo(_))))

  def eraseSemanticInfo[IdentInfo, SemanticInfo](e : tree.Expr[IdentInfo, SemanticInfo]) : tree.TypelessExpr =
    e.cata(
        (l, lit, _) => // literal
          tree.Literal(l, lit, ())
      , (l, ident, _, _) => // identifier reference
          tree.Ident(l, ident, (), ())
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
      , (l, enums, body, _) => // for-yield loop
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

  def eraseSemanticInfo[IdentInfo, SemanticInfo](decl : tree.Decl[IdentInfo, SemanticInfo]) : tree.TypelessDecl =
    decl.cata(
        (l, mods, pats, _, declType) => // val
          tree.Decl.Val(l, mods, pats, List(), declType)
      , (l, mods, pats, _, declType) => // var
          tree.Decl.Var(l, mods, pats, List(), declType)
      , (l, _, mods, name, typeArgs, argss, declType) => // method
          tree.Decl.Method(l, List(), mods, name, typeArgs, argss, declType)
      , (l, _, mods, name, params, bounds) => // type
          tree.Decl.Type(l, List(), mods, name, params, bounds)
      , (l, imports) => // import
          tree.Decl.Import(l, imports)
      )

  def eraseSemanticInfo[IdentInfo, SemanticInfo](defn : tree.Defn[IdentInfo, SemanticInfo]) : tree.TypelessDefn =
    defn.cata(
       (l, mods, pats, _, oDeclType, rhs) => // val
         tree.Defn.Val(l, mods, pats, List(), oDeclType, eraseSemanticInfo(rhs))
     , (l, mods, pats, _, oDeclType, oRhs) => // var
         tree.Defn.Var(l, mods, pats, List(), oDeclType, oRhs.map(eraseSemanticInfo(_)))
     , (l, _, mods, name, typeArgs, argss, oDeclType, body) => // method
         tree.Defn.Method(l, List(), mods, name, typeArgs, argss, oDeclType, eraseSemanticInfo(body))
     , (l, _, mods, name, params, body) => // type
         tree.Defn.Type(l, List(), mods, name, params, body)
     , (l, _, mods, name, argss, body) => // macro
         tree.Defn.Macro(l, List(), mods, name, argss, eraseSemanticInfo(body))
     , (l, _, mods, name, paramss, body) => { // secondary constructor
         val (initializer, stmts) = body
         tree.Defn.SecondaryCtor(l, List(), mods, name, paramss, (eraseSemanticInfo(initializer), stmts.map(eraseSemanticInfo(_))))
       }
     , (l, _, mods, name, statements) => // class
         tree.Defn.Class(l, List(), mods, name, statements.map(eraseSemanticInfo(_)))
     , (l, _, mods, name, statements) => // trait
         tree.Defn.Trait(l, List(), mods, name, statements.map(eraseSemanticInfo(_)))
     , (l, _, mods, name, statements) => // object
         tree.Defn.Object(l, List(), mods, name, statements.map(eraseSemanticInfo(_)))
     , (l, _, mods, name, statements) => // package object
         tree.Defn.PackageObject(l, List(), mods, name, statements.map(eraseSemanticInfo(_)))
     , (l, _, name, statements) => // package
         tree.Defn.Package(l, List(), name, statements.map(eraseSemanticInfo(_)))
     )

  def eraseSemanticInfo[IdentInfo, SemanticInfo](stmt : tree.Statement[IdentInfo, SemanticInfo]) : tree.TypelessStatement =
    stmt.fold(
       (decl : tree.Decl[IdentInfo, SemanticInfo]) => tree.Statement.fromDecl(eraseSemanticInfo(decl))
     , (defn : tree.Defn[IdentInfo, SemanticInfo]) => tree.Statement.fromDefn(eraseSemanticInfo(defn))
     , (expr : tree.Expr[IdentInfo, SemanticInfo]) => tree.Statement.fromExpr(eraseSemanticInfo(expr))
     )

  def lookup[K, V](key : K, l : List[(K,V)], eq : (K, K) => Boolean) : Option[V] =
    l.collectFirst{case (k, v) if eq(k, key) => v}

  def eqMetaVariables(v1 : AST.MetaVariable, v2 : AST.MetaVariable) : Boolean = {
      val (AST.MetaVariable(meta.Term.Name(name1)), AST.MetaVariable(meta.Term.Name(name2))) = (v1, v2)
      name1 == name2
    }

  def instantiateStmts(pats : List[AST.Statement], context : List[(AST.MetaVariable, Value)], freshLabels : Stream[SLabel]) : Option[(List[tree.TypelessStatement], Stream[SLabel])] =
    pats.foldRight(Option((List[tree.TypelessStatement](), freshLabels)))( (p, acc) =>
      for ((stmtsToRight, freshLabelsN) <- acc;
           (stmt, freshLabelsNPlus1) <- instantiateStmt(p, context, freshLabelsN))
      yield (stmt :: stmtsToRight, freshLabelsNPlus1)
    )

  def instantiateStmt(pat : AST.Statement, context : List[(AST.MetaVariable, Value)], freshLabels : Stream[SLabel]) : Option[(tree.TypelessStatement, Stream[SLabel])] =
    pat match {
      case p : AST.MetaVariable =>
        for (v <- lookup(p, context, eqMetaVariables))
        yield (v.cata(tree.Statement.fromDecl(_), tree.Statement.fromDefn(_), tree.Statement.fromExpr(_)), freshLabels)
      case p : AST.Term =>
        for ((e, freshLabels2) <- instantiate(p, context, freshLabels))
        yield (tree.Statement.fromExpr(e), freshLabels2)
    }

  def instantiateList(pats : List[AST.Term], context : List[(AST.MetaVariable, Value)], freshLabels : Stream[SLabel]) : Option[(List[tree.TypelessExpr], Stream[SLabel])] =
    pats.foldRight(Option((List[tree.TypelessExpr](), freshLabels)))( (p, acc) =>
      for ((exprsToRight, freshLabelsN) <- acc;
           (e, freshLabelsNPlus1) <- instantiate(p, context, freshLabelsN))
      yield (e :: exprsToRight, freshLabelsNPlus1)
    )

  def instantiate(pat : AST.Term, context : List[(AST.MetaVariable, Value)], freshLabels : Stream[SLabel]) : Option[(tree.TypelessExpr, Stream[SLabel])] = {
    pat match {
      case AST.Wildcard =>
        None
      case AST.Literal(lit) =>
        Some((tree.Literal(freshLabels.head, lit, ()), freshLabels.tail))
      case AST.Ident(name) =>
        Some((tree.Ident(freshLabels.head, name.toString, (), ()), freshLabels.tail))
      case metaVar @ AST.MetaVariable(_) =>
        def instantiateValue(v : Value) : Option[tree.TypelessExpr] =
          v.cata(
              _ => None
            , _ => None
            , e => Some(eraseSemanticInfo(e))
            )

        for (v <- lookup(metaVar, context, eqMetaVariables);
             e <- instantiateValue(v))
        yield (e, freshLabels)
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
      case AST.Block(stmtPats) =>
        for ((stmts, freshLabelsN) <- instantiateStmts(stmtPats, context, freshLabels))
        yield (tree.Block(freshLabelsN.head, stmts, ()), freshLabelsN.tail)
    }
  }
}
