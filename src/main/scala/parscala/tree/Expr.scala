package parscala
package tree

import scala.language.higherKinds

import scala.meta

import scalaz.{State, StateT, \/, Traverse, Monad, MonadState, MonadTrans, IndexedStateT}
import scalaz.syntax.bind.ToBindOpsUnapply // >>= and >>

import parscala.Control.{foldM_, forM, forM_}
import dot.{Dot, DotAttr, DotGraph, DotNode, DotEdge, Shape}

class ExprTree (val root : Expr, val nodes : ExprMap)

sealed abstract class Expr {
  def label : SLabel
}

case class Literal(val l : SLabel, val sugared : meta.Lit, val typ : List[scalac.Type]) extends Expr {
  def label : SLabel = l
}

case class Ident(val l : SLabel, val symbols : List[Symbol], val typ : List[scalac.Type]) extends Expr {
  def label : SLabel = l
}

case class Assign(val l : SLabel, val lhs : Expr, val rhs : Expr, val typ : List[scalac.Type]) extends Expr {
  def label : SLabel = l
}

case class App(val l : SLabel, val method : Expr, val args : List[Expr], val typ : List[scalac.Type]) extends Expr {
  def label : SLabel = l
}

case class AppInfix(val l : SLabel, val lhs : Expr, val method : meta.Name, val args : List[Expr], val typ : List[scalac.Type]) extends Expr {
  def label : SLabel = l
}

case class AppUnary(val l : SLabel, val method : Ident, val arg : Expr, val funRef : DLabel, val typ : List[scalac.Type]) extends Expr {
  def label : SLabel = l
}

case class New(val l : SLabel, val tpe : meta.Type, val args : List[List[Expr]], val typ : List[scalac.Type]) extends Expr {
  def label : SLabel = l
}

case class NewAnonymous(val l : SLabel, val template : Template, val typ : List[scalac.Type]) extends Expr {
  def label : SLabel = l
}

case class Select(val l : SLabel, val qualifier : Expr, val sel : meta.Name, val typ : List[scalac.Type]) extends Expr {
  def label : SLabel = l
}

case class This(val l : SLabel, val qualifier : meta.Name, val typ : List[scalac.Type]) extends Expr {
  def label : SLabel = l
}

case class Super(val l : SLabel, val thisp : meta.Name, val superp : meta.Name, val typ : List[scalac.Type]) extends Expr {
  def label : SLabel = l
}

case class Tuple(val l : SLabel, val components : List[Expr], val typ : List[scalac.Type]) extends Expr {
  def label : SLabel = l
}

case class If(val l : SLabel, val pred : Expr, val thenE : Expr, val typ : List[scalac.Type]) extends Expr {
  def label : SLabel = l
}

case class IfElse(val l : SLabel, val pred : Expr, val thenE : Expr, val elseE : Expr, val typ : List[scalac.Type]) extends Expr {
  def label : SLabel = l
}

case class While(val l : SLabel, val pred : Expr, val body : Expr, val typ : List[scalac.Type]) extends Expr {
  def label : SLabel = l
}

case class For(val l : SLabel, val enums : List[meta.Enumerator], val body : Expr, val typ : List[scalac.Type]) extends Expr {
  def label : SLabel = l
}

case class ForYield(val l : SLabel, val enums : List[meta.Enumerator], val body : Expr, val typ : List[scalac.Type]) extends Expr {
  def label : SLabel = l
}

case class ReturnUnit(val l : SLabel, val typ : List[scalac.Type]) extends Expr {
  def label : SLabel = l
}

case class Return(val l : SLabel, val e : Expr, val typ : List[scalac.Type]) extends Expr {
  def label : SLabel = l
}

case class Throw(val l : SLabel, val e : Expr, val typ : List[scalac.Type]) extends Expr {
  def label : SLabel = l
}

case class Block(val l : SLabel, val statements : List[Statement], val typ : List[scalac.Type]) extends Expr {
  def label : SLabel = l
}

case class Lambda(val l : SLabel, val args : List[Expr], val body : Expr, val typ : List[scalac.Type]) extends Expr {
  def label : SLabel = l
}

case class Other(val l : SLabel, val expr : meta.Term, val typ : List[scalac.Type]) extends Expr {
  def label : SLabel = l
}

final case class Template(early : List[Statement], inits : List[Initializer], self : meta.Self, statements : List[Statement])

final case class Initializer(tpe : meta.Type, argss : List[List[Expr]])

sealed abstract class Reference

final case class RefThis(val expr : This) extends Reference
final case class RefSuper(val expr : Super) extends Reference
final case class RefIdent(val expr : Ident) extends Reference
final case class RefSelect(val expr : Select) extends Reference
final case class RefAppUnary(val expr : AppUnary) extends Reference

object Expr {
  def cata[A](literal : (SLabel, meta.Lit, List[scalac.Type]) => A,
              ident : (SLabel, List[Symbol], List[scalac.Type]) => A,
              assign : (SLabel, Expr, Expr, List[scalac.Type]) => A,
              app : (SLabel, Expr, List[Expr], List[scalac.Type]) => A,
              appInfix : (SLabel, Expr, meta.Name, List[Expr], List[scalac.Type]) => A,
              appUnary : (SLabel, meta.Name, Expr, List[scalac.Type]) => A,
              new_ : (SLabel, meta.Type, List[List[Expr]], List[scalac.Type]) => A,
              select : (SLabel, Expr, meta.Name, List[scalac.Type]) => A,
              this_ : (SLabel, meta.Name, List[scalac.Type]) => A,
              super_ : (SLabel, meta.Name, meta.Name, List[scalac.Type]) => A,
              tuple : (SLabel, List[Expr], List[scalac.Type]) => A,
              if_ : (SLabel, Expr, Expr, List[scalac.Type]) => A,
              ifElse : (SLabel, Expr, Expr, Expr, List[scalac.Type]) => A,
              while_ : (SLabel, Expr, Expr, List[scalac.Type]) => A,
              for_ : (SLabel, List[meta.Enumerator], Expr, List[scalac.Type]) => A,
              forYield : (SLabel, List[meta.Enumerator], Expr, List[scalac.Type]) => A,
              returnUnit : (SLabel, List[scalac.Type]) => A,
              return_ : (SLabel, Expr, List[scalac.Type]) => A,
              throw_ : (SLabel, Expr, List[scalac.Type]) => A,
              block : (SLabel, List[Statement], List[scalac.Type]) => A,
              nOther : (SLabel, List[scalac.Type]) => A,
              n : Expr) : A =
    n match {
      case Literal(sl, lit, t) => literal(sl, lit, t)
      case Ident(sl, syms, t) => ident(sl, syms, t)
      case Assign(sl, lhs, rhs, t) => assign(sl, lhs, rhs, t)
      case App(sl, f, args, t) => app(sl, f, args, t)
      case AppInfix(sl, lhs, op, rhs, t) => appInfix(sl, lhs, op, rhs, t)
      case AppUnary(sl, op, rhs, t) => appUnary(sl, op, rhs, t)
      case New(sl, cls, argss, t) => new_(sl, cls, argss, t)
      case Select(sl, qual, name, t) => select(sl, qual, name, t)
      case This(sl, qual, t) => this_(sl, qual, t)
      case Tuple(sl, comps, t) => tuple(sl, comps, t)
      case If(sl, pred, thenE, t) => if_(sl, pred, thenE, t)
      case IfElse(sl, pred, thenE, elseE, t) => ifElse(sl, pred, thenE, elseE, t)
      case While(sl, pred, body, t) => while_(sl, pred, body, t)
      case For(sl, enums, body, t) => for_(sl, enums, body, t)
      case ForYield(sl, enums, body, t) => forYield(sl, enums, body, t)
      case ReturnUnit(sl, t) => returnUnit(sl, t)
      case Return(sl, expr, t) => return_(sl, expr, t)
      case Throw(sl, expr, t) => throw_(sl, expr, t)
      case Block(sl, statements, t) => block(sl, statements, t)
      case Other(sl, expr) => nOther(sl, expr)
    }

  case class St
    ( pGen : PLabelGen
    , sGen : SLabelGen
    , dGen : DLabelGen
    , exprs : ExprMap
    , symbols : SymbolTable
    , decls : DeclMap
    , defns : DefnMap
    , packages : List[Defn.Package]
    )

  type Exception[A] = String \/ A
  type NodeGen[A] = StateT[Exception, St, A]
  private val stateInstance : MonadState[NodeGen, St] = IndexedStateT.stateTMonadState[St, Exception]
  private val transInstance : MonadTrans[({ type λ[M[_], A] = StateT[M, St, A] })#λ] = IndexedStateT.StateMonadTrans
  val nodeGenMonadInstance : Monad[NodeGen] = stateInstance

  def raiseError[A](e : String) : NodeGen[A] =
    transInstance.liftM[Exception, A](\/.DisjunctionInstances1.raiseError[A](e))
/*
  private val errorInstance : MonadError[NodeGen, String] = new MonadError {
    val errInst : MonadError[Exception, String] = \/.DisjunctionInstances1
    override def bind[A, B](fa : NodeGen[A])(f : NodeGen(A) => NodeGen[B]) : NodeGen[B] = stateInstance.bind
    override def handleError
  }
*/
  private def modifySt[A](f : St => (A, St)) : NodeGen[A] =
    for (s <- stateInstance.get;
         (x, sNew) = f(s);
         _ <- stateInstance.put(sNew))
    yield x

  private def getDLabel (s : Symbol) : NodeGen[Option[DLabel]] =
    stateInstance.gets[Option[DLabel]](_.symbols.get(s))

  private def genSLabel : NodeGen[SLabel] =
    modifySt{ s => (s.sGen.head, s.copy(sGen = s.sGen.tail)) }

  private def genPLabel : NodeGen[PLabel] =
    modifySt{ s => (s.pGen.head, s.copy(pGen = s.pGen.tail)) }

  private def genDLabel() : NodeGen[DLabel] =
    modifySt { s => (s.dGen.head, s.copy(dGen = s.dGen.tail)) }

  private def genDLabel(sym : Symbol) : NodeGen[DLabel] =
    modifySt{ s =>
      s.symbols.get(sym) match {
        case Some(dl) => (dl, s)
        case None => (s.dGen.head, s.copy(dGen = s.dGen.tail, symbols = s.symbols + ((sym, s.dGen.head)))) 
      }
    }

  private def addSymbol(sym : Symbol, l : DLabel) : NodeGen[Unit] =
    modifySt { s => ((), s.copy(symbols = s.symbols + ((sym, l)))) }

  private def label(f : SLabel => Expr) : NodeGen[Expr] = 
    for (l <- genSLabel;
         n = f(l);
         _ <- modifySt{ s => ((), s.copy(exprs = s.exprs.updated(l, n))) })
    yield n

  private def labelPat(f : PLabel => Pat) : NodeGen[Pat] = 
    for (l <- genPLabel;
         p = f(l))
    yield p
/*
  private def nLiteral(lit : Lit, t : Tree) : NodeGen[Expr] = 
    label(Literal(_, lit, t))

  private def nIdent(symbol : Symbol, ident : Tree) : NodeGen[Expr] =
    label(Ident(_, symbol, ident))

  private def nPatDef(lhs : Pat, rhs : Expr, tr : Tree) : NodeGen[Expr] =
    label(PatDef(_, lhs, rhs, tr))

  private def nAssign(lhs : Expr, rhs : Expr, tr : Tree) : NodeGen[Expr] =
    label(Assign(_, lhs, rhs, tr))

  private def nNew(constructor : Tree, args : List[List[Expr]], tr : Tree) : NodeGen[Expr] = 
    label(New(_, constructor, args, tr))

  private def nApp(fun : Expr, args : List[List[Expr]], funRef : DLabel, tr : Tree) : NodeGen[Expr] = 
    label(App(_, fun, args, funRef, tr))

  private def nIf(pred : Expr, thenE : Expr, tr : Tree) : NodeGen[Expr] = 
    label(If(_, pred, thenE, tr))

  private def nIfElse(pred : Expr, thenE : Expr, elseE : Expr, tr : Tree) : NodeGen[Expr] = 
    label(IfElse(_, pred, thenE, elseE, tr))

  private def nWhile(pred : Expr, body : Expr, tr : Tree) : NodeGen[Expr] = 
    label(While(_, pred, body, tr))

  private def nFor(enums : List[Expr], body : Expr, tr : Tree) : NodeGen[Expr] = 
    label(For(_, enums, body, tr))

  private def nForYield(enums : List[Expr], body : Expr, tr : Tree) : NodeGen[Expr] = 
    label(ForYield(_, enums, body, tr))

  private def nSelect(expr : Expr, sel : TermName, tr : Tree) : NodeGen[Expr] = 
    label(Select(_, expr, sel, tr))

  private def nThis(qualifier : TypeName, tr : Tree) : NodeGen[Expr] = 
    label(This(_, qualifier, tr))

  private def nTuple(comps : List[Expr], tuple : Tree) : NodeGen[Expr] = 
    label(Tuple(_, comps, tuple))

  private def nReturnUnit(tr : Tree) : NodeGen[Expr] =
    label(ReturnUnit(_, tr))

  private def nReturn(expr : Expr, tr : Tree) : NodeGen[Expr] =
    label(Return(_, expr, tr))

  private def nBlock(exprs : List[Expr], tr : Tree) : NodeGen[Expr] =
    label(Block(_, exprs, tr))

  private def nOther(tr : Tree) : NodeGen[Expr] =
    label(Other(_, tr))

*/

  private def collectMethod(t : Tree) : NodeGen[Unit] =
    if (t.symbol != null && t.symbol.isMethod)
      nodeGenMonadInstance.void(genDLabel(t.symbol))
    else
      nodeGenMonadInstance.pure(())

  def singleton[A, B](as : List[A])(f : A => B)(err : => B) : B =
    as match {
      case List(a) => f(a)
      case _ => err
    }

  def symbols(trees : List[Tree]) : List[Symbol] =
    for (t <- trees; s = t.symbol; if s != null) yield s

  def genExpr2(sugared : meta.Term, ts : List[Tree]) : NodeGen[Expr] = {
    val samePos : List[Tree] = ts.flatMap(searchSamePosition(sugared, _))
    val types : List[scalac.Type] = samePos map (_.tpe)
    def childSamePos(child : meta.Tree) : List[Tree] =
      if (samePos.isEmpty)
        ts.flatMap(searchSamePosition(child, _))
      else
        samePos.flatMap(searchSamePosition(child, _))

    def resugarChild(child : meta.Term) : NodeGen[Expr] =
      genExpr2(child, childSamePos(child))

    Control.exprCataMeta(
        lit => label(Literal(_, lit, types)) // literal
      , name => label(Ident(_, symbols(ts), types)) // name
      , metaComponents => // tuple
          for (components <- forM(metaComponents)(resugarChild(_));
               tuple <- label(Tuple(_, components, types)))
          yield tuple
      , (metaType, _name, metaArgss) => // new
          for (argss <- forM(metaArgss){args =>
                   forM(args)(resugarChild(_))
                 };
               new_ <- label(New(_, metaType, argss, types)))
          yield new_
        // metaName should be inspected for symbols
      , metaName => label(This(_, metaName, types)) // this
        // metaName should be inspected for symbols
      , (metaQualifier, metaName) => // select
          for (qualifier <- resugarChild(metaQualifier);
               select <- label(Select(_, qualifier, metaName, types)))
          yield select
      , (metaFun, metaArgs) => // apply
          for (fun <- resugarChild(metaFun);
               args <- forM(metaArgs)(resugarChild(_));
               app <- label(App(_, fun, args, types)))
          yield app
        // metaOp should be inspected for symbols
      , (metaArgLeft, metaOp, _metaTypeArgs, metaArgsRight) => // applyInfix
          for (argLeft <- resugarChild(metaArgLeft);
               argsRight <- forM(metaArgsRight)(resugarChild(_));
               appInfix <- label(AppInfix(_, argLeft, metaOp, argsRight, types)))
          yield appInfix
      , (metaPred, metaThen) => // if then
          for (pred <- resugarChild(metaPred);
               then_ <- resugarChild(metaThen);
               ifThen <- label(If(_, pred, then_, types)))
          yield ifThen
      , (metaPred, metaThen, metaElse) => // if then else
          for (pred <- resugarChild(metaPred);
               then_ <- resugarChild(metaThen);
               else_ <- resugarChild(metaElse);
               ifThenElse <- label(IfElse(_, pred, then_, else_, types)))
          yield ifThenElse
      , (metaPred, metaBody) => // while
          for (pred <- resugarChild(metaPred);
               body <- resugarChild(metaBody);
               while_ <- label(While(_, pred, body, types)))
          yield while_
      , (metaEnums, metaBody) => // for
          for (body <- resugarChild(metaBody);
               for_ <- label(For(_, metaEnums, body, types)))
          yield for_
      , (metaEnums, metaOutput) => // for yield
          for (output <- resugarChild(metaOutput);
               forYield <- label(For(_, metaEnums, output, types)))
          yield forYield
      , (metaLhs, metaRhs) => // assign
          for (lhs <- resugarChild(metaLhs);
               rhs <- resugarChild(metaRhs);
               assign <- label(Assign(_, lhs, rhs, types)))
          yield assign
      , () => label(ReturnUnit(_, types)) // return
      , (metaExpr) => // return expr
          for (expr <- resugarChild(metaExpr);
               return_ <- label(Return(_, expr, types)))
          yield return_
      , (metaStats) => // block
          for (stats <- forM(metaStats)(genStat(_, samePos));
               block <- label(Block(_, stats, types)))
          yield block
      , (metaTerm) => label(Other(_, metaTerm, types)) // other
      , sugared
      )
  }

/*
  def genExpr(t : Tree, desugared : meta.Term) : NodeGen[Expr] = {
    import scalaz.syntax.bind._
    import scalac.Quasiquote

    def step(ns : List[Expr], tr : Tree) : NodeGen[List[Expr]] =
      for (n <- genExpr(tr, ???))
      yield n :: ns

    def deepStep(nns : List[List[Expr]], tr : List[Tree]) : NodeGen[List[List[Expr]]] = 
      for (ns <- foldM(step, List.empty, tr))
      yield ns.reverse :: nns

    Control.exprCata(
        nLiteral(_, _) // literal
      , ident => // identifier reference
          collectMethod(t) >>
          nIdent(ident, t)
      , comps =>  // tuple
          foldM(step, List.empty, comps) >>= (nodes =>
          nTuple(nodes, t))
      , (earlydefns, parents, stats) => { // new
          val q"$p(...$argss)" :: _ = parents
          foldM(deepStep, List.empty, argss) >>= (nodes =>
          nNew(p, nodes.reverse, t))
        }
      , nThis(_, t) // this
      , (expr, termName) => // selection
          collectMethod(t) >>
          genExpr(expr, ???) >>= (e => 
          nSelect(e, termName, t))
      , (method, argss) => // application
          genExpr(method, ???) >>= (m =>
          foldM(deepStep, List.empty, argss) >>= (nodes =>
          getDLabel(method.symbol) >>= {
            case Some(funRef) =>
              nApp(m, nodes, funRef, t)
            case None =>
              genDLabel(method.symbol) >>= (funRef =>
              nApp(m, nodes, funRef, t)
              )
          }))
      , (pred, thenE) => // if-then
          genExpr(pred, ???) >>= (p => 
          genExpr(thenE, ???) >>= (th =>
          nIf(p, th, t)))
      , (pred, thenE, elseE) => // if-then-else
          genExpr(pred, ???) >>= (p => 
          genExpr(thenE, ???) >>= (th =>
          genExpr(elseE, ???) >>= (e =>
          nIfElse(p, th, e, t))))
      , (pred, body) => // while loop
          genExpr(pred, ???) >>= (p => 
          genExpr(body, ???) >>= (b =>
          nWhile(p, b, t)))
      , (enums, body) => // for loop
          foldM(step, List.empty, enums) >>= (nodes =>
          genExpr(body, ???) >>= (b =>
          nFor(nodes, b, t)))
      , (enums, body) => // for-yield loop
          foldM(step, List.empty, enums) >>= (nodes =>
          genExpr(body, ???) >>= (b =>
          nForYield(nodes, b, t)))
      , (lhs, rhs) => // assignment
          genExpr(lhs, ???) >>= (lExpr =>
          genExpr(rhs, ???) >>= (rExpr =>
          nAssign(lExpr, rExpr, t)))
      , (_, lhs, rhs) => // var or val def
          genExpr(rhs, ???) >>= (rExpr =>
          labelPat(IdentPat(_, lhs)) >>= (pat =>
          nPatDef(pat, rExpr, t)))
      , () => // return
          nReturnUnit(t)
      , expr => // return with expr
          genExpr(expr, ???) >>= (node =>
          nReturn(node, t))
      , stmts => // expression block
          foldM(step, List.empty, stmts) >>= (nodes => nBlock(nodes.reverse, t))
      , other => // other expression
          nOther(other)
      , t
      )
  }
*/
  private def putDecl[D <: Decl](genLabel : NodeGen[DLabel])(f : DLabel => NodeGen[D]) : NodeGen[D] =
    for (l <- genLabel;
         decl <- f(l);
         _ <- modifySt { st => (decl, st.copy(decls = st.decls + (l -> decl))) }
         )
    yield decl

  private def putDefn[D <: Defn](genLabel : NodeGen[DLabel])(f : DLabel => NodeGen[D]) : NodeGen[D] =
    for (l <- genLabel;
         defn <- f(l);
         _ <- modifySt { st => (defn, st.copy(defns = st.defns + (l -> defn))) }
         )
    yield defn

  private def check(b : Boolean, msg : String) : NodeGen[Unit] =
    if (b)
      stateInstance.pure(())
    else
      raiseError(msg)

  private def dropAnonymousPackage(t : Tree) : List[Tree] = {
    import scalac.Quasiquote
    t match {
      case q"package $p { ..$stats }" if p.symbol.toString == "<empty>" => stats // TODO: is "<empty>" correct?
      case _ => List(t)
    }
  }

  private def searchSamePosition(metaTree : meta.Tree, t : Tree) : List[Tree] = {
    def includes(p : scalac.Position, what : meta.Position) : Boolean =
      p.isRange && (p.start <= what.start && what.end <= p.end)

    def equals(p : scalac.Position, p2 : meta.Position) : Boolean =
      p.isRange && (p.start == p2.start && p.end == p2.end)

    if (includes(t.pos, metaTree.pos)) {
      val samePosChildren : List[Tree] = t.children.flatMap(searchSamePosition(metaTree, _))
      if (equals(t.pos, metaTree.pos))
        t :: samePosChildren
      else
        samePosChildren
    }
    else
      List()
  }

  private def overlapping(pos : meta.Position, desugared : List[Tree]) : List[Tree] =
    desugared.filter{ tr => pos.start <= tr.pos.end && tr.pos.start <= pos.end }

  def resugar(sugared : meta.Source, desugared : Tree) : NodeGen[Unit] = {
    val metaStats : List[meta.Stat] = sugared.stats
    val scalacStats : List[Tree] = dropAnonymousPackage(desugared)
    forM_(metaStats){ stat => genStat(stat, overlapping(stat.pos, scalacStats)) }
  }

  def genStat(sugared : meta.Stat, ts : List[Tree]) : NodeGen[Statement] =
    Control.metaStatKindCata(
        term => // term
          stateInstance.map(genExpr2(term, ts))(Statement.fromExpr(_))
      , decl =>  // decl
          stateInstance.map(genDecl(decl, ts))(Statement.fromDecl(_))
      , defn => // definition
          stateInstance.map(genDefn(defn, ts))(Statement.fromDefn(_))
      , _ => // secondary constructor
          raiseError("Secondary constructors are not supported yet.")
      , pobj => // package object
          stateInstance.map(genPkgObj(pobj, ts))(Statement.fromDefn(_))
      , pkg => // package
          stateInstance.map(genPkg(pkg, ts))(Statement.fromDefn(_))
      , imprt => // import
          stateInstance.map(genImport(imprt, ts))(Statement.fromDecl(_))
      , sugared
      )

  def genDefn(sugared : meta.Defn, ts : List[Tree]) : NodeGen[Defn] =
    Control.defnCataMeta(
        (_mods, pats, _oDeclType, metaRhs) => valDef => // value
          scalaz.std.option.cata(valsVarsGettersSetters(ts))(
            { case (vals @ val_ :: _, getters) =>
                putDefn(genDLabel()){ l => {
                    val symbols : List[Symbol] = vals.map(_.symbol)
                    for( _ <- forM_(symbols)(addSymbol(_, l));
                         scalac.ValDef(_, _, _, scalacRhs) = val_;
                         rhs <- genExpr2(metaRhs, List(scalacRhs)))
                    yield Defn.Val(l, pats, symbols.toSet, rhs, valDef, vals, getters)
                    }
                  }
              case _ =>
                raiseError("Matching asts of the value definition " + valDef + " do not contain value definitions.")
            }
            , raiseError("Matching asts of the value definition " + valDef + " contain asts other than value definitions and getters.")
            )
      , (_mods, pats, _oDeclType, oMetaRhs) => varDef => // variable
          scalaz.std.option.cata(valsVarsGettersSetters(ts))(
            { case (vars @ var_ :: _, gettersSetters) =>
                putDefn(genDLabel()){ l => {
                    val optionTraverse : Traverse[Option] = scalaz.std.option.optionInstance
                    val symbols : List[Symbol] = vars.map(_.symbol)
                    for( _ <- forM_(symbols)(addSymbol(_, l));
                         scalac.ValDef(_, _, _, scalacRhs) = var_;
                         oRhs <- optionTraverse.traverse(oMetaRhs)(metaRhs => genExpr2(metaRhs, List(scalacRhs))))
                    yield Defn.Var(l, pats, symbols.toSet, oRhs, varDef, vars, gettersSetters)
                  }
                }
             case _ =>
               raiseError("Matching asts of the value definition " + varDef + " do not contain value definitions.")
            }
            , raiseError("Matching asts of the value definition " + varDef + " contain asts other than value definitions and getters.")
            )
      , (_mods, name, _typeParams, paramss, oDeclType, metaBody) => defn => // method
          ts match {
            case List(desugared @ scalac.DefDef(_, _, _, _, _, scalacBody)) =>
              putDefn(genDLabel(desugared.symbol)){ l =>
                for (body <- genExpr2(metaBody, List(scalacBody)))
                yield Defn.Method(l, desugared.symbol, name, paramss, body, defn, desugared)
              }
            case List(_) =>
              raiseError("The matching ast for the method definition " + name + " is not a method.")
            case List() =>
              raiseError("There are no matching asts for the method definition " + name + ".")
            case _ =>
              raiseError("There are more than one matching asts for the method definition " + name + ".")
          }
      , (_mods, _name, _typeParams, _paramss, _oDeclType, _metaBody) => _macroDef => // macro
          raiseError("Macros are not supported yet.")
      , (_mods, _name, _typeParams, _metaBody) => _typeDef => // type
          raiseError("Type definitions are not supported yet.")
      , (_mods, name, _typeParams, _constructor, metaBody) => classDef => // class
          ts match {
            case List(desugared @ scalac.ClassDef(_, _, _, scalacBody)) =>
              for (_ <- check(!desugared.symbol.asClass.isTrait, "The matching ast for the class definition " + name + " is a trait.");
                   class_ <- putDefn(genDLabel(desugared.symbol)){ l =>
                               for (statements <- resugarTemplate(metaBody, scalacBody))
                               yield Defn.Class(l, desugared.symbol, name, statements, classDef, desugared)
                             })
                yield class_
            case List(_) =>
              raiseError("The matching ast for the class definition " + name + " is not a class.")
            case List() =>
              raiseError("There are no matching asts for the class definition " + name + ".")
            case _ =>
              raiseError("There are more than one matching asts for the class definition " + name + ".")
          }
      , (_mods, name, _typeParams, _constructor, metaBody) => traitDef => // trait
          ts match {
            case List(desugared @ scalac.ClassDef(_, _, _, scalacBody)) =>
              for (_ <- check(desugared.symbol.asClass.isTrait, "The matching ast for the trait definition " + name + " is a class.");
                   trait_ <- putDefn(genDLabel(desugared.symbol)){ l =>
                               for (statements <- resugarTemplate(metaBody, scalacBody))
                               yield Defn.Trait(l, desugared.symbol, name, statements, traitDef, desugared)
                             })
              yield trait_
            case List(_) =>
              raiseError("The matching ast for the trait definition " + name + " is not a trait.")
            case List() =>
              raiseError("There are no matching asts for the trait definition " + name + ".")
            case _ =>
              raiseError("There are more than one matching asts for the trait definition " + name + ".")
          }
      , (_mods, name, metaBody) => objectDef => // object
          ts match {
            case List(desugared @ scalac.ModuleDef(scalacMods @ _, scalacName @ _, scalacBody)) =>
              putDefn(genDLabel(desugared.symbol)){ l =>
                for(statements <- resugarTemplate(metaBody, scalacBody))
                yield Defn.Object(l, desugared.symbol, name, statements, objectDef, desugared)
              }
            case List(_) =>
              raiseError("The matching ast for the object definition " + name + " is not an object.")
            case List() =>
              raiseError("There are no matching asts for the object definition " + name + ".")
            case _ =>
              raiseError("There are more than one matching asts for the object definition " + name + ".")
          }
      , sugared
      )

  def resugarTemplate(sugared : meta.Template, desugared : scalac.Template) : NodeGen[List[Statement]] = {
    val metaStatements : List[meta.Stat] = sugared.stats
    val scalac.Template(_, _, scalacStatements : List[Tree]) = desugared
    val listTraverse : Traverse[List] = scalaz.std.list.listInstance
    nodeGenMonadInstance.traverse(metaStatements){ statement => genStat(statement, overlapping(statement.pos, scalacStatements)) }(listTraverse)
  }

  def genPkg(sugared : meta.Pkg, ts : List[Tree]) : NodeGen[Defn.Package] =
    ts match {
      case List(tr @ scalac.PackageDef(_, scalacStats)) =>
        putDefn(genDLabel(tr.symbol)){ l =>
          for (statements <- forM(sugared.stats)( stat => genStat(stat, overlapping(stat.pos, scalacStats)) ))
          yield Defn.Package(l, tr.symbol, tr.symbol.fullName, statements, sugared, tr)
        }
      case List(_) =>
        raiseError("The matching desugared ast for the package " + sugared.name + " is not a package definition.")
      case List() =>
        raiseError("There are no matching desugared asts for the package " + sugared.name + ".")
      case _ =>
        raiseError("There are more than one matching desugared asts for the package " + sugared.name + ".")
    }


  def genImport(sugared : meta.Import, ts : List[Tree]) : NodeGen[Decl.Import] =
    ts match {
      case List(desugared : scalac.Import) =>
        putDecl(genDLabel(desugared.symbol)){ l =>
          stateInstance.pure(Decl.Import(l, sugared, desugared))
        }
      case List(_) =>
        raiseError("The matching desugared ast of the import declaration " + sugared + " is not an import declaration.")
      case List() =>
        raiseError("There are no matching desugared asts for the import declaration " + sugared + ".")
      case _ =>
        raiseError("There are more than one matching desugared asts for the import declaration " + sugared + ".")
    }

  def genPkgObj(sugared : meta.Pkg.Object, ts : List[Tree]) : NodeGen[Defn.PackageObject] = {
    val meta.Pkg.Object(_, name, metaBody) = sugared
    ts match {
      case List(scalac.PackageDef(_, List(desugared @ scalac.ModuleDef(_, _, scalacBody)))) =>
        putDefn(genDLabel(desugared.symbol)){ l =>
          for (body <- resugarTemplate(metaBody, scalacBody))
          yield Defn.PackageObject(l, desugared.symbol, name, body, sugared, desugared)
        }
      case List(_) =>
        raiseError("The matching desugared ast of the package object definition " + name + " is not a package object definition.")
      case List() =>
        raiseError("There are no matching desugared asts for the package object definition " + name + ".")
      case _ =>
        raiseError("There are more than one matching desugared asts for the package object definition " + name + ".")
    }
  }

    // the first component has either only values or only variables but not both
  private def valsVarsGettersSetters(ts : List[Tree]) : Option[(List[scalac.ValDef], List[scalac.DefDef])] = {
    val optionMonad : Monad[Option] = scalaz.std.option.optionInstance
    val listFoldable : scalaz.Foldable[List] = scalaz.std.list.listInstance
    listFoldable.foldRightM(ts, (List[scalac.ValDef](), List[scalac.DefDef]())){ case (tr, (vs, getset)) =>
      tr match {
        case v : scalac.ValDef => Some((v :: vs, getset))
        case d : scalac.DefDef => Some((vs, d :: getset))
        case _ => None
      }
    } (optionMonad)
  }

  def genDecl(sugared : meta.Decl, ts : List[Tree]) : NodeGen[Decl] = {
    Control.declCataMeta(
        (mods, pats) => valDecl => // val
          scalaz.std.option.cata(valsVarsGettersSetters(ts))(
            { case (vals, gettersSetters) =>
                putDecl(genDLabel()){ l => for (
                    _ <- check(gettersSetters.isEmpty, "For a value declaration statement, there is a matching getter or a setter.");
                    _ <- forM_(ts){ t => addSymbol(t.symbol, l) };
                    symbols : Set[Symbol] = ts.map(_.symbol).toSet
                  ) yield
                    Decl.Val(l, pats, symbols, valDecl, vals, gettersSetters)
                }
            }
            , raiseError("For a value declaration statement, there is a matching desugared ast which is not a value declaration.")
            )
      , (mods, pats) => varDecl => // var
          scalaz.std.option.cata(valsVarsGettersSetters(ts))(
            { case (vars, gettersSetters) =>
                putDecl(genDLabel()){ l => for (
                    _ <- forM(ts){ t => addSymbol(t.symbol, l) };
                    symbols : Set[Symbol] = ts.map(_.symbol).toSet
                  ) yield
                    Decl.Var(l, pats, symbols, varDecl, vars, gettersSetters)
                }
            }
            , raiseError("For a variable declaration statement, there is a matching desugared ast which is not a variable declaration nor is a method.")
            )
      , (_mods, name, _typeParams, argss) => defDecl => // method
          ts match {
            case List(tr : scalac.DefDef) =>
              putDecl(genDLabel(tr.symbol)){ l =>
                stateInstance.pure(Decl.Method(l, tr.symbol, name, argss, defDecl, tr))
              }
            case List(_) =>
              raiseError("The matching desugared ast of a method declaration is not a method declaration.")
            case List() =>
              raiseError("There are no matching desugared asts for declaration of method " + name + ".")
            case _ =>
              raiseError("There are more than one desugared asts for declaration of method " + name + ".")
          }
      , (mods, name, typeParams, bounds) => typeDecl =>  // type
          ts match {
            case List(tr : scalac.TypeDef) =>
              putDecl(genDLabel(tr.symbol)){ l => 
                stateInstance.pure(Decl.Type(l, tr.symbol, name, typeParams, bounds, typeDecl, tr))
              }
            case List(_) =>
              raiseError("The matching desugared ast of a type declaration is not a type declaration.")
            case List() =>
              raiseError("There are no matching sugared asts for declaration of type " + name + ".")
            case _ =>
              raiseError("There are more than one sugared asts for declaration of type " + name + ".")
          }
      , sugared
      )
  }

  private def genPat(t : Tree) : NodeGen[Pat] = 
    Control.patCata(
        (c) => { // literal
          val lit : Lit = Control.litCata(
              IntLit
            , BooleanLit
            , CharLit
            , StringLit
            , FloatLit
            , DoubleLit
            , SymbolLit
            , OtherLit
            , c
            )
          genPLabel >>= (l =>
          stateInstance.pure(LiteralPat(l, lit))
          )
        }
      , (name, pat) => // binding
          genPat(pat) >>= (p =>
          genPLabel >>= (l =>
          stateInstance.pure(AsPat(l, t.symbol, p))
          ))
      , () => // underscore
          genPLabel >>= (l =>
          stateInstance.pure(UnderscorePat(l))
          )
      , _ => // other pattern
          genPLabel >>= (l =>
          stateInstance.pure(OtherPat(l))
          )
      , t
      )
/*
  def fromTree(t : Tree) : (ProgramGraph, Option[ExprTree]) = {
    val (st, nodes) : (St, Option[ExprTree]) = 
      if (t.isTerm) {
        val (st, node) = run(genExpr(t))
        (st, Some(new ExprTree(node, st.exprs)))
      } else {
        val (st, _) = run(genDecl(t))
        (st, None)
      }
    (new ProgramGraph(st.decls, st.exprs, st.symbols, st.packages), nodes)
  }*/

  def runNodeGen[A](m : NodeGen[A]) : String \/ (St, A) = {
    val startSt : St = St(PLabel.stream, SLabel.stream, DLabel.stream, Map(), Map(), Map(), Map(), List())
    m.run(startSt)
  }

  def toDot(n : Expr) : DotGraph = {
    type St = (List[DotNode], List[DotEdge])
    type DotGen[A] = State[St, A]

    val dotGenState : MonadState[DotGen, St] = IndexedStateT.stateMonad

    def add(node : DotNode, edges : List[DotEdge]) : DotGen[DotNode] = 
      for (_ <- dotGenState.modify{ case (ns, es) => (node :: ns, edges ++ es) })
      yield node

    def addEdge(edge : DotEdge) : DotGen[Unit] =
      dotGenState.modify{ case (ns, es) => (ns, edge :: es) }

    def record(l : SLabel, header : String, body : String) : DotNode =
      DotNode(l.toString) !! DotAttr.shape(Shape.Record) !! DotAttr.labelWithPorts("{ %s - %s | %s }".format(l.toString, header, Dot.dotEscape(body)))

    def edge(source : DotNode, target : DotNode, label : String) : DotEdge =
      DotEdge(source, target) !! DotAttr.label(label)

    def enum(parent : DotNode, children : List[DotNode], eLabelTemplate : String => String) : DotGen[Unit] = 
      foldM_[DotGen, Int, DotNode]((i : Int, child : DotNode) =>
                for (_ <- addEdge(edge(parent, child, eLabelTemplate(i.toString)))) yield i + 1,
             0,
             children)

    def deepEnum(parent : DotNode, children : List[List[DotNode]], eLabelTemplate : (String, String) => String) : DotGen[Unit] =
      foldM_[DotGen, Int, List[DotNode]]((i, chldrn) =>
                for (_ <- enum(parent, chldrn, eLabelTemplate(i.toString, _))) yield i + 1,
            0,
            children)

    def formatExpr(n : Expr) : DotGen[DotNode] =
      cata(
          (l, lit, t) => // literal
            add(record(l, "Literal", t.toString), List())
        , (l, _, t) => // identifier reference
            add(record(l, "Identifier", t.toString), List())
        , (l, pat, rhs, t) => // pattern definition
            formatExpr(rhs) >>= (right => {
            val patDef = record(l, "Pattern definition", pat.toString)
            add(patDef, List(edge(patDef, right, "value")))
            })
        , (l, lhs, rhs, t) => // assignment
            formatExpr(lhs) >>= (left => 
            formatExpr(rhs) >>= (right => {
              val as = record(l, "Assignment", t.toString())
              val lEdge = edge(as, left, "left")
              val rEdge = edge(as, right, "right")
              add(as, List(lEdge, rEdge))
            }))
        , (l, m, argss, _, t) => // application
            formatExpr(m) >>= ( method => 
            mapM(mapM(formatExpr, _ : List[Expr]), argss) >>= (nodess => {
              val app = record(l, "Application", t.toString())
              val edgeToMethod = edge(app, method, "method")
              deepEnum(app, nodess, "arg(%s, %s)".format(_, _)) >>
              add(app, List(edgeToMethod))
            }))
        , (l, cls, argss, t) => // new
            mapM(mapM(formatExpr, _ : List[Expr]), argss) >>= (nodess => {
              val newE = record(l, "New", t.toString())
              //val clsExpr = ???
              deepEnum(newE, nodess, "arg(%s, %s)".format(_, _)) >>
              add(newE, List())
            })
        , (l, obj, termName, t) => // selection
            formatExpr(obj) >>= (o => {
              //val termExpr = ???
              val select = record(l, "Selection", t.toString())
              add(select, List(edge(select, o, "")))
            })
        , (l, typeName, t) => // this
            add(record(l, "This", t.toString()), List())
        , (l, comps, t) => // tuple
            mapM(formatExpr, comps) >>= (nodes => {
              val tuple = record(l, "Tuple", t.toString())
              enum(tuple, nodes,"comp(%s)".format(_))
              add(tuple, List())
            })
        , (l, pred, thenE, t) => // if-then
            formatExpr(pred) >>= (p =>
            formatExpr(thenE) >>= (th => {
              val ifE = record(l, "If-then", "")
              add(ifE, List(edge(ifE, p, "predicate"),
                            edge(ifE, th, "then")))
            }))
        , (l, pred, thenE, elseE, t) => // if-then-else
            formatExpr(pred) >>= (p =>
            formatExpr(thenE) >>= (th =>
            formatExpr(elseE) >>= (el => {
              val ifE = record(l, "If-then-else", "")
              add(ifE, List(edge(ifE, p, "predicate"),
                            edge(ifE, th, "then"),
                            edge(ifE, el, "else")))
            })))
        , (l, pred, body, t) => // while loop
            formatExpr(pred) >>= (p =>
            formatExpr(body) >>= (b => {
              val whileE = record(l, "While loop", "")
              add(whileE, List(edge(whileE, p, "predicate"), 
                               edge(whileE, b, "body")))
            }))
        , (l, enums, body, t) => // for loop
            mapM(formatExpr, enums) >>= (nodes =>
            formatExpr(body) >>= (b => {
              val forE = record(l, "For loop", "")
              enum(forE, nodes, "enum(%s)".format(_)) >>
              add(forE, List(edge(forE, b, "body")))
            }))
        , (l, enums, body, t) => // for-yield loop
            mapM(formatExpr, enums) >>= (nodes =>
            formatExpr(body) >>= (b => {
              val forE = record(l, "For-yield loop", "")
              enum(forE, nodes, "enum(%s)".format(_)) >>
              add(forE, List(edge(forE, b, "yield")))
            }))
        , (l, t) => { // return
            val returnE = record(l, "Return", "")
            add(returnE, List())
          }
        , (l, expr, t) => // return with expr
            formatExpr(expr) >>= (e => {
              val returnE = record(l, "Return", "")
              add(returnE, List(edge(returnE, e, "return")))
            })
        , (l, expr, t) => // throw
            formatExpr(expr) >>= (e => {
              val throwE = record(l, "Throw", "")
              add(throwE, List(edge(throwE, e, "throw")))
            })
        , (l, stmts, _) => // block
            mapM(formatExpr, stmts) >>= (nodes => {
              val b = record(l, "Block", "")
              enum(b, nodes, "expr(%s)".format(_)) >>
              add(b, List())
            })
        , (l, args, body, _) => // lambda function
            mapM(formatExpr, args) >>= (nodes => {
              formatExpr(body) >>= (b => {
                val lambda = record(l, "Lambda", "")
                enum(lambda, nodes, "arg(%s)".format(_)) >>
                add(lambda, List(edge(lambda, b, "body")))
              })
            })
        , (l, expr) => { // other expression
            val e = record(l, "Expression", expr.toString())
            add(e, List())
          }
        , n
        )

    val (nodes, edges) = formatExpr(n).exec((List(), List()))
    DotGraph("", nodes.reverse, edges)
  }
/*
  def resugar(root : Expr, transformer : Expr => NodeGen[Expr]) : NodeGen[Expr] = {
    for (transformRes <- transformer(root))
    yield transformRes match {
      }
  }
*/
  
}

sealed abstract class TransformResult[A]

case class Skip[A]() extends TransformResult[A]
case class Changed[A](result : A) extends TransformResult[A]


case class Continue[A]() extends TransformResult[A]
case class TransformSkip[A](result : A) extends TransformResult[A]
case class TransformContinue[A](result : A) extends TransformResult[A]
