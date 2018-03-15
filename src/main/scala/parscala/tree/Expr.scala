package parscala
package tree

import scala.language.higherKinds

import scala.meta

import scalaz.{State, StateT, \/, Traverse, Monad, MonadState, MonadTrans, IndexedStateT}
import scalaz.syntax.bind.ToBindOpsUnapply // >>= and >>

import parscala.Control.{mapM, foldM_, forM, forM_}
import dot.{DotAttr, DotGraph, DotNode, DotEdge}

class ExprTree (val root : Expr, val nodes : ExprMap)

sealed abstract class Expr {
  def label : SLabel
}

case class Literal(val l : SLabel, val sugared : meta.Lit, val typ : List[scalac.Type]) extends Expr {
  def label : SLabel = l
}

case class Ident(val l : SLabel, val name : String, val symbols : List[Symbol], val typ : List[scalac.Type]) extends Expr {
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

case class AppUnary(val l : SLabel, val method : meta.Name, val arg : Expr, val typ : List[scalac.Type]) extends Expr {
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
              ident : (SLabel, String, List[Symbol], List[scalac.Type]) => A,
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
              nOther : (SLabel, meta.Term, List[scalac.Type]) => A,
              n : Expr) : A =
    n match {
      case Literal(sl, lit, t) => literal(sl, lit, t)
      case Ident(sl, name, syms, t) => ident(sl, name, syms, t)
      case Assign(sl, lhs, rhs, t) => assign(sl, lhs, rhs, t)
      case App(sl, f, args, t) => app(sl, f, args, t)
      case AppInfix(sl, lhs, op, rhs, t) => appInfix(sl, lhs, op, rhs, t)
      case AppUnary(sl, op, rhs, t) => appUnary(sl, op, rhs, t)
      case New(sl, cls, argss, t) => new_(sl, cls, argss, t)
      case Select(sl, qual, name, t) => select(sl, qual, name, t)
      case This(sl, qual, t) => this_(sl, qual, t)
      case Super(sl, thisp, superp, t) => super_(sl, thisp, superp, t)
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
      case Other(sl, expr, t) => nOther(sl, expr, t)
    }

  case class St
    ( pGen : PLabelGen
    , sGen : SLabelGen
    , dGen : DLabelGen
    , exprs : ExprMap
    , symbols : SymbolTable
    , decls : DeclMap
    , defns : DefnMap
    , topLevels : List[Either[Decl, Defn]]
    , callTargets : Map[SLabel, List[Either[DLabel, SLabel]]]
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

  def symbolsOf(trees : List[Tree]) : List[Symbol] =
    for (t <- trees; s = t.symbol; if s != null) yield s

  def genExpr(sugared : meta.Term, ts : List[Tree]) : NodeGen[Expr] = {
    val samePos : List[Tree] = ts.flatMap(searchSamePosition(sugared, _))
    val types : List[scalac.Type] = samePos map (_.tpe)
    val childSamePos : meta.Tree => List[Tree] =
      if (samePos.isEmpty)
        child => ts.flatMap(searchSamePosition(child, _))
      else
        child => samePos.flatMap(searchSamePosition(child, _))

    def resugarChild(child : meta.Term) : NodeGen[Expr] =
      genExpr(child, childSamePos(child))

    Control.exprCataMeta(
        lit => label(Literal(_, lit, types)) // literal
      , name => label(Ident(_, name, symbolsOf(ts), types)) // name
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
         _ <- modifySt { st => (decl, st.copy( decls = st.decls + (l -> decl)
                                             , topLevels = if (Decl.isTopLevel(decl))
                                                             Left(decl) :: st.topLevels
                                                           else
                                                             st.topLevels
                                             )
                               )
                       }
         )
    yield decl

  private def putDefn[D <: Defn](genLabel : NodeGen[DLabel])(f : DLabel => NodeGen[D]) : NodeGen[D] =
    for (l <- genLabel;
         defn <- f(l);
         _ <- modifySt { st => (defn, st.copy( defns = st.defns + (l -> defn)
                                             , topLevels = if (Defn.isTopLevel(defn))
                                                             Right(defn) :: st.topLevels
                                                           else
                                                             st.topLevels
                                             )
                               )
                       }
         )
    yield defn

  private def check(b : Boolean, msg : String) : NodeGen[Unit] =
    if (b)
      stateInstance.pure(())
    else
      raiseError(msg)

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

  def resugar(sugared : meta.Source, desugared : Tree) : NodeGen[Unit] = {
    val metaStats : List[meta.Stat] = sugared.stats
    forM_(metaStats){ stat => genStat(stat, searchSamePosition(stat, desugared)) }
  }

  def genStat(sugared : meta.Stat, ts : List[Tree]) : NodeGen[Statement] =
    Control.metaStatKindCata(
        term => // term
          stateInstance.map(genExpr(term, ts))(Statement.fromExpr(_))
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

  def genDefn(sugared : meta.Defn, ts : List[Tree]) : NodeGen[Defn] = {
    val samePos : List[Tree] = ts.flatMap(searchSamePosition(sugared, _))

    val childSamePos : meta.Tree => List[Tree] =
      if (samePos.isEmpty)
        child => ts.flatMap(searchSamePosition(child, _))
      else
        child => samePos.flatMap(searchSamePosition(child, _))

    val symbols : List[Symbol] = symbolsOf(ts)

    Control.defnCataMeta(
        (_mods, pats, _oDeclType, metaRhs) => _ => // value
          putDefn(genDLabel()){ l => {
              for( _ <- forM_(symbols)(addSymbol(_, l));
                   rhs <- genExpr(metaRhs, childSamePos(metaRhs)))
              yield Defn.Val(l, pats, symbols, rhs)
            }
          }
      , (_mods, pats, _oDeclType, oMetaRhs) => _ => // variable
          putDefn(genDLabel()){ l => {
              val optionTraverse : Traverse[Option] = scalaz.std.option.optionInstance
              for( _ <- forM_(symbols)(addSymbol(_, l));
                   oRhs <- optionTraverse.traverse(oMetaRhs)(metaRhs => genExpr(metaRhs, childSamePos(metaRhs))))
              yield Defn.Var(l, pats, symbols, oRhs)
            }
          }
      , (_mods, name, _typeParams, paramss, oDeclType, metaBody) => _ => // method
          putDefn(genDLabel()){ l =>
            for (_ <- forM_(symbols)(addSymbol(_, l));
                 body <- genExpr(metaBody, childSamePos(metaBody)))
            yield Defn.Method(l, symbols, name, paramss, body)
          }
//          raiseError(s"Found ${symbols.length} matching symbols for method definition $name.")
/*          ts match {
            case List(desugared @ scalac.DefDef(_, _, _, _, _, scalacBody)) =>

            case List(_) =>
              raiseError("The matching ast for the method definition " + name + " is not a method.")
            case List() =>
              raiseError("There are no matching asts for the method definition " + name + ".")
            case _ =>
              raiseError("There are more than one matching asts for the method definition " + name + ".")
          }
*/
      , (_mods, _name, _typeParams, _paramss, _oDeclType, _metaBody) => _ => // macro
          raiseError("Macros are not supported yet.")
      , (_mods, _name, _typeParams, _metaBody) => _ => // type
          raiseError("Type definitions are not supported yet.")
      , (_mods, name, _typeParams, _constructor, metaBody) => _ => // class
          putDefn(genDLabel()){ l =>
           for (_ <- forM_(symbols)(addSymbol(_, l));
                statements <- resugarTemplate(metaBody, ts))
           yield Defn.Class(l, symbols, name, statements)
          }
//              raiseError(s"Found ${symbols.length} matching symbols and ${ts.length} matching asts for class definition $name.")
/*          ts match {
            case List(scalac.ClassDef(_, _, _, scalacBody)) =>
            case List(_) =>
              raiseError("The matching ast for the class definition " + name + " is not a class. ")
            case List() =>
              raiseError("There are no matching asts for the class definition " + name + ".")
            case _ =>
              raiseError("There are more than one matching asts for the class definition " + name + ".")
          }
*/ 
      , (_mods, name, _typeParams, _constructor, metaBody) => _ => // trait
          putDefn(genDLabel()){ l =>
            for (_ <- forM_(symbols)(addSymbol(_, l));
                 statements <- resugarTemplate(metaBody, ts))
            yield Defn.Trait(l, symbols, name, statements)
          }
//              raiseError(s"Found ${symbols.length} matching symbols and ${ts.length} matching asts for trait definition $name.")
/*
            case List(scalac.ClassDef(_, _, _, scalacBody)) =>
            case List(_) =>
              raiseError("The matching ast for the trait definition " + name + " is not a trait.")
            case List() =>
              raiseError("There are no matching asts for the trait definition " + name + ".")
            case _ =>
              raiseError("There are more than one matching asts for the trait definition " + name + ".")
*/
      , (_mods, name, metaBody) => _ => // object
          putDefn(genDLabel()){ l =>
            for(_ <- forM_(symbols)(addSymbol(_, l));
                statements <- resugarTemplate(metaBody, ts))
            yield Defn.Object(l, symbols, name, statements)
          }
//              raiseError(s"Found ${symbols.length} matching symbols and ${ts.length} matching asts for object definition $name.")
/*          ts match {
            case List(scalac.ModuleDef(scalacMods @ _, scalacName @ _, scalacBody)) =>

            case List(_) =>
              raiseError("The matching ast for the object definition " + name + " is not an object.")
            case List() =>
              raiseError("There are no matching asts for the object definition " + name + ".")
            case _ =>
              raiseError("There are more than one matching asts for the object definition " + name + ".")
          }
*/
      , sugared
      )
  }

  /**
   * 't' need not represent a template. It suffices if it has a Template descendant.
   */
  def resugarTemplate(sugared : meta.Template, ts : List[Tree]) : NodeGen[List[Statement]] = {
    val metaStatements : List[meta.Stat] = sugared.stats
    val listTraverse : Traverse[List] = scalaz.std.list.listInstance
    nodeGenMonadInstance.traverse(metaStatements){ statement => genStat(statement, ts.flatMap(searchSamePosition(statement, _))) }(listTraverse)
  }

  def genPkg(sugared : meta.Pkg, ts : List[Tree]) : NodeGen[Defn.Package] = {
    val symbols : List[Symbol] = symbolsOf(ts)
    putDefn(genDLabel()){ l =>
      for ( _ <- forM_(symbols)(addSymbol(_, l));
            statements <- forM(sugared.stats)( stat => genStat(stat, ts.flatMap(searchSamePosition(stat, _))) ))
      yield Defn.Package(l, symbols, sugared.ref, statements)
    }
  //  raiseError(s"Found ${symbols.length} matching symbols and ${ts.length} matching asts for package definition ${sugared.ref}.")
/*
      case List(scalac.PackageDef(_, scalacStats)) =>

      case List(_) =>
        raiseError("The matching desugared ast for the package " + sugared.name + " is not a package definition.")
      case List() =>
        raiseError("There are no matching desugared asts for the package " + sugared.name + ".")
      case _ =>
        raiseError("There are more than one matching desugared asts for the package " + sugared.name + ".")
*/
  }

  def genImport(sugared : meta.Import, ts : List[Tree]) : NodeGen[Decl.Import] = {
    val symbols : List[Symbol] = symbolsOf(ts)
    putDecl(genDLabel()){ l =>
      for (_ <- forM_(symbols)(addSymbol(_, l)))
      yield Decl.Import(l, sugared)
    }
/*      case List(_) =>
        raiseError("The matching desugared ast of the import declaration " + sugared + " is not an import declaration.")
      case List() =>
        raiseError("There are no matching desugared asts for the import declaration " + sugared + ".")
      case _ =>
        raiseError("There are more than one matching desugared asts for the import declaration " + sugared + ".")
*/
  }

  def genPkgObj(sugared : meta.Pkg.Object, ts : List[Tree]) : NodeGen[Defn.PackageObject] = {
    val meta.Pkg.Object(_, name, metaBody) = sugared
    val symbols : List[Symbol] = symbolsOf(ts)
    putDefn(genDLabel()){ l =>
      for (_ <- forM_(symbols)(addSymbol(_, l));
           body <- resugarTemplate(metaBody, ts))
      yield Defn.PackageObject(l, symbols, name, body)
    }
/*    ts match {
      case List(scalac.PackageDef(_, List(desugared @ scalac.ModuleDef(_, _, scalacBody)))) =>

      case List(_) =>
        raiseError("The matching desugared ast of the package object definition " + name + " is not a package object definition.")
      case List() =>
        raiseError("There are no matching desugared asts for the package object definition " + name + ".")
      case _ =>
        raiseError("There are more than one matching desugared asts for the package object definition " + name + ".")
    }
*/
  }

  def genDecl(sugared : meta.Decl, ts : List[Tree]) : NodeGen[Decl] = {
    val symbols : List[Symbol] = symbolsOf(ts)
    Control.declCataMeta(
        (mods, pats) => _ => // val
          putDecl(genDLabel()){ l =>
            for (_ <- forM_(symbols)(addSymbol(_, l)))
            yield Decl.Val(l, pats, symbols)
          }
//            , raiseError("For a value declaration statement, there is a matching desugared ast which is not a value declaration.")
      , (mods, pats) => _ => // var
          putDecl(genDLabel()){ l => 
            for (_ <- forM(symbols)(addSymbol(_, l)))
            yield Decl.Var(l, pats, symbols)
          }
          //raiseError("For a variable declaration statement, there is a matching desugared ast which is not a variable declaration nor is a method.")
      , (_mods, name, _typeParams, argss) => _ => // method
          putDecl(genDLabel()){ l =>
            for (_ <- forM_(symbols)(addSymbol(_, l)))
            yield Decl.Method(l, symbols, name, argss)
          }
/*
          ts match {
            case List(tr : scalac.DefDef) =>
            case List(_) =>
              raiseError("The matching desugared ast of a method declaration is not a method declaration.")
            case List() =>
              raiseError("There are no matching desugared asts for declaration of method " + name + ".")
            case _ =>
              raiseError("There are more than one desugared asts for declaration of method " + name + ".")
          }
*/
      , (mods, name, typeParams, bounds) => _ =>  // type
          putDecl(genDLabel()){ l =>
            for(_ <- forM_(symbols)(addSymbol(_, l)))
            yield Decl.Type(l, symbols, name, typeParams, bounds)
          }
/*
          ts match {
            case List(tr : scalac.TypeDef) =>
            case List(_) =>
              raiseError("The matching desugared ast of a type declaration is not a type declaration.")
            case List() =>
              raiseError("There are no matching sugared asts for declaration of type " + name + ".")
            case _ =>
              raiseError("There are more than one sugared asts for declaration of type " + name + ".")
          }
*/
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
    val startSt : St = St(PLabel.stream, SLabel.stream, DLabel.stream, Map(), Map(), Map(), Map(), List(), Map())
    m.run(startSt)
  }

  def toDot(n : Expr) : DotGraph = {
    type St = (List[DotNode], List[DotEdge])
    type DotGen[A] = State[St, A]

    val dotGenState : MonadState[DotGen, St] = IndexedStateT.stateMonad

    def addNode(node : DotNode) : DotGen[DotNode] = add(node, List())

    def add(node : DotNode, edges : List[DotEdge]) : DotGen[DotNode] = 
      for (_ <- dotGenState.modify{ case (ns, es) => (node :: ns, edges ++ es) })
      yield node

    def addEdge(edge : DotEdge) : DotGen[Unit] =
      dotGenState.modify{ case (ns, es) => (ns, edge :: es) }

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
            add(DotNode.record(l, "Literal", s"${lit.toString}, types: ${t.toString}"), List())
        , (l, ident, _, t) => // identifier reference
            add(DotNode.record(l, "Identifier", s"${ident.toString}, types: ${t.toString}"), List())
        , (l, lhs, rhs, t) => // assignment
            formatExpr(lhs) >>= (left => 
            formatExpr(rhs) >>= (right => {
              val as = DotNode.record(l, "Assignment", t.toString())
              val lEdge = edge(as, left, "left")
              val rEdge = edge(as, right, "right")
              add(as, List(lEdge, rEdge))
            }))
        , (l, m, args, _t) => // application
            formatExpr(m) >>= ( method => 
            mapM(formatExpr, args) >>= (nodes => {
            val app = DotNode.record(l, "Application", "")
            val edgeToMethod = edge(app, method, "method")
            enum(app, nodes, "arg(%s)".format(_)) >>
            add(app, List(edgeToMethod))
            }))
        , (l, lhs, op, args, _t) => // infix application
            formatExpr(lhs) >>= (lhsNode =>
            mapM(formatExpr, args) >>= (nodes => {
            val app = DotNode.record(l, "Infix application", op.toString)
            addEdge(edge(app, lhsNode, "left"))
            enum(app, nodes, "arg(%s)".format(_))
            add(app, List())
            }))
        , (l, op, arg, _t) => // unary application
            formatExpr(arg) >>= (argNode => {
            add(argNode, List())
            val app = DotNode.record(l, "Unary application", op.toString)
            add(app, List(edge(app, argNode, "arg")))
            })
        , (l, class_, argss, t) => // new
            mapM(mapM(formatExpr, _ : List[Expr]), argss) >>= (nodess => {
              val newE = DotNode.record(l, "New", class_.toString)
              deepEnum(newE, nodess, "arg(%s, %s)".format(_, _)) >>
              add(newE, List())
            })
        , (l, obj, termName, t) => // selection
            formatExpr(obj) >>= (o => {
              val select = DotNode.record(l, "Selection", termName.toString())
              add(select, List(edge(select, o, "")))
            })
        , (l, typeName, _t) => // this
            addNode(DotNode.record(l, "This", typeName.toString))
        , (l, thisp, superp, _t) => // super
            addNode(DotNode.record(l, "Super", thisp.toString + " " + superp.toString))
        , (l, comps, t) => // tuple
            mapM(formatExpr, comps) >>= (nodes => {
              val tuple = DotNode.record(l, "Tuple", t.toString())
              enum(tuple, nodes,"comp(%s)".format(_))
              add(tuple, List())
            })
        , (l, pred, thenE, t) => // if-then
            formatExpr(pred) >>= (p =>
            formatExpr(thenE) >>= (th => {
              val ifE = DotNode.record(l, "If-then", "")
              add(ifE, List(edge(ifE, p, "predicate"),
                            edge(ifE, th, "then")))
            }))
        , (l, pred, thenE, elseE, t) => // if-then-else
            formatExpr(pred) >>= (p =>
            formatExpr(thenE) >>= (th =>
            formatExpr(elseE) >>= (el => {
              val ifE = DotNode.record(l, "If-then-else", "")
              add(ifE, List(edge(ifE, p, "predicate"),
                            edge(ifE, th, "then"),
                            edge(ifE, el, "else")))
            })))
        , (l, pred, body, t) => // while loop
            formatExpr(pred) >>= (p =>
            formatExpr(body) >>= (b => {
              val whileE = DotNode.record(l, "While loop", "")
              add(whileE, List(edge(whileE, p, "predicate"), 
                               edge(whileE, b, "body")))
            }))
        , (l, enums, body, t) => // for loop
//            mapM(formatExpr, enums) >>= (nodes =>
            formatExpr(body) >>= (b => {
              val forE = DotNode.record(l, "For loop", "")
//              enum(forE, nodes, "enum(%s)".format(_)) >>
              add(forE, List(edge(forE, b, "body")))
//            }))
            })
        , (l, enums, body, t) => // for-yield loop
//            mapM(formatExpr, enums) >>= (nodes =>
            formatExpr(body) >>= (b => {
              val forE = DotNode.record(l, "For-yield loop", "")
//              enum(forE, nodes, "enum(%s)".format(_)) >>
              add(forE, List(edge(forE, b, "yield")))
//            }))
            })
        , (l, t) => { // return
            val returnE = DotNode.record(l, "Return", "")
            add(returnE, List())
          }
        , (l, expr, t) => // return with expr
            formatExpr(expr) >>= (e => {
              val returnE = DotNode.record(l, "Return", "")
              add(returnE, List(edge(returnE, e, "return")))
            })
        , (l, expr, t) => // throw
            formatExpr(expr) >>= (e => {
              val throwE = DotNode.record(l, "Throw", "")
              add(throwE, List(edge(throwE, e, "throw")))
            })
        , (l, stmts, _) => // block
            for (nodes <- mapM[DotGen, Statement, DotNode](
                              (stmt : Statement) =>
                                stmt.fold((decl : Decl) => dotGenState.pure(DotNode.record(decl.label, "Declaration", ""))
                                         ,(defn : Defn) => dotGenState.pure(DotNode.record(defn.label, "Definition", ""))
                                         ,(e : Expr) => formatExpr(e)
                                         )
                            , stmts
                            );
                 b : DotNode = DotNode.record(l, "Block", "");
                 _ <- enum(b, nodes, "expr(%s)".format(_));
                 node <- add(b, List()))
             yield node
/*        , (l, args, body, _) => // lambda function
            mapM(formatExpr, args) >>= (nodes => {
              formatExpr(body) >>= (b => {
                val lambda = DotNode.record(l, "Lambda", "")
                enum(lambda, nodes, "arg(%s)".format(_)) >>
                add(lambda, List(edge(lambda, b, "body")))
              })
            })
*/
        , (l, expr, _t) => { // other expression
            val e = DotNode.record(l, "Expression", expr.toString())
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
