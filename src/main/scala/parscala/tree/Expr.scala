package parscala
package tree

import scala.language.higherKinds

import scala.meta

import org.typelevel.paiges.Doc
import scalaz.{StateT, \/, Traverse, Monoid, Monad, MonadState, MonadTrans, IndexedStateT, WriterT}
// import scalaz.syntax.bind.ToBindOpsUnapply // >>= and >>

import parscala.Control.{mapM, forM, forM_}
import dot.{DotGraph, DotNode, DotGen}

sealed abstract class Expr[IdentInfo, SemanticInfo] {
  def label : SLabel

  def cata[A](literal : (SLabel, meta.Lit, SemanticInfo) => A,
              ident : (SLabel, String, IdentInfo, SemanticInfo) => A,
              assign : (SLabel, Expr[IdentInfo, SemanticInfo], Expr[IdentInfo, SemanticInfo], SemanticInfo) => A,
              app : (SLabel, Expr[IdentInfo, SemanticInfo], List[Expr[IdentInfo, SemanticInfo]], SemanticInfo) => A,
              appInfix : (SLabel, Expr[IdentInfo, SemanticInfo], meta.Term.Name, List[Expr[IdentInfo, SemanticInfo]], SemanticInfo) => A,
              appUnary : (SLabel, meta.Name, Expr[IdentInfo, SemanticInfo], SemanticInfo) => A,
              new_ : (SLabel, meta.Type, List[List[Expr[IdentInfo, SemanticInfo]]], SemanticInfo) => A,
              select : (SLabel, Expr[IdentInfo, SemanticInfo], meta.Term.Name, IdentInfo, SemanticInfo) => A,
              thisApply : (SLabel, List[List[Expr[IdentInfo, SemanticInfo]]]) => A,
              this_ : (SLabel, meta.Name, SemanticInfo) => A,
              super_ : (SLabel, meta.Name, meta.Name, SemanticInfo) => A,
              tuple : (SLabel, List[Expr[IdentInfo, SemanticInfo]], SemanticInfo) => A,
              if_ : (SLabel, Expr[IdentInfo, SemanticInfo], Expr[IdentInfo, SemanticInfo], SemanticInfo) => A,
              ifElse : (SLabel, Expr[IdentInfo, SemanticInfo], Expr[IdentInfo, SemanticInfo], Expr[IdentInfo, SemanticInfo], SemanticInfo) => A,
              while_ : (SLabel, Expr[IdentInfo, SemanticInfo], Expr[IdentInfo, SemanticInfo], SemanticInfo) => A,
              for_ : (SLabel, List[meta.Enumerator], Expr[IdentInfo, SemanticInfo], SemanticInfo) => A,
              forYield : (SLabel, List[meta.Enumerator], Expr[IdentInfo, SemanticInfo], SemanticInfo) => A,
              returnUnit : (SLabel, SemanticInfo) => A,
              return_ : (SLabel, Expr[IdentInfo, SemanticInfo], SemanticInfo) => A,
              throw_ : (SLabel, Expr[IdentInfo, SemanticInfo], SemanticInfo) => A,
              block : (SLabel, List[Statement[IdentInfo, SemanticInfo]], SemanticInfo) => A,
              nOther : (SLabel, meta.Term, SemanticInfo) => A) : A =
    this match {
      case Literal(sl, lit, t) => literal(sl, lit, t)
      case Ident(sl, name, sym, t) => ident(sl, name, sym, t)
      case Assign(sl, lhs, rhs, t) => assign(sl, lhs, rhs, t)
      case App(sl, f, args, t) => app(sl, f, args, t)
      case AppInfix(sl, lhs, op, rhs, t) => appInfix(sl, lhs, op, rhs, t)
      case AppUnary(sl, op, rhs, t) => appUnary(sl, op, rhs, t)
      case New(sl, cls, argss, t) => new_(sl, cls, argss, t)
      case Select(sl, qual, name, sym, t) => select(sl, qual, name, sym, t)
      case ThisApply(sl, argss) => thisApply(sl, argss)
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
}

case class Literal[IdentInfo, SemanticInfo](l : SLabel, sugared : meta.Lit, info : SemanticInfo) extends Expr[IdentInfo, SemanticInfo] {
  def label : SLabel = l

  override def toString : String = sugared.toString
}

case class Ident[IdentInfo, SemanticInfo](l : SLabel, name : String, identInfo : IdentInfo, info : SemanticInfo) extends Expr[IdentInfo, SemanticInfo] {
  def label : SLabel = l

  override def toString : String = name
}

case class Assign[IdentInfo, SemanticInfo](l : SLabel, lhs : Expr[IdentInfo, SemanticInfo], rhs : Expr[IdentInfo, SemanticInfo], info : SemanticInfo) extends Expr[IdentInfo, SemanticInfo] {
  def label : SLabel = l

  override def toString : String = s"$lhs = $rhs"
}

case class App[IdentInfo, SemanticInfo](l : SLabel, method : Expr[IdentInfo, SemanticInfo], args : List[Expr[IdentInfo, SemanticInfo]], info : SemanticInfo) extends Expr[IdentInfo, SemanticInfo] {
  def label : SLabel = l

  override def toString : String = {
    val args_ : String = args.mkString("(",",",")")
    s"$method$args_"
  }
}

case class AppInfix[IdentInfo, SemanticInfo](l : SLabel, lhs : Expr[IdentInfo, SemanticInfo], method : meta.Term.Name, args : List[Expr[IdentInfo, SemanticInfo]], info : SemanticInfo) extends Expr[IdentInfo, SemanticInfo] {
  def label : SLabel = l

  override def toString : String = {
    val args_ : String = args match {
      case List(arg) => arg.toString
      case _ => args.mkString("(",",",")")
    }
    s"$lhs $method $args_"
  }
}

case class AppUnary[IdentInfo, SemanticInfo](l : SLabel, method : meta.Name, arg : Expr[IdentInfo, SemanticInfo], info : SemanticInfo) extends Expr[IdentInfo, SemanticInfo] {
  def label : SLabel = l
}

case class New[IdentInfo, SemanticInfo](l : SLabel, tpe : meta.Type, args : List[List[Expr[IdentInfo, SemanticInfo]]], info : SemanticInfo) extends Expr[IdentInfo, SemanticInfo] {
  def label : SLabel = l
}

/*
case class NewAnonymous[IdentInfo, SemanticInfo](l : SLabel, template : Template[IdentInfo, SemanticInfo], info : SemanticInfo) extends Expr[IdentInfo, SemanticInfo] {
  def label : SLabel = l
}
*/

case class Select[IdentInfo, SemanticInfo](l : SLabel, qualifier : Expr[IdentInfo, SemanticInfo], sel : meta.Term.Name, identInfo : IdentInfo, info : SemanticInfo) extends Expr[IdentInfo, SemanticInfo] {
  def label : SLabel = l

  override def toString : String = s"$qualifier.$sel"
}

case class ThisApply[IdentInfo, SemanticInfo](l : SLabel, argss : List[List[Expr[IdentInfo, SemanticInfo]]]) extends Expr[IdentInfo, SemanticInfo] {
  def label : SLabel = l
}

case class This[IdentInfo, SemanticInfo](l : SLabel, qualifier : meta.Name, info : SemanticInfo) extends Expr[IdentInfo, SemanticInfo] {
  def label : SLabel = l
}

case class Super[IdentInfo, SemanticInfo](l : SLabel, thisp : meta.Name, superp : meta.Name, info : SemanticInfo) extends Expr[IdentInfo, SemanticInfo] {
  def label : SLabel = l
}

case class Tuple[IdentInfo, SemanticInfo](l : SLabel, components : List[Expr[IdentInfo, SemanticInfo]], info : SemanticInfo) extends Expr[IdentInfo, SemanticInfo] {
  def label : SLabel = l
}

case class If[IdentInfo, SemanticInfo](l : SLabel, pred : Expr[IdentInfo, SemanticInfo], thenE : Expr[IdentInfo, SemanticInfo], info : SemanticInfo) extends Expr[IdentInfo, SemanticInfo] {
  def label : SLabel = l
}

case class IfElse[IdentInfo, SemanticInfo](l : SLabel, pred : Expr[IdentInfo, SemanticInfo], thenE : Expr[IdentInfo, SemanticInfo], elseE : Expr[IdentInfo, SemanticInfo], info : SemanticInfo) extends Expr[IdentInfo, SemanticInfo] {
  def label : SLabel = l
}

case class While[IdentInfo, SemanticInfo](l : SLabel, pred : Expr[IdentInfo, SemanticInfo], body : Expr[IdentInfo, SemanticInfo], info : SemanticInfo) extends Expr[IdentInfo, SemanticInfo] {
  def label : SLabel = l
}

case class For[IdentInfo, SemanticInfo](l : SLabel, enums : List[meta.Enumerator], body : Expr[IdentInfo, SemanticInfo], info : SemanticInfo) extends Expr[IdentInfo, SemanticInfo] {
  def label : SLabel = l
}

case class ForYield[IdentInfo, SemanticInfo](l : SLabel, enums : List[meta.Enumerator], body : Expr[IdentInfo, SemanticInfo], info : SemanticInfo) extends Expr[IdentInfo, SemanticInfo] {
  def label : SLabel = l
}

case class ReturnUnit[IdentInfo, SemanticInfo](l : SLabel, info : SemanticInfo) extends Expr[IdentInfo, SemanticInfo] {
  def label : SLabel = l
}

case class Return[IdentInfo, SemanticInfo](l : SLabel, e : Expr[IdentInfo, SemanticInfo], info : SemanticInfo) extends Expr[IdentInfo, SemanticInfo] {
  def label : SLabel = l
}

case class Throw[IdentInfo, SemanticInfo](l : SLabel, e : Expr[IdentInfo, SemanticInfo], info : SemanticInfo) extends Expr[IdentInfo, SemanticInfo] {
  def label : SLabel = l
}

case class Block[IdentInfo, SemanticInfo](l : SLabel, statements : List[Statement[IdentInfo, SemanticInfo]], info : SemanticInfo) extends Expr[IdentInfo, SemanticInfo] {
  def label : SLabel = l
}

/*
case class Lambda[IdentInfo, SemanticInfo](l : SLabel, args : List[Expr[IdentInfo, SemanticInfo]], body : Expr[IdentInfo, SemanticInfo], info : SemanticInfo) extends Expr[IdentInfo, SemanticInfo] {
  def label : SLabel = l
}
*/

case class Other[IdentInfo, SemanticInfo](l : SLabel, expr : meta.Term, info : SemanticInfo) extends Expr[IdentInfo, SemanticInfo] {
  def label : SLabel = l
}

final case class Template[IdentInfo, SemanticInfo](early : List[Statement[IdentInfo, SemanticInfo]], inits : List[Initializer[IdentInfo, SemanticInfo]], self : meta.Self, statements : List[Statement[IdentInfo, SemanticInfo]])

final case class Initializer[IdentInfo, SemanticInfo](tpe : meta.Type, argss : List[List[Expr[IdentInfo, SemanticInfo]]])

sealed abstract class Reference[IdentInfo, SemanticInfo]

final case class RefThis[IdentInfo, SemanticInfo](expr : This[IdentInfo, SemanticInfo]) extends Reference[IdentInfo, SemanticInfo]
final case class RefSuper[IdentInfo, SemanticInfo](expr : Super[IdentInfo, SemanticInfo]) extends Reference[IdentInfo, SemanticInfo]
final case class RefIdent[IdentInfo, SemanticInfo](expr : Ident[IdentInfo, SemanticInfo]) extends Reference[IdentInfo, SemanticInfo]
final case class RefSelect[IdentInfo, SemanticInfo](expr : Select[IdentInfo, SemanticInfo]) extends Reference[IdentInfo, SemanticInfo]
final case class RefAppUnary[IdentInfo, SemanticInfo](expr : AppUnary[IdentInfo, SemanticInfo]) extends Reference[IdentInfo, SemanticInfo]

object Expr {
  case class St
    ( pGen : PLabelGen
    , sGen : SLabelGen
    , dGen : DLabelGen
    , exprs : TypedExprMap
    , symbolTable : SymbolTable
    , decls : TypedDeclMap
    , defns : TypedDefnMap
    , topLevels : List[Either[TypedDecl, TypedDefn]]
    , callTargets : Map[SLabel, List[Either[DLabel, SLabel]]]
    )

  type Log = List[String]
  private implicit val logMonoid : Monoid[List[String]] = scalaz.std.list.listMonoid[String]

  type Exception[A] = String \/ A
  type Logger[A] = WriterT[Log, Exception, A]
  type NodeGen[A] = StateT[St, Logger, A]

  private val logTransInstance : MonadTrans[({ type λ[M[_], A] = WriterT[Log, M, A] })#λ] = WriterT.writerTHoist
  private val stateInstance : MonadState[NodeGen, St] = IndexedStateT.stateTMonadState[St, Logger]

  private val transInstance : MonadTrans[({ type λ[M[_], A] = StateT[St, M, A] })#λ] = IndexedStateT.StateMonadTrans
  private val m : Monad[NodeGen] = stateInstance

  val nodeGenMonadInstance : Monad[NodeGen] = stateInstance

  private def raiseError[A](e : String) : NodeGen[A] =
    transInstance.liftM[Logger, A](logTransInstance.liftM[Exception, A](\/.DisjunctionInstances1.raiseError[A](e)))

  private def log(m : String) : NodeGen[Unit] =
    transInstance.liftM[Logger, Unit](scalaz.WriterT.writerTMonadListen[Log, Exception].tell(List(m)))
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

  private val genSLabel : NodeGen[SLabel] =
    modifySt{ s => (s.sGen.head, s.copy(sGen = s.sGen.tail)) }

/*
  private val genPLabel : NodeGen[PLabel] =
    modifySt{ s => (s.pGen.head, s.copy(pGen = s.pGen.tail)) }
*/

  private val genDLabel : NodeGen[DLabel] =
    modifySt { s => (s.dGen.head, s.copy(dGen = s.dGen.tail)) }

  private def genDLabelFor(sym : Symbol) : NodeGen[DLabel] =
    modifySt{ s =>
      s.symbolTable.get(sym) match {
        case Some(dl) => (dl, s)
        case None => (s.dGen.head, s.copy(dGen = s.dGen.tail, symbolTable = s.symbolTable + ((sym, s.dGen.head)))) 
      }
    }

  private def addSymbol(sym : Symbol, l : DLabel) : NodeGen[Unit] =
    modifySt { s => ((), s.copy(symbolTable = s.symbolTable + ((sym, l)))) }

  private def addCallTarget(l : SLabel, callee : DLabel) : NodeGen[Unit] =
    modifySt { s => ((), s.copy(callTargets = updateMap(s.callTargets, l, Left(callee) :: (_ : List[Either[DLabel, SLabel]]), List(Left(callee))))) }

  private def updateMap[K,V](m : Map[K, V], k : K, f : V => V, e : => V) : Map[K, V] =
    m.get(k) match {
      case None => m.updated(k, e)
      case Some(x) => m.updated(k, f(x))
    }

  private def label[E <: TypedExpr](f : SLabel => E) : NodeGen[E] =
    for (l <- genSLabel;
         n = f(l);
         _ <- modifySt{ s => ((), s.copy(exprs = s.exprs.updated(l, n))) })
    yield n

/*  private def labelPat(f : PLabel => Pat) : NodeGen[Pat] = 
    for (l <- genPLabel;
         p = f(l))
    yield p

  private def collectMethod(t : Tree) : NodeGen[Unit] =
    if (t.symbol != null && t.symbol.isMethod)
      m.void(genDLabelFor(t.symbol))
    else
      m.pure(())
*/
  private def singleton[A, B](as : List[A])(f : A => B)(err : => B) : B =
    as match {
      case List(a) => f(a)
      case _ => err
    }

  private def symbolsOf(trees : List[Tree]) : List[Symbol] =
    for (t <- trees; s = t.symbol; if s != null) yield s

  def genExpr(sugared : meta.Term, ts : List[Tree]) : NodeGen[TypedExpr] = {
    val samePos : List[Tree] = searchSamePosition(sugared.pos, ts, StopOnExactPosition())
    val types : List[scalac.Type] = samePos map (_.tpe)
    lazy val symbols : List[Symbol] = symbolsOf(samePos)
    val childSearchScope : List[Tree] = if (!samePos.isEmpty) samePos else ts
    val childSamePos : (meta.Tree, PositionSearchSetting) => List[Tree] =
        (child, setting) => searchSamePosition(child.pos, childSearchScope, setting)

    def resugarChild(child : meta.Term) : NodeGen[TypedExpr] =
      genExpr(child, childSearchScope)

    Control.exprCataMeta[NodeGen[TypedExpr]](
        lit => label(Literal(_, lit, types)) // literal
      , name => singleton(symbols){symbol => label[TypedExpr](Ident(_, name, symbol, types))} // name
                  (raiseError(s"Found ${symbols.length} matching ast symbols for identifier $name, expected 1."))
      , metaComponents => // tuple
          for (components <- forM(metaComponents)(resugarChild(_));
               tuple <- label(Tuple(_, components, types)))
          yield tuple
      , (metaType, _, metaArgss) => // new
          for (argss <- forM(metaArgss){args =>
                   forM(args)(resugarChild(_))
                 };
               new_ <- label(New(_, metaType, argss, types)))
          yield new_
        // metaName should be inspected for symbols
      , metaName => label(This(_, metaName, types)) // this
        // metaName should be inspected for symbols
      , (metaQualifier, metaName) => // select
          singleton(symbols){symbol =>
            for (qualifier <- resugarChild(metaQualifier);
                 select <- label[TypedExpr](Select(_, qualifier, metaName, symbol, types)))
            yield select}
            (raiseError(s"Found ${symbols.length} matching ast symbols for select $metaName, expected 1."))
      , (metaFun, metaArgs) => // apply
          for (fun <- resugarChild(metaFun);
               args <- forM(metaArgs)(resugarChild(_));
               app <- label(App(_, fun, args, types));
               targets <- mapM(genDLabelFor, for (f <- childSamePos(metaFun, StopOnExactPosition()); if f.symbol != null) yield f.symbol);
               _ <- mapM((t : DLabel) => addCallTarget(app.label, t), targets))
          yield app
        // metaOp should be inspected for symbols
      , (metaArgLeft, metaOp, _, metaArgsRight) => // applyInfix
          for (argLeft <- resugarChild(metaArgLeft);
               argsRight <- forM(metaArgsRight)(resugarChild(_));
               appInfix <- label(AppInfix(_, argLeft, metaOp, argsRight, types));
               desugaredSelects = searchSamePosition(meta.Position.Range(sugared.pos.input, metaArgLeft.pos.start, metaOp.pos.end), samePos, SearchChildrenOnExactPosition());
               targets = symbolsOf(desugaredSelects);
               _ <- mapM[NodeGen, Symbol, Unit]((t : Symbol) => for (l <- genDLabelFor(t); _ <- addCallTarget(appInfix.label, l)) yield (), targets))
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
          for (stats <- forM(metaStats)(stat => genStat(stat, childSamePos(stat, StopOnExactPosition())));
               block <- label(Block(_, stats, types)))
          yield block
      , (metaTerm) => label(Other(_, metaTerm, types)) // other
      , sugared
      )
  }

  private def putDecl[D <: TypedDecl](genLabel : NodeGen[DLabel])(f : DLabel => NodeGen[D]) : NodeGen[D] =
    for (l <- genLabel;
         decl <- f(l);
         _ <- modifySt { st => (decl, st.copy( decls = st.decls + (l -> decl)
                                             , topLevels = if (decl.isTopLevel)
                                                             Left(decl) :: st.topLevels
                                                           else
                                                             st.topLevels
                                             )
                               )
                       }
         )
    yield decl

  private def putDefn[D <: TypedDefn](genLabel : NodeGen[DLabel])(f : DLabel => NodeGen[D]) : NodeGen[D] =
    for (l <- genLabel;
         defn <- f(l);
         _ <- modifySt { st => (defn, st.copy( defns = st.defns + (l -> defn)
                                             , topLevels = if (defn.isTopLevel)
                                                             Right(defn) :: st.topLevels
                                                           else
                                                             st.topLevels
                                             )
                               )
                       }
         )
    yield defn

  private def putDefn[D <: TypedDefn](m : NodeGen[D]) : NodeGen[D] =
    for (defn <- m;
         _ <- modifySt { st => (defn, st.copy( defns = st.defns + (defn.label -> defn)
                                             , topLevels = if (defn.isTopLevel)
                                                             Right(defn) :: st.topLevels
                                                           else
                                                             st.topLevels
                                             )
                               )
                       }
         )
    yield defn

  sealed abstract class PositionSearchSetting
  case class SearchChildrenOnExactPosition() extends PositionSearchSetting
  case class StopOnExactPosition() extends PositionSearchSetting

  private def searchSamePosition(metaPos : meta.Position, roots : List[Tree], setting : PositionSearchSetting) : List[Tree] = {
    def includes(p : scalac.Position, what : meta.Position) : Boolean =
      p.isRange && (p.start <= what.start && what.end <= p.end)

    def equals(p : scalac.Position, p2 : meta.Position) : Boolean =
      p.isRange && (p.start == p2.start && p.end == p2.end)

    val searchChildrenOnExactPosition : Boolean =
      setting == SearchChildrenOnExactPosition()

    def search(ts : List[Tree], visited : Set[Tree]) : (List[Tree], Set[Tree]) =
      ts.foldLeft((List[Tree](), visited))(
        (acc, t) => {
          val ((found, visited2), searchChildren) : ((List[Tree], Set[Tree]), Boolean) = inspect(t, acc._2)
          val acc2 : (List[Tree], Set[Tree]) = scalaz.std.tuple.tuple2Bitraverse.bimap(acc)(found ++ _, visited2 ++ _)
          if (searchChildren)
            scalaz.std.tuple.tuple2Bitraverse.leftMap(search(t.children, acc2._2))(acc2._1 ++ _)
          else
            acc2
        }
      )

    def inspect(t : Tree, visited : Set[Tree]) : ((List[Tree], Set[Tree]), Boolean) =
      if (includes(t.pos, metaPos))
        if (!(visited contains t)) {
          if (equals(t.pos, metaPos))
            ((List(t), visited + t), searchChildrenOnExactPosition)
          else
            ((List(), visited + t), true)
        }
        else
          ((List(), visited), false)
      else
        ((List(), visited), false)

    search(roots, Set[Tree]())._1
  }

  def resugar(sugared : meta.Source, desugared : Tree) : NodeGen[Unit] = {
    val metaStats : List[meta.Stat] = sugared.stats
    forM_(metaStats){ stat => genStat(stat, searchSamePosition(stat.pos, List(desugared), SearchChildrenOnExactPosition())) }
  }

  def genStat(sugared : meta.Stat, ts : List[Tree]) : NodeGen[TypedStatement] = {
    Control.metaStatKindCata(
        term => // term
          stateInstance.map(genExpr(term, ts))(Statement.fromExpr(_))
      , decl =>  // decl
          stateInstance.map(genDecl(decl, ts))(Statement.fromDecl(_))
      , defn => // definition
          stateInstance.map(genDefn(defn, ts))(Statement.fromDefn(_))
      , sndctr => // secondary constructor
          stateInstance.map(genCtor(sndctr, ts))(Statement.fromDefn(_))
      , pobj => // package object
          stateInstance.map(genPkgObj(pobj, ts))(Statement.fromDefn(_))
      , pkg => // package
          stateInstance.map(genPkg(pkg, ts))(Statement.fromDefn(_))
      , imprt => // import
          stateInstance.map(genImport(imprt, ts))(Statement.fromDecl(_))
      , sugared
      )
  }
  def genDefn(sugared : meta.Defn, scope : List[Tree]) : NodeGen[TypedDefn] = {
    // used for definitions other than vals or vars
    lazy val samePos : List[Tree] = searchSamePosition(sugared.pos, scope, SearchChildrenOnExactPosition())

    val childScope : (List[Tree], List[Tree]) => List[Tree] =
      (samePos, ascdendantScope) =>
        if (!samePos.isEmpty)
          samePos
        else
          ascdendantScope

    lazy val symbols : List[Symbol] = symbolsOf(samePos)

    Control.defnCataMeta(
        (mods, pats, oDeclType, metaRhs) => _ => // value
          putDefn(genDLabel){ l => {
              val valSymbols : List[Symbol] = extractVarValSymbols(pats, sugared, scope)
              val numVals : Int = pats.flatMap(Control.patNames).length
              for( _ <- m.whenM(valSymbols.length != numVals)
                               (log(s"Number of values ($numVals) and symbols (${valSymbols.length}) differ in definition of $pats at ${sugared.pos}."));
                   _ <- forM_(valSymbols)(addSymbol(_, l));
                   rhs <- genExpr(metaRhs, childScope(samePos, scope)))
              yield Defn.Val(l, mods, pats, valSymbols, oDeclType, rhs)
            }
          }
      , (mods, pats, oDeclType, oMetaRhs) => _ => // variable
          putDefn(genDLabel){ l => {
              val varSymbols : List[Symbol] = extractVarValSymbols(pats, sugared, scope)
              val optionTraverse : Traverse[Option] = scalaz.std.option.optionInstance
              val numVars : Int = pats.flatMap(Control.patNames).length
              for( _ <- m.whenM(varSymbols.length != numVars)
                               (log(s"Number of variables ($numVars) and symbols (${symbols.length}) differ in definition of $pats at ${sugared.pos}."));
                   _ <- forM_(varSymbols)(addSymbol(_, l));
                   oRhs <- optionTraverse.traverse(oMetaRhs)(metaRhs => genExpr(metaRhs, childScope(samePos, scope))))
              yield Defn.Var(l, mods, pats, varSymbols, oDeclType, oRhs)
            }
          }
      , (mods, name, typeParams, paramss, oDeclType, metaBody) => _ => // method
          putDefn(for (l <- singleton(samePos){
                              case t : scalac.DefDef => genDLabelFor(t.symbol)
                              case _ => log(s"The matching ast for method definition $name is not a method."); genDLabel
                            }
                            { log(s"Found ${scope.length} matching asts for method definition $name, expected 1.");
                              val symbols : List[Symbol] = symbolsOf(samePos);
                              for (l <- genDLabel;
                                   _ <- forM_(symbols)(addSymbol(_, l)))
                              yield l
                            };
                       body <- genExpr(metaBody, childScope(samePos, scope)))
                  yield Defn.Method(l, symbols, mods, name, typeParams, paramss, oDeclType, body)
                 )
       , (mods, name, _, paramss, _, metaBody) => _ => // macro
          putDefn(
            for (l <- singleton(samePos){
                        case m : scalac.DefDef => genDLabelFor(m.symbol)
                        case _ => log(s"The matching ast for macro definition $name is not a macro."); genDLabel
                      }
                      { log(s"Found ${scope.length} matching asts for macro definition $name, expected 1.");
                        for (l <- genDLabel;
                             _ <- forM_(symbols)(addSymbol(_, l)))
                        yield l
                      };
                 b <- genExpr(metaBody, childScope(samePos, scope)))
            yield Defn.Macro(l, symbols, mods, name, paramss, b)
          )
      , (mods, name, typeParams, metaBody) => _ => // type
          putDefn(
            for (l <- genDLabel)
            yield Defn.Type(l, symbols, mods, name, typeParams, metaBody)
          )
      , (mods, name, _, _, metaBody) => _ => // class
          putDefn(genDLabel){ l => {
              val matchingClasses : List[Tree] = samePos.filter(t => t.symbol != null && t.symbol.isClass)
              val symbols : List[Symbol] = symbolsOf(matchingClasses)
              for (_ <- (m.unlessM(matchingClasses.length == 1)
                          (log(s"Found ${matchingClasses.length} matching asts for the class definition $name, expected 1.")));
                  _ <- forM_(symbols)(s => addSymbol(s, l));
                  statements <- resugarTemplate(metaBody, matchingClasses))
               yield Defn.Class(l, symbols, mods, name, statements)
            }
          }
      , (mods, name, _, _, metaBody) => _ => // trait
          putDefn(genDLabel){ l => {
              val matchingTraits : List[Tree] = samePos.filter(t => t.symbol != null && !t.symbol.isClass)
              val symbols : List[Symbol] = symbolsOf(matchingTraits)
              for (_ <- (m.unlessM(matchingTraits.length == 1)
                          (log(s"Found ${matchingTraits.length} matching asts for the trait definition $name, expected 1.")));
                   _ <- forM_(symbols)(addSymbol(_, l));
                   statements <- resugarTemplate(metaBody, matchingTraits))
              yield Defn.Trait(l, symbols, mods, name, statements)
            }
          }
      , (mods, name, metaBody) => _ => // object
          putDefn(genDLabel){ l => {
              val matchingObjects : List[Tree] = samePos.filter{
                  case _ : scalac.ModuleDef => true
                  case _ => false
                }
              val symbols : List[Symbol] = symbolsOf(matchingObjects);
              for (_ <- (m.unlessM(matchingObjects.length == 1)
                          (log(s"Found ${matchingObjects.length} matching asts for the object definition $name, expected 1.")));
                   _ <- forM_(symbols)(addSymbol(_, l));
                   statements <- resugarTemplate(metaBody, matchingObjects))
              yield Defn.Object(l, symbols, mods, name, statements)
            }
          }
      , sugared
      )
  }

  private def genCtor(ctor : meta.Ctor.Secondary, scope : List[Tree]) : NodeGen[TypedDefn] = {
    val meta.Ctor.Secondary(mods, name, paramss, meta.Init(_, _, argss), stats) = ctor
    val samePos : List[Tree] = searchSamePosition(ctor.pos, scope, SearchChildrenOnExactPosition())
    val symbols : List[Symbol] = symbolsOf(samePos)
    val childScope : List[Tree] = if (!samePos.isEmpty) samePos else scope
    putDefn(
      for (l <- genDLabel;
           _ <- forM_(symbols)(addSymbol(_, l));
           ctorArgss <- forM(argss)(args => forM(args)(genExpr(_, childScope)));
           initializer <- label(ThisApply(_, ctorArgss));
           body <- forM(stats)(genStat(_, childScope)))
      yield Defn.SecondaryCtor(l, symbols, mods, name, paramss, (initializer, body))
    )
  }

  private def extractVarValSymbols(pats : List[meta.Pat], metaDecl : meta.Decl, scope : List[Tree]) : List[Symbol] = {
    val extendStart : meta.Position => meta.Position =
      pos => meta.Position.Range(metaDecl.pos.input, metaDecl.pos.start, pos.end)
    val extendEnd : meta.Position => meta.Position =
      pos => meta.Position.Range(metaDecl.pos.input, pos.start, metaDecl.pos.end)

    val patternsPos : List[meta.Position] = pats match {
      case List(pat) => List(extendStart(extendEnd(pat.pos)))
      case _ => extendStart(pats.head.pos) :: extendEnd(pats.last.pos) :: pats.tail.init.map(_.pos)
    }

    val valsSamePos : List[Tree] = patternsPos.flatMap(searchSamePosition(_, scope, SearchChildrenOnExactPosition())).filter{
      case _ : scalac.ValDef => true
      case _ => false
    }
    symbolsOf(valsSamePos)
  }

  private def extractVarValSymbols(pats : List[meta.Pat], metaDef : meta.Defn, scope : List[Tree]) : List[Symbol] = {
    val extendStart : meta.Position => meta.Position =
      pos => meta.Position.Range(metaDef.pos.input, metaDef.pos.start, pos.end)
    val extendEnd : meta.Position => meta.Position =
      pos => meta.Position.Range(metaDef.pos.input, pos.start, metaDef.pos.end)
    // if metaDef is like var x : Int = _, then the placeholder is not included in
    // the range of the scalac tree
    val extendEndUntilPlaceHolder : meta.Position => meta.Position =
      metaDef match {
        // if metaDef is like var x : Int = _, then type must be given
        case meta.Defn.Var(_, _, Some(typ), None) =>
          pos => meta.Position.Range(metaDef.pos.input, pos.start, typ.pos.end)
        case _ =>
          extendEnd
      }
    // extends the first pattern's starting position and last pattern's ending position
    val patternsPos : List[meta.Position] = pats match {
      case List(pat) => 
        val names : List[meta.Name] = Control.patNames(pat)
        if (names.length == 1) 
          List(extendStart(extendEndUntilPlaceHolder(pat.pos)))
        else 
          names.map(_.pos)
      case _ =>
        val headNames : List[meta.Name] = Control.patNames(pats.head)
        val headPoss : List[meta.Position] =
          if (headNames.length == 1) 
            List(extendStart(pats.head.pos))
          else
            headNames.map(_.pos)
        val lastNames : List[meta.Name] = Control.patNames(pats.last)
        val lastPoss : List[meta.Position] = 
          if (lastNames.length == 1) 
            List(extendEndUntilPlaceHolder(pats.last.pos)) 
          else
            lastNames.map(_.pos)
        headPoss ++ lastPoss ++ pats.tail.init.map(_.pos)
    }
    // filter filters out getter and setter methods of vals and vars
    val valsSamePos : List[Tree] = patternsPos.flatMap(searchSamePosition(_, scope, SearchChildrenOnExactPosition())).filter{
      case _ : scalac.ValDef => true
      case _ => false
    }
    symbolsOf(valsSamePos)
  }

  /**
   * Elements of 'ts' need not represent templates. It suffices if they have Template descendants.
   */
  def resugarTemplate(sugared : meta.Template, scope : List[Tree]) : NodeGen[List[TypedStatement]] = {
    val metaStatements : List[meta.Stat] = sugared.stats
    val listTraverse : Traverse[List] = scalaz.std.list.listInstance
    m.traverse(metaStatements){ statement => genStat(statement, scope) }(listTraverse)
  }

  def genPkg(sugared : meta.Pkg, ts : List[Tree]) : NodeGen[Defn.TypedPackage] = {
    val symbols : List[Symbol] = symbolsOf(ts)
    putDefn(genDLabel){ l =>
      for ( _ <- forM_(symbols)(addSymbol(_, l));
            statements <- forM(sugared.stats)( stat => genStat(stat, searchSamePosition(stat.pos, ts, SearchChildrenOnExactPosition())) ))
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

  def genImport(sugared : meta.Import, ts : List[Tree]) : NodeGen[Decl.TypedImport] = {
    val symbols : List[Symbol] = symbolsOf(ts)
    putDecl(genDLabel){ l =>
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

  def genPkgObj(sugared : meta.Pkg.Object, ts : List[Tree]) : NodeGen[Defn.TypedPackageObject] = {
    val meta.Pkg.Object(mods, name, metaBody) = sugared
    val symbols : List[Symbol] = symbolsOf(ts)
    putDefn(genDLabel){ l =>
      for (_ <- forM_(symbols)(addSymbol(_, l));
           body <- resugarTemplate(metaBody, ts))
      yield Defn.PackageObject(l, symbols, mods, name, body)
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

  def genDecl(sugared : meta.Decl, scope : List[Tree]) : NodeGen[TypedDecl] = {
    val samePos : List[Tree] = searchSamePosition(sugared.pos, scope, SearchChildrenOnExactPosition())
    val symbols : List[Symbol] = symbolsOf(samePos)
    Control.declCataMeta(
        (mods, pats, declType) => _ => { // val
          val valSymbols : List[Symbol] = extractVarValSymbols(pats, sugared, scope)
          val numVals : Int = pats.flatMap(Control.patNames).length
          putDecl(genDLabel){ l =>
            for (_ <- m.whenM(valSymbols.length != numVals)
                             (log(s"Number of values ($numVals) and symbols (${valSymbols.length}) differ in definition of $pats at ${sugared.pos}."));
                 _ <- forM_(valSymbols)(addSymbol(_, l)))
            yield Decl.Val(l, mods, pats, valSymbols, declType)
          }
        }
//            , raiseError("For a value declaration statement, there is a matching desugared ast which is not a value declaration.")
      , (mods, pats, declType) => _ => { // var
          val varSymbols : List[Symbol] = extractVarValSymbols(pats, sugared, scope)
          val numVars : Int = pats.flatMap(Control.patNames).length
          putDecl(genDLabel){ l =>
            for (_ <- m.whenM(varSymbols.length != numVars)
                             (log(s"Number of variables ($numVars) and symbols (${varSymbols.length}) differ in definition of $pats at ${sugared.pos}."));
                 _ <- forM(varSymbols)(addSymbol(_, l)))
            yield Decl.Var(l, mods, pats, varSymbols, declType)
          }
        }
          //raiseError("For a variable declaration statement, there is a matching desugared ast which is not a variable declaration nor is a method.")
      , (mods, name, typeParams, argss, declType) => _ => // method
          putDecl(genDLabel){ l =>
            for (_ <- forM_(symbols)(addSymbol(_, l)))
            yield Decl.Method(l, symbols, mods, name, typeParams, argss, declType)
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
          putDecl(genDLabel){ l =>
            for(_ <- forM_(symbols)(addSymbol(_, l)))
            yield Decl.Type(l, symbols, mods, name, typeParams, bounds)
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

/*
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
      , (_, pat) => // binding
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
*/

  def runNodeGen[A](m : NodeGen[A]) : String \/ (List[String], (St, A)) = {
    val startSt : St = St(PLabel.stream, SLabel.stream, DLabel.stream, Map(), Map(), Map(), Map(), List(), Map())
    m.run(startSt).run
  }

  def prettyPrint[IdentInfo, SemanticInfo](e : Expr[IdentInfo, SemanticInfo]) : Doc = {
    import PrettyPrint.{lparen, rparen, paren}
    def prettyArgs(args : List[Expr[IdentInfo, SemanticInfo]]) : Doc =
      paren(Doc.intercalate(Doc.text(",") + Doc.lineOrSpace, args.map(prettyPrint)).aligned)

    def prettyArgss(argss : List[List[Expr[IdentInfo, SemanticInfo]]]) : Doc =
      Doc.intercalate(Doc.lineOrEmpty,
                      argss.map(args => lparen + Doc.intercalate(Doc.text(","), args.map(prettyPrint)) + rparen))

    e.cata(
        (_, lit, _) => // literal
          Doc.str(lit)
      , (_, ident, _, _) => // identifier reference
          Doc.text(ident)
      , (_, lhs, rhs, _) => // assignment
          prettyPrint(lhs) + Doc.space + Doc.str("=") + Doc.lineOrSpace + prettyPrint(rhs)
      , (_, m, args, _) => // application
          prettyPrint(m) + prettyArgs(args)
      , (_, lhs, op, args, _) => // infix application
          prettyPrint(lhs) + Doc.space + Doc.str(op) + Doc.lineOrSpace + 
            (args match {
              case List(arg) => prettyPrint(arg)
              case _ => prettyArgs(args)
            })
      , (_, op, arg, _) => // unary application
          Doc.str(op) + Doc.space + prettyPrint(arg)
      , (_, class_, argss, _) => // new
          Doc.text("new") + Doc.space + Doc.str(class_) + prettyArgss(argss)
      , (_, obj, termName, _, _) => // selection
          prettyPrint(obj) + Doc.text(".") + Doc.str(termName)
      , (_, argss) => // this(...) application
          Doc.str("this") + prettyArgss(argss)
      , (_, typeName, _) => // this
          Doc.str(typeName) + Doc.text("this")
      , (_, _, _, _) => // super
          Doc.text("super")
      , (_, comps, _) => // tuple
          prettyArgs(comps)
      , (_, pred, thenE, _) => // if-then
          Doc.text("if") + Doc.space + lparen + prettyPrint(pred) + rparen + Doc.space +
          prettyPrint(thenE)
      , (_, pred, thenE, elseE, _) => // if-then-else
          Doc.text("if") + Doc.space + lparen + prettyPrint(pred) + rparen + Doc.space +
          prettyPrint(thenE) + Doc.space + Doc.str("else") + Doc.space + prettyPrint(elseE)
      , (_, pred, body, _) => // while loop
          Doc.text("while") + Doc.space + lparen + prettyPrint(pred) + rparen + Doc.space +
          prettyPrint(body)
      , (_, enums, body, _) => // for loop
         Doc.text("for") + lparen + Doc.cat(enums.map(e => Doc.str(e) + Doc.text(";"))) + rparen + Doc.space + prettyPrint(body)
      , (_, enums, body, _) => // for-yield loop
         Doc.text("for") + lparen + Doc.cat(enums.map(e => Doc.str(e) + Doc.text(";"))) + rparen + Doc.space + Doc.text("yield") + Doc.space + prettyPrint(body)
      , (_, _) => // return
         Doc.text("return")
      , (_, expr, _) => // return with expr
         Doc.text("return") + Doc.space + prettyPrint(expr)
      , (_, expr, _) => // throw
         Doc.text("throw") + Doc.space + prettyPrint(expr)
      , (_, stmts, _) => // block
         Doc.text("{") + Doc.stack(Doc.empty :: stmts.map(Statement.prettyPrint)).nested(2) + Doc.line + Doc.text("}")
      , (_, expr, _) => // other expression
         Doc.str(expr)
      )
  }

  def prettyPrintLint(e : TypedExpr) : Doc = {
    import PrettyPrint.{paren, curly}
    import Doc.comma

    def prettyArgs(args : List[TypedExpr]) : Doc =
      paren(Doc.intercalate(Doc.text(",") + Doc.lineOrSpace, args.map(prettyPrintLint)).aligned)

    def prettyArgss(argss : List[List[TypedExpr]]) : Doc =
      Doc.intercalate(Doc.lineOrEmpty,
                      argss.map(args => paren(Doc.intercalate(Doc.text(","), args.map(prettyPrintLint)))))

    def isVarargMethod(f : scalac.Type) : Boolean = {
      def hasVarargParam(ps : List[scalac.Symbol]) : Boolean =
        ps.lastOption match {
          case None => false
          case Some(p) => p.tpe.toString.endsWith("*")
        }

      f.paramss.exists(hasVarargParam)
    }

    def prettyParamList(l : List[scalac.Symbol]) : Doc =
        paren(Doc.intercalate(comma, l.map(s => Doc.str(s.tpe))))

    def prettyType(t : scalac.Type) : Doc =
      if (t.paramss.isEmpty)
        Doc.str(t.underlying)
      else {
        val arr : Doc = Doc.space + Doc.text("=>") + Doc.space
        Doc.intercalate(arr, for (paramList <- t.paramss) yield prettyParamList(paramList)) + arr + prettyType(t.resultType)
      }

    def typeAnnot(e : Doc, ts : List[scalac.Type]) : Doc = ts match {
      case t :: ts =>
        if (!ts.isEmpty)
          println("Warning: found ambigious type for expression " + e)

        // Scala doesn't have vararg type for terms, so we don't try to write one.
        if (isVarargMethod(t)) {
          e
        } else {
          paren(e + Doc.space + Doc.text(":") + Doc.space + prettyType(t))
        }
      case _ =>
        println("Warning: found no type for expression " + e)
        e
    }

    e.cata(
        (_, lit, typ) => // literal
          typeAnnot(Doc.str(lit), typ)
      , (_, ident, symbol, typ) => { // identifier reference
          val i : Doc = Doc.text(ident)
          if (symbol.hasPackageFlag)
            i
          else
            typeAnnot(i, typ)
        }
      , (_, lhs, rhs, typ) => // assignment
          typeAnnot(prettyPrintLint(lhs) + Doc.space + Doc.str("=") + Doc.lineOrSpace + prettyPrintLint(rhs), typ)
      , (_, m, args, t) => // application
          typeAnnot(prettyPrintLint(m) + prettyArgs(args), t)
      , (_, lhs, op, args, t) => // infix application
          typeAnnot(prettyPrintLint(lhs) + Doc.space + Doc.str(op) + Doc.lineOrSpace +
            (args match {
              case List(arg) => prettyPrintLint(arg)
              case _ => prettyArgs(args)
            }), t)
      , (_, op, arg, t) => // unary application
          typeAnnot(Doc.str(op) + Doc.space + prettyPrintLint(arg), t)
      , (_, class_, argss, t) => // new
          typeAnnot(Doc.text("new") + Doc.space + Doc.str(class_) + prettyArgss(argss), t)
      , (_, obj, termName, symbol, t) => { // selection
          val sel : Doc = prettyPrintLint(obj) + Doc.text(".") + Doc.str(termName)
          if (symbol.hasPackageFlag)
            sel
          else
            typeAnnot(sel, t)
        }
      , (_, argss) => // this(...) application
          Doc.text("this") + prettyArgss(argss)
      , (_, typeName, t) => // this
          typeAnnot(Doc.str(typeName) + Doc.text("this"), t)
      , (_, _, _, t) => // super
          typeAnnot(Doc.text("super"), t)
      , (_, comps, t) => // tuple
          typeAnnot(prettyArgs(comps), t)
      , (_, pred, thenE, t) => // if-then
          typeAnnot(
              Doc.text("if") + Doc.space + paren(prettyPrintLint(pred)) + Doc.space +
              prettyPrintLint(thenE)
            , t
            )
      , (_, pred, thenE, elseE, t) => // if-then-else
          typeAnnot(
              Doc.text("if") + Doc.space + paren(prettyPrintLint(pred)) + Doc.space +
              prettyPrintLint(thenE) + Doc.space + Doc.str("else") + Doc.space + prettyPrintLint(elseE)
            , t
            )
      , (_, pred, body, t) => // while loop
          typeAnnot(
              Doc.text("while") + Doc.space + paren(prettyPrintLint(pred)) + Doc.space +
              prettyPrintLint(body)
            , t
            )
      , (_, enums, body, t) => // for loop
         typeAnnot(
             Doc.text("for") + paren(Doc.cat(enums.map(e => Doc.str(e) + Doc.text(";")))) + Doc.space + prettyPrintLint(body)
           , t
           )
      , (_, enums, body, t) => // for-yield loop
         typeAnnot(
             Doc.text("for") + paren(Doc.cat(enums.map(e => Doc.str(e) + Doc.text(";")))) + Doc.space + Doc.text("yield") + Doc.space + prettyPrintLint(body)
           , t
           )
      , (_, _) => // return
         Doc.text("return")
      , (_, expr, t) => // return with expr
         typeAnnot(Doc.text("return") + Doc.space + prettyPrintLint(expr), t)
      , (_, expr, t) => // throw
         typeAnnot(Doc.text("throw") + Doc.space + prettyPrintLint(expr), t)
      , (_, stmts, t) => // block
         typeAnnot(curly(Doc.stack(Doc.empty :: stmts.map(Statement.prettyPrintLint)).nested(2) + Doc.line), t)
      , (_, expr, t) => // other expression
         typeAnnot(paren(Doc.str(expr)), t)
      )
  }

  def toDot(n : TypedExpr) : DotGraph = {
    val (nodes, edges) = DotGen.exec(toDotGen(n))
    DotGraph("", nodes, edges)
  }

  def toDotGen(n : TypedExpr) : DotGen.DotGen[DotNode] = {
      n.cata(
          (l, lit, t) => { // literal
            val types : String = t.mkString(" or ")
            DotGen.node(DotNode.record(l, "Literal", s"$lit : $types"))
          }
        , (l, ident, _, t) => { // identifier reference
            val types : String = t.mkString(" or ")
            DotGen.node(DotNode.record(l, "Identifier", s"$ident : $types"))
          }
        , (l, lhs, rhs, t) => // assignment
            for (left <- toDotGen(lhs);
                 right <- toDotGen(rhs);
                 as <- DotGen.node(DotNode.record(l, "Assignment", t.toString()));
                 _ <- DotGen.edge(as, left, "left");
                 _ <- DotGen.edge(as, right, "right"))
            yield as
        , (l, m, args, t) => // application
            for (method <- toDotGen(m);
                 nodes <- mapM(toDotGen, args);
                 app <- DotGen.node(DotNode.record(l, "Application", " : " + t.mkString(" or ")));
                 _ <- DotGen.edge(app, method, "method");
                 _ <- DotGen.enum(app, nodes, "arg(%s)".format(_)))
            yield app
        , (l, lhs, op, args, t) => // infix application
            for (lhsNode <- toDotGen(lhs);
                 nodes <- mapM(toDotGen, args);
                 app <- DotGen.node(DotNode.record(l, "Infix application", op.toString + " : " + t.mkString(" or ")));
                 _ <- DotGen.edge(app, lhsNode, "left");
                 _ <- DotGen.enum(app, nodes, "arg(%s)".format(_)))
            yield app
        , (l, op, arg, _) => // unary application
            for (argNode <- toDotGen(arg);
                 app <- DotGen.node(DotNode.record(l, "Unary application", op.toString));
                 _ <- DotGen.edge(app, argNode, "arg"))
            yield app
        , (l, class_, argss, _) => // new
            for (newE <- DotGen.node(DotNode.record(l, "New", class_.toString));
                 nodess <- mapM(mapM(toDotGen, _ : List[TypedExpr]), argss);
                 _ <- DotGen.deepEnum(newE, nodess, "arg(%s, %s)".format(_, _)))
            yield newE
        , (l, obj, _, _, _) => // selection
            for (o <- toDotGen(obj);
                 select <- DotGen.node(DotNode.record(l, "Selection", n.toString));
                 _ <- DotGen.edge(select, o, ""))
            yield select
        , (l, _) => // this(...) application
           DotGen.node(DotNode.record(l, "This application", ""))
        , (l, typeName, _) => // this
            DotGen.node(DotNode.record(l, "This", typeName.toString))
        , (l, thisp, superp, _) => // super
            DotGen.node(DotNode.record(l, "Super", thisp.toString + " " + superp.toString))
        , (l, comps, t) => // tuple
            for (nodes <- mapM(toDotGen, comps);
                 tuple <- DotGen.node(DotNode.record(l, "Tuple", t.mkString(" or ")));
                 _ <- DotGen.enum(tuple, nodes,"comp(%s)".format(_)))
            yield tuple
        , (l, pred, thenE, _) => // if-then
            for (p <- toDotGen(pred);
                 then_ <- toDotGen(thenE);
                 ifE <- DotGen.node(DotNode.record(l, "If-then", ""));
                 _ <- DotGen.edge(ifE, p, "predicate");
                 _ <- DotGen.edge(ifE, then_, "then"))
            yield ifE
        , (l, pred, thenE, elseE, _) => // if-then-else
            for (p <- toDotGen(pred);
                 then_ <- toDotGen(thenE);
                 else_ <- toDotGen(elseE);
                 ifE <- DotGen.node(DotNode.record(l, "If-then-else", ""));
                 _ <- DotGen.edge(ifE, p, "predicate");
                 _ <- DotGen.edge(ifE, then_, "then");
                 _ <- DotGen.edge(ifE, else_, "else"))
            yield ifE
        , (l, pred, body, _) => // while loop
            for (p <- toDotGen(pred);
                 b <- toDotGen(body);
                 whileE <- DotGen.node(DotNode.record(l, "While loop", ""));
                 _ <- DotGen.edge(whileE, p, "predicate");
                 _ <- DotGen.edge(whileE, b, "body"))
            yield whileE
        , (l, _, body, _) => // for loop
            for (b <- toDotGen(body);
                 forE <- DotGen.node(DotNode.record(l, "For loop", ""));
                 _ <- DotGen.edge(forE, b, "body"))
            yield forE
        , (l, _, body, _) => // for-yield loop
            for (b <- toDotGen(body);
                 forE <- DotGen.node(DotNode.record(l, "For-yield loop", ""));
                 _ <- DotGen.edge(forE, b, "yield"))
            yield forE
        , (l, _) => // return
            DotGen.node(DotNode.record(l, "Return", ""))
        , (l, expr, _) => // return with expr
            for (e <- toDotGen(expr);
                 returnE <- DotGen.node(DotNode.record(l, "Return", ""));
                 _ <- DotGen.edge(returnE, e, "return"))
            yield returnE
        , (l, expr, _) => // throw
            for (e <- toDotGen(expr);
                 throwE <- DotGen.node(DotNode.record(l, "Throw", ""));
                 _ <- DotGen.edge(throwE, e, "throw"))
            yield throwE
        , (l, stmts, _) => // block
            for (nodes <- mapM[DotGen.DotGen, TypedStatement, DotNode](
                              (stmt : TypedStatement) =>
                                stmt.fold((decl : TypedDecl) => Decl.toDotGen(decl)
                                         ,(defn : TypedDefn) => Defn.toDotGen(defn)
                                         ,(e : TypedExpr) => toDotGen(e)
                                         )
                            , stmts
                            );
                 b <- DotGen.node(DotNode.record(l, "Block", ""));
                 _ <- DotGen.enum(b, nodes, "expr(%s)".format(_)))
             yield b
/*        , (l, args, body, _) => // lambda function
            mapM(toDotGen, args) >>= (nodes => {
              toDotGen(body) >>= (b => {
                val lambda = DotNode.record(l, "Lambda", "")
                enum(lambda, nodes, "arg(%s)".format(_)) >>
                add(lambda, List(edge(lambda, b, "body")))
              })
            })
*/
        , (l, expr, _) => // other expression
            DotGen.node(DotNode.record(l, "Expression", expr.toString()))
        )
  }
}

sealed abstract class TransformResult[A]

case class Skip[A]() extends TransformResult[A]
case class Changed[A](result : A) extends TransformResult[A]


case class Continue[A]() extends TransformResult[A]
case class TransformSkip[A](result : A) extends TransformResult[A]
case class TransformContinue[A](result : A) extends TransformResult[A]
