package parscala
package tree

import scalaz.{State, Monad, MonadState, IndexedStateT}
import scalaz.syntax.bind.ToBindOpsUnapply // >>= and >>

import parscala.Control.{foldM, foldM_, mapM}
import dot.{Dot, DotAttr, DotGraph, DotNode, DotEdge}

class NodeTree (val root : Node, val nodes : ExprMap[Node])

sealed abstract class Node {
  def label : SLabel
  def tree : Tree
}

case class Literal(val l : SLabel, lit : Lit, val t : Tree) extends Node {
  def label : SLabel = l
  def tree : Tree = t
}

case class Ident(val l : SLabel, val s : Symbol, val variable : Tree) extends Node {
  def label : SLabel = l
  def tree : Tree = variable
}

case class PatDef(val l : SLabel, val lhs : Pat, val rhs : Node, val t : Tree) extends Node {
  def label : SLabel = l
  def tree : Tree = t
}

case class Assign(val l : SLabel, val lhs : Node, val rhs : Node, val tr : Tree) extends Node {
  def label : SLabel = l
  def tree : Tree = tr
}

case class App(val l : SLabel, val method : Node, val args : List[List[Node]], val funRef : DLabel, val tr : Tree) extends Node {
  def label : SLabel = l
  def tree : Tree = tr
}

case class New(val l : SLabel, val constructor : Tree, val args : List[List[Node]], val tr : Tree) extends Node {
  def label : SLabel = l
  def tree : Tree = tr
}

case class Select(val l : SLabel, val expr : Node, val sel : TermName, val tr : Tree) extends Node {
  def label : SLabel = l
  def tree : Tree = tr
}

case class This(val l : SLabel, val obj : TypeName, val tr : Tree) extends Node {
  def label : SLabel = l
  def tree : Tree = tr
}

case class Tuple(val l : SLabel, val components : List[Node], val tr : Tree) extends Node {
  def label : SLabel = l
  def tree : Tree = tr
}

case class If(val l : SLabel, val pred : Node, val thenE : Node, val tr : Tree) extends Node {
  def label : SLabel = l
  def tree : Tree = tr
}

case class IfElse(val l : SLabel, val pred : Node, val thenE : Node, val elseE : Node, val tr : Tree) extends Node {
  def label : SLabel = l
  def tree : Tree = tr
}

case class While(val l : SLabel, val pred : Node, val body : Node, val tr : Tree) extends Node {
  def label : SLabel = l
  def tree : Tree = tr
}

case class For(val l : SLabel, val enums : List[Node], val body : Node, val tr : Tree) extends Node {
  def label : SLabel = l
  def tree : Tree = tr
}

case class ForYield(val l : SLabel, val enums : List[Node], val body : Node, val tr : Tree) extends Node {
  def label : SLabel = l
  def tree : Tree = tr
}

case class ReturnUnit(val l : SLabel, val tr : Tree) extends Node {
  def label : SLabel = l
  def tree : Tree = tr
}

case class Return(val l : SLabel, val e : Node, val tr : Tree) extends Node {
  def label : SLabel = l
  def tree : Tree = tr
}

case class Throw(val l : SLabel, val e : Node, val tr : Tree) extends Node {
  def label : SLabel = l
  def tree : Tree = tr
}

case class Block(val l : SLabel, val exprs : List[Node], val b : Tree) extends Node {
  def label : SLabel = l
  def tree : Tree = b
}

case class Lambda(val l : SLabel, val args : List[Node], val body : Node, val tr : Tree) extends Node {
  def label : SLabel = l
  def tree : Tree = tr
}

case class Expr(val l : SLabel, val expr : Tree) extends Node {
  def label : SLabel = l
  def tree : Tree = expr
}

object Node {
  def nodeCata[A](nLiteral : (SLabel, Lit, Tree) => A,
                  nIdent : (SLabel, Symbol, Tree) => A,
                  nPatDef : (SLabel, Pat, Node, Tree) => A,
                  nAssign : (SLabel, Node, Node, Tree) => A,
                  nApp : (SLabel, Node, List[List[Node]], DLabel, Tree) => A,
                  nNew : (SLabel, Tree, List[List[Node]], Tree) => A,
                  nSelect : (SLabel, Node, TermName, Tree) => A,
                  nThis : (SLabel, TypeName, Tree) => A,
                  nTuple : (SLabel, List[Node], Tree) => A,
                  nIf : (SLabel, Node, Node, Tree) => A,
                  nIfElse : (SLabel, Node, Node, Node, Tree) => A,
                  nWhile : (SLabel, Node, Node, Tree) => A,
                  nFor : (SLabel, List[Node], Node, Tree) => A,
                  nForYield : (SLabel, List[Node], Node, Tree) => A,
                  nReturnUnit : (SLabel, Tree) => A,
                  nReturn : (SLabel, Node, Tree) => A,
                  nThrow : (SLabel, Node, Tree) => A,
                  nBlock : (SLabel, List[Node], Tree) => A,
                  nLambda : (SLabel, List[Node], Node, Tree) => A,
                  nExpr : (SLabel, Tree) => A,
                  n : Node) : A =
    n match {
      case Literal(sl, lit, t) => nLiteral(sl, lit, t)
      case Ident(sl, sym, t) => nIdent(sl, sym, t)
      case PatDef(sl, pat, rhs, t) => nPatDef(sl, pat, rhs, t)
      case Assign(sl, lhs, rhs, t) => nAssign(sl, lhs, rhs, t)
      case App(sl, f, args, funRef, t) => nApp(sl, f, args, funRef, t)
      case New(sl, ctr, args, t) => nNew(sl, ctr, args, t)
      case Select(sl, qual, name, t) => nSelect(sl, qual, name, t)
      case This(sl, qual, t) => nThis(sl, qual, t)
      case Tuple(sl, comps, t) => nTuple(sl, comps, t)
      case If(sl, pred, thenE, tr) => nIf(sl, pred, thenE, tr)
      case IfElse(sl, pred, thenE, elseE, tr) => nIfElse(sl, pred, thenE, elseE, tr)
      case While(sl, pred, body, tr) => nWhile(sl, pred, body, tr)
      case For(sl, enums, body, tr) => nFor(sl, enums, body, tr)
      case ForYield(sl, enums, body, tr) => nForYield(sl, enums, body, tr)
      case ReturnUnit(sl, t) => nReturnUnit(sl, t)
      case Return(sl, expr, t) => nReturn(sl, expr, t)
      case Throw(sl, expr, t) => nThrow(sl, expr, t)
      case Block(sl, exprs, t) => nBlock(sl, exprs, t)
      case Lambda(sl, args, body, tr) => nLambda(sl, args, body, tr)
      case Expr(sl, expr) => nExpr(sl, expr)
    }

  case class St
    ( pGen : PLabelGen
    , sGen : SLabelGen 
    , dGen : DLabelGen
    , exprs : ExprMap[Node]
    , symbols : Map[Symbol, DLabel]
    , decls : DeclMap[Decl]
    , packages : List[Package]
    )
  type NodeGen[A] = State[St, A]
  private val stateInstance : MonadState[NodeGen, St] = IndexedStateT.stateMonad[St]
  val nodeGenMonadInstance : Monad[NodeGen] = stateInstance

  private def modifySt[A](f : St => (A, St)) : NodeGen[A] =
    for (s <- stateInstance.get;
         (x, sNew) = f(s);
         _ <- stateInstance.put(sNew))
    yield x

  private def getDLabel (s : Symbol) : NodeGen[Option[DLabel]] =
    State.gets[St, Option[DLabel]](_.symbols.get(s))

  private def genSLabel : NodeGen[SLabel] =
    modifySt{ s => (s.sGen.head, s.copy(sGen = s.sGen.tail)) }

  private def genPLabel : NodeGen[PLabel] =
    modifySt{ s => (s.pGen.head, s.copy(pGen = s.pGen.tail)) }

  private def genDLabel (sym : Symbol) : NodeGen[DLabel] =
    modifySt{ s => (s.dGen.head, s.copy(dGen = s.dGen.tail, symbols = s.symbols + ((sym, s.dGen.head)))) }

  private def label(f : SLabel => Node) : NodeGen[Node] = 
    for (l <- genSLabel;
         n = f(l);
         _ <- modifySt{ s => ((), s.copy(exprs = s.exprs.updated(l, n))) })
    yield n

  private def labelPat(f : PLabel => Pat) : NodeGen[Pat] = 
    for (l <- genPLabel;
         p = f(l))
    yield p

  private def nLiteral(lit : Lit, t : Tree) : NodeGen[Node] = 
    label(Literal(_, lit, t))

  private def nIdent(symbol : Symbol, ident : Tree) : NodeGen[Node] =
    label(Ident(_, symbol, ident))

  private def nPatDef(lhs : Pat, rhs : Node, tr : Tree) : NodeGen[Node] =
    label(PatDef(_, lhs, rhs, tr))

  private def nAssign(lhs : Node, rhs : Node, tr : Tree) : NodeGen[Node] =
    label(Assign(_, lhs, rhs, tr))

  private def nNew(constructor : Tree, args : List[List[Node]], tr : Tree) : NodeGen[Node] = 
    label(New(_, constructor, args, tr))

  private def nApp(fun : Node, args : List[List[Node]], funRef : DLabel, tr : Tree) : NodeGen[Node] = 
    label(App(_, fun, args, funRef, tr))

  private def nIf(pred : Node, thenE : Node, tr : Tree) : NodeGen[Node] = 
    label(If(_, pred, thenE, tr))

  private def nIfElse(pred : Node, thenE : Node, elseE : Node, tr : Tree) : NodeGen[Node] = 
    label(IfElse(_, pred, thenE, elseE, tr))

  private def nWhile(pred : Node, body : Node, tr : Tree) : NodeGen[Node] = 
    label(While(_, pred, body, tr))

  private def nFor(enums : List[Node], body : Node, tr : Tree) : NodeGen[Node] = 
    label(For(_, enums, body, tr))

  private def nForYield(enums : List[Node], body : Node, tr : Tree) : NodeGen[Node] = 
    label(ForYield(_, enums, body, tr))

  private def nSelect(expr : Node, sel : TermName, tr : Tree) : NodeGen[Node] = 
    label(Select(_, expr, sel, tr))

  private def nThis(qualifier : TypeName, tr : Tree) : NodeGen[Node] = 
    label(This(_, qualifier, tr))

  private def nTuple(comps : List[Node], tuple : Tree) : NodeGen[Node] = 
    label(Tuple(_, comps, tuple))

  private def nReturnUnit(tr : Tree) : NodeGen[Node] =
    label(ReturnUnit(_, tr))

  private def nReturn(expr : Node, tr : Tree) : NodeGen[Node] =
    label(Return(_, expr, tr))

  private def nBlock(exprs : List[Node], tr : Tree) : NodeGen[Node] =
    label(Block(_, exprs, tr))

  private def nExpr(tr : Tree) : NodeGen[Node] =
    label(Expr(_, tr))

  private def addUnknownMethod(s : Symbol) : NodeGen[DLabel] =
    for (l <- genDLabel(s);
         m = Method(l, s, s.fullName, List(), None);
         _ <- modifySt { st => ((), st.copy(decls = st.decls + (l -> m))) })
    yield l
  
  private def collectMethod(t : Tree) : NodeGen[Unit] =
    if (t.symbol != null && t.symbol.isMethod)
      nodeGenMonadInstance.void(addUnknownMethod(t.symbol))
    else
      nodeGenMonadInstance.pure(())

  def genNode(t : Tree) : NodeGen[Node] = {
    import scalaz.syntax.bind._
    import compiler.Quasiquote

    def step(ns : List[Node], tr : Tree) : NodeGen[List[Node]] =
      for (n <- genNode(tr))
      yield n :: ns

    def deepStep(nns : List[List[Node]], tr : List[Tree]) : NodeGen[List[List[Node]]] = 
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
          genNode(expr) >>= (e => 
          nSelect(e, termName, t))
      , (method, argss) => // application
          genNode(method) >>= (m =>
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
          genNode(pred) >>= (p => 
          genNode(thenE) >>= (th =>
          nIf(p, th, t)))
      , (pred, thenE, elseE) => // if-then-else
          genNode(pred) >>= (p => 
          genNode(thenE) >>= (th =>
          genNode(elseE) >>= (e =>
          nIfElse(p, th, e, t))))
      , (pred, body) => // while loop
          genNode(pred) >>= (p => 
          genNode(body) >>= (b =>
          nWhile(p, b, t)))
      , (enums, body) => // for loop
          foldM(step, List.empty, enums) >>= (nodes =>
          genNode(body) >>= (b =>
          nFor(nodes, b, t)))
      , (enums, body) => // for-yield loop
          foldM(step, List.empty, enums) >>= (nodes =>
          genNode(body) >>= (b =>
          nForYield(nodes, b, t)))
      , (lhs, rhs) => // assignment
          genNode(lhs) >>= (lNode =>
          genNode(rhs) >>= (rNode =>
          nAssign(lNode, rNode, t)))
      , (_, lhs, rhs) => // var or val def
          genNode(rhs) >>= (rNode =>
          labelPat(IdentPat(_, lhs)) >>= (pat =>
          nPatDef(pat, rNode, t)))
      , () => // return
          nReturnUnit(t)
      , expr => // return with expr
          genNode(expr) >>= (node =>
          nReturn(node, t))
      , stmts => // expression block
          foldM(step, List.empty, stmts) >>= (nodes => nBlock(nodes.reverse, t))
      , other => // other expression
          nExpr(other)
      , t)
  }

  private def withDLabel(genLabel : NodeGen[DLabel])(f : DLabel => NodeGen[Decl]) : NodeGen[Decl] =
    for (l <- genLabel;
         decl <- f(l);
         _ <- modifySt { st => ((), st.copy(decls = st.decls + (l -> decl))) })
    yield decl

  def genDecl(t : Tree) : NodeGen[Decl] =
    Control.declCata(
        (name, sym, mRhs) => // variable
          withDLabel(genDLabel(sym))(l => {
          val m : NodeGen[Option[Node]] = scalaz.std.option.cata(mRhs)(
              rhs => stateInstance.map(genNode(rhs))(Some(_))
            , stateInstance.pure(None)
            )
          m >>= (mExpr =>
          stateInstance.pure(Var(l, sym, name, mExpr))
          )})
      , (name, sym, mRhs) => // value
          withDLabel(genDLabel(sym))(l => {
          val m : NodeGen[Option[Node]] = scalaz.std.option.cata(mRhs)(
              rhs => stateInstance.map(genNode(rhs))(Some(_))
            , stateInstance.pure(None)
            )
          m >>= (mAst => 
          stateInstance.pure(Val(l, sym, name, mAst))
          )})
      , (name, sym, argss, mBody) => // method
          withDLabel(genDLabel(sym))(l =>
          mapM(mapM(genPat, _ : List[Tree]), argss) >>= (pats => {
          val m : NodeGen[Option[Node]] = scalaz.std.option.cata(mBody)(
              body => stateInstance.map(genNode(body))(Some(_))
            , stateInstance.pure(None)
            )
          m >>= (mAst => 
          stateInstance.pure(Method(l, sym, name, pats, mAst))
          )}))
      , (name, sym, decls) => // class
          withDLabel(genDLabel(sym))(l =>
          mapM(genDecl, decls) >>= (ds =>
          stateInstance.pure(Class(l, sym, name, ds))
          ))
      , (name, sym, decls) => // object
          withDLabel(genDLabel(sym))(l =>
          mapM(genDecl, decls) >>= (ds =>
          stateInstance.pure(Object(l,sym, name, ds))
          ))
      , (name, sym, decls) => // package object
          withDLabel(genDLabel(sym))(l =>
          mapM(genDecl, decls) >>= (ds =>
          stateInstance.pure(PackageObject(l,sym, name, ds))
          ))
      , (name, sym, decls) => // package
          withDLabel(genDLabel(sym))(l =>
          mapM(genDecl, decls) >>= (ds => {
          val p : Package = Package(l,sym, name, ds)
          modifySt{ s => ((), s.copy(packages = p :: s.packages)) } >>= (_ =>
          stateInstance.pure(p)
          )}))
      , t
      )

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

  def fromTree(t : Tree) : (ProgramGraph, Option[NodeTree]) = {
    val (st, nodes) : (St, Option[NodeTree]) = 
      if (t.isTerm) {
        val (st, node) = run(genNode(t))
        (st, Some(new NodeTree(node, st.exprs)))
      } else {
        val (st, _) = run(genDecl(t))
        (st, None)
      }
    (new ProgramGraph(st.decls, st.exprs, st.symbols, st.packages), nodes)
    }

  def run[A](m : NodeGen[A]) : (St, A) = {
    val startSt : St = St(PLabel.stream, SLabel.stream, DLabel.stream, Map(), Map(), Map(), List())
    m.run(startSt)
  }

  def toDot(n : Node) : DotGraph = {
    type St = (List[DotNode], List[DotEdge])
    type DotGen[A] = State[St, A]

    def add(node : DotNode, edges : List[DotEdge]) : DotGen[DotNode] = 
      for (_ <- State.modify[St]{ case (ns, es) => (node :: ns, edges ++ es) })
      yield node

    def addEdge(edge : DotEdge) : DotGen[Unit] =
      State.modify[St]{ case (ns, es) => (ns, edge :: es) }

    def record(l : SLabel, header : String, body : String) : DotNode =
      DotNode(l.toString) !! DotAttr.shape("record") !! DotAttr.labelWithPorts("{ %s | %s }".format(header, Dot.dotEscape(body)))

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

    def formatNode(n : Node) : DotGen[DotNode] = 
      nodeCata(
          (l, lit, t) => // literal
            add(record(l, "Literal", t.toString), List())
        , (l, _, t) => // identifier reference
            add(record(l, "Identifier", t.toString), List())
        , (l, pat, rhs, t) => // pattern definition
            formatNode(rhs) >>= (right => {
            val patDef = record(l, "Pattern definition", pat.toString)
            add(patDef, List(edge(patDef, right, "value")))
            })
        , (l, lhs, rhs, t) => // assignment
            formatNode(lhs) >>= (left => 
            formatNode(rhs) >>= (right => {
              val as = record(l, "Assignment", t.toString())
              val lEdge = edge(as, left, "left")
              val rEdge = edge(as, right, "right")
              add(as, List(lEdge, rEdge))
            }))
        , (l, m, argss, _, t) => // application
            formatNode(m) >>= ( method => 
            mapM(mapM(formatNode, _ : List[Node]), argss) >>= (nodess => {
              val app = record(l, "Application", t.toString())
              val edgeToMethod = edge(app, method, "method")
              deepEnum(app, nodess, "arg(%s, %s)".format(_, _)) >>
              add(app, List(edgeToMethod))
            }))
        , (l, cls, argss, t) => // new
            mapM(mapM(formatNode, _ : List[Node]), argss) >>= (nodess => {
              val newE = record(l, "New", t.toString())
              //val clsNode = ???
              deepEnum(newE, nodess, "arg(%s, %s)".format(_, _)) >>
              add(newE, List())
            })
        , (l, obj, termName, t) => // selection
            formatNode(obj) >>= (o => {
              //val termNode = ???
              val select = record(l, "Selection", t.toString())
              add(select, List(edge(select, o, "")))
            })
        , (l, typeName, t) => // this
            add(record(l, "This", t.toString()), List())
        , (l, comps, t) => // tuple
            mapM(formatNode, comps) >>= (nodes => {
              val tuple = record(l, "Tuple", t.toString())
              enum(tuple, nodes,"comp(%s)".format(_))
              add(tuple, List())
            })
        , (l, pred, thenE, t) => // if-then
            formatNode(pred) >>= (p =>
            formatNode(thenE) >>= (th => {
              val ifE = record(l, "If-then", "")
              add(ifE, List(edge(ifE, p, "predicate"),
                            edge(ifE, th, "then")))
            }))
        , (l, pred, thenE, elseE, t) => // if-then-else
            formatNode(pred) >>= (p =>
            formatNode(thenE) >>= (th =>
            formatNode(elseE) >>= (el => {
              val ifE = record(l, "If-then-else", "")
              add(ifE, List(edge(ifE, p, "predicate"),
                            edge(ifE, th, "then"),
                            edge(ifE, el, "else")))
            })))
        , (l, pred, body, t) => // while loop
            formatNode(pred) >>= (p =>
            formatNode(body) >>= (b => {
              val whileE = record(l, "While loop", "")
              add(whileE, List(edge(whileE, p, "predicate"), 
                               edge(whileE, b, "body")))
            }))
        , (l, enums, body, t) => // for loop
            mapM(formatNode, enums) >>= (nodes =>
            formatNode(body) >>= (b => {
              val forE = record(l, "For loop", "")
              enum(forE, nodes, "enum(%s)".format(_)) >>
              add(forE, List(edge(forE, b, "body")))
            }))
        , (l, enums, body, t) => // for-yield loop
            mapM(formatNode, enums) >>= (nodes =>
            formatNode(body) >>= (b => {
              val forE = record(l, "For-yield loop", "")
              enum(forE, nodes, "enum(%s)".format(_)) >>
              add(forE, List(edge(forE, b, "yield")))
            }))
        , (l, t) => { // return
            val returnE = record(l, "Return", "")
            add(returnE, List())
          }
        , (l, expr, t) => // return with expr
            formatNode(expr) >>= (e => {
              val returnE = record(l, "Return", "")
              add(returnE, List(edge(returnE, e, "return")))
            })
        , (l, expr, t) => // throw
            formatNode(expr) >>= (e => {
              val throwE = record(l, "Throw", "")
              add(throwE, List(edge(throwE, e, "throw")))
            })
        , (l, stmts, _) => // block
            mapM(formatNode, stmts) >>= (nodes => {
              val b = record(l, "Block", "")
              enum(b, nodes, "expr(%s)".format(_)) >>
              add(b, List())
            })
        , (l, args, body, _) => // lambda function
            mapM(formatNode, args) >>= (nodes => {
              formatNode(body) >>= (b => {
                val lambda = record(l, "Lambda", "")
                enum(lambda, nodes, "arg(%s)".format(_)) >>
                add(lambda, List(edge(lambda, b, "body")))
              })
            })
        , (l, expr) => { // other expression
            val e = record(l, "Expression", expr.toString())
            add(e, List())
          }
        , n)

    val (nodes, edges) = formatNode(n).exec((List(), List()))
    DotGraph("", nodes.reverse, edges)
  }
/*
  def resugar(root : Node, transformer : Node => NodeGen[Node]) : NodeGen[Node] = {
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
