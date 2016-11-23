package parscala

import scala.language.higherKinds
import scala.collection.immutable.Stream

import scalaz.State

import tree._

abstract class O
abstract class C

object CEdgeTag extends Enumeration {
  type TagType = Value
  val T, F, NoLabel = Value
}

class ExtensibleCFGraph(graph : CFGraph, val gen : BLabelGen){
  def mkLabel : (BLabel, ExtensibleCFGraph) = 
    (gen.head, new ExtensibleCFGraph(graph, gen.tail))

   def emptyBlock : (Block[Node, C, O], ExtensibleCFGraph) = {
     val (l, gen2) = mkLabel
     (BFirst(NLabel(l)), gen2)
   }

  val (start, done) : (BLabel, BLabel) = (graph.start, graph.done)

  def +(block : Block[Node,C,C]) : ExtensibleCFGraph = 
    new ExtensibleCFGraph(graph + block, gen)

  def +(blocks : List[Block[Node,C,C]]) : ExtensibleCFGraph = 
    blocks.foldLeft(this)(_ + _)

  def update(l : BLabel, f : Block[Node,C,C] => Block[Node,C,C]) =
    graph.get(l) map {b => new ExtensibleCFGraph(new CFGraph(graph.graph.updated(l, f(b)), graph.start, graph.done), gen)} getOrElse this

  def freeze : CFGraph = 
    graph

  type CEdge = (BLabel, BLabel, CEdgeTag.TagType)
  type CGraph = List[CEdge]
  type BLabelGen = Stream[BLabel]
  type SLabelGen = Stream[SLabel]
  type EdgeTag = CEdgeTag.TagType

  def insertEntry : ExtensibleCFGraph = {
    val (e, gen2) = (gen.head, gen.tail)
    val entry : Block[Node,C,C] = BCat(BFirst(NLabel(e)), BLast(NBranch(start, done)))
    val extended : CFGraph = new CFGraph((graph + entry).graph, entry.entryLabel, done)
    new ExtensibleCFGraph(extended, gen2)
  }

  def controlPrecedessors(l : BLabel, cEdges : CGraph) : List[(BLabel, EdgeTag)] =
    for ((s, t, tag) <- cEdges if (t == l)) yield (s, tag)

  def insertRegions(ls : Iterable[BLabel], cEdges : CGraph, regions : Map[Set[(BLabel, EdgeTag)], BLabel], gen : BLabelGen) : (CGraph, Map[Set[(BLabel, EdgeTag)], BLabel], BLabelGen) = 
    ls.foldLeft((cEdges, regions, gen)){ (acc, l) => insertRegion(l, acc._1, acc._2, acc._3) }

  def insertRegion(l : BLabel, cEdges : CGraph, regions : Map[Set[(BLabel, EdgeTag)], BLabel], gen : BLabelGen) : (CGraph, Map[Set[(BLabel, EdgeTag)], BLabel], BLabelGen) = {
    val prec : List[(BLabel, EdgeTag)] = controlPrecedessors(l, cEdges)
    val precS : Set[(BLabel, EdgeTag)] = prec.toSet
    if (!precS.isEmpty) {
      val otherEdges : CGraph = cEdges filter (_._2 != l)
      (regions get precS) match {
        case Some(region : BLabel) => 
          ((region, l, CEdgeTag.NoLabel) :: otherEdges, regions, gen)
        case None =>
          val (region, gen2) = (gen.head, gen.tail)
          val regionEdges : CGraph = prec map {case (s, tag) => (s, region, tag)} 
          ((region, l, CEdgeTag.NoLabel) :: regionEdges ++ otherEdges, regions + (precS -> region), gen2)
      }
    } else
      (cEdges, regions, gen)
  }

  type Edge = (BLabel, BLabel)
  type Region = BLabel

  def factor(cEdges : CGraph, regions : Map[Set[(BLabel, EdgeTag)], BLabel], cEdgesWithRegion : CGraph, domTree : DomTree) : CGraph = {
    def doFactor(l : BLabel, g : CGraph) : CGraph = {
      val prec : List[(BLabel, EdgeTag)] = controlPrecedessors(l, cEdges)
      val precS : Set[(BLabel, EdgeTag)] = prec.toSet
      domTree.children(l).foldLeft(g){(acc, child) => 
        val childPrec = controlPrecedessors(child, cEdges).toSet
        val inter : Set[(BLabel, EdgeTag)] = precS & childPrec
        if (inter == precS) {
          (regions get precS, regions get childPrec) match {
            case (Some(regionParent), Some(regionChild)) if (regionParent != regionChild) =>
              val otherEdges : CGraph = acc filterNot {case (s,t,tag) => t == regionChild && inter((s,tag))}
              (regionParent, regionChild, CEdgeTag.NoLabel) :: otherEdges
            case _ =>
                acc
          }
        } else if (inter == childPrec) {
          (regions get precS, regions get childPrec) match {
            case (Some(regionParent), Some(regionChild)) if (regionParent != regionChild) =>
              val otherEdges : CGraph = acc filterNot {case (s, t, tag) => t == regionParent && inter((s, tag))}
              (regionChild, regionParent, CEdgeTag.NoLabel) :: otherEdges
            case _ =>
              acc
          }
        } else
            acc
      }
    }

    domTree.postOrder(doFactor, cEdgesWithRegion)
  }

  def merge(cEdges : CGraph, gen : BLabelGen) : (CGraph, BLabelGen) = {
    val (pEdges, nonPredicateEdges) = cEdges partition {case (_,_,tag) => tag == CEdgeTag.T || tag == CEdgeTag.F}
    val edges : Map[(BLabel, EdgeTag), List[(BLabel, BLabel, EdgeTag)]] = pEdges groupBy { case (s, _, tag) => (s, tag) }
    val (merged : List[CEdge], gen2) = edges.foldLeft((List.empty[CEdge], gen)){ (acc, g) =>
      val (edges, gen) = acc
      g._2 match {
        case List(e) => (e :: edges, gen)
        case es => 
          val (region, gen2) = (gen.head, gen.tail)
          val (s, tag) = g._1
          val merged : List[CEdge] = es map { case (_, t, _) => (region, t, CEdgeTag.NoLabel) }
          ((s, region, tag) :: merged ++ edges, gen2)
      }
    }
    (merged ++ nonPredicateEdges, gen2)
  }

  def controlDependency : (ControlDependency, ExtensibleCFGraph) = {
    val (e, egraph) = emptyBlock
    val entry : Block[Node,C,C] = BCat(e, BLast(NBranch(start, done)))
    val extended : CFGraph = new CFGraph((graph + entry).graph, entry.entryLabel, done)
    val revCFG : ReverseCFGraph = extended.reverse
    val domTree : DomTree = revCFG.immediateDominators(revCFG.dominators)
    val controlEdges : CGraph = revCFG.controlDependency

    val labels : Set[BLabel] = extended.graph.keySet

    val (cEdges, regions, gen2) = insertRegions(labels, controlEdges, Map.empty, egraph.gen)
    val cEdges2 : CGraph = factor(controlEdges, regions, cEdges, domTree)
    val (cEdges3, gen3) = merge(cEdges2, gen2)

    (new ControlDependency(extended, e.entryLabel, cEdges3), new ExtensibleCFGraph(extended, gen3))
  }
}

object CFGraph {
  import compiler.Quasiquote

  def apply(m : Method) : CFGraph =
    mkExtCFGraph(m).freeze

  private type St = (Stream[BLabel], Stream[SLabel], CFGraph)

  private type CFGGen[A] = State[St, A]

  def genBLabel : CFGGen[BLabel] =
    for (l <- State.gets[St, BLabel](_._1.head);
         _ <- State.modify[St]{ case (bGen, sGen, graph) => (bGen, sGen.tail, graph) })
    yield l

  def genSLabel : CFGGen[SLabel] =
    for (l <- State.gets[St, SLabel](_._2.head);
         _ <- State.modify[St]{ case (bGen, sGen, graph) => (bGen, sGen.tail, graph) })
    yield l

  def modifyGraph(f : CFGraph => CFGraph) : CFGGen[Unit] = 
    State.modify[St]{ case (bGen, sGen, graph) => (bGen, sGen, f(graph)) }

  private def toList(t : Tree) : List[Tree] = 
    t match {
      case _ if t.isInstanceOf[compiler.Block] => {
        val q"{ ..$stmts }" = t
        stmts
      }
      case q"$stmt" => List(stmt)
    }

  def mkExtCFGraph(m : Method) : ExtensibleCFGraph = {
    val bGen : BLabelGen = BLabel.stream
    val sGen : SLabelGen = SLabel.stream
    val (List(fst,s,d), bGen2) = ((bGen take 3).toList, bGen drop 3)

    val start = BCat(emptyBlockWithLabel(s), BLast(NJump(fst)))
    val done = BCat(emptyBlockWithLabel(d), BLast(NDone()))
    val graph = new CFGraph(start, done)
    val first = emptyBlockWithLabel(fst)

    val stmts : List[Tree] = m.body match {
      case Some(ast) => toList(ast)
      case None => List.empty
    }

    val (bGen3, _, cfg) = cfgStmts(first, d, d, stmts, detailedStmtCfg) exec ((bGen2, sGen, graph))
    new ExtensibleCFGraph(cfg, bGen3)
  }

  def emptyBlockWithLabel(label : BLabel) : Block[Node, C, O] = 
    BFirst(NLabel(label))

  def emptyBlock : State[St, Block[Node, C, O]] = 
    for (l <- genBLabel) yield emptyBlockWithLabel(l)
    
  def cfgStmts(b : Block[Node,C,O], nextLabel : BLabel, abruptNext : BLabel, stmts : List[Tree], g : (Tree, Block[Node,C,O]) => CFGGen[Block[Node,C,O]]) : State[St, Unit] = {

    stmts match {
      case q"if ($p) $t else $f" :: xs =>
        for (tBranch <- emptyBlock;
             fBranch <- emptyBlock;
             succ <- emptyBlock;
             _ <- cfgStmts(tBranch, succ.entryLabel, abruptNext, toList(t), g);
             _ <- cfgStmts(fBranch, succ.entryLabel, abruptNext, toList(f), g);
             bWithTest <- g(p, b);
             flushed =  BCat(bWithTest, BLast(NCond(new Expression(p), tBranch.entryLabel, fBranch.entryLabel)));
             _ <- modifyGraph{ _ + flushed };
             _ <- cfgStmts(succ, nextLabel, abruptNext, xs, g))
        yield ()
      case q"while ($p) $loopBody" :: xs =>
        for (succ <- emptyBlock;
             body <- emptyBlock;
             testPBegin <- emptyBlock;
             withTest <- g(p, testPBegin);
             testP = BCat(withTest, BLast(NCond(new Expression(p), body.entryLabel, succ.entryLabel)));
             flushed = BCat(b, BLast(NJump(testP.entryLabel)));
             _ <- modifyGraph{ _ + flushed + testP };
             _ <- cfgStmts(body, testP.entryLabel, abruptNext, toList(loopBody), g);
             _ <- cfgStmts(succ, nextLabel, abruptNext, xs, g))
        yield ()
      case q"do $loopBody while ($p)" :: xs => 
        for (succ <- emptyBlock;
             body <- emptyBlock;
             testPBegin <- emptyBlock;
             withTest <- g(p, testPBegin);
             testP = BCat(withTest, BLast(NCond(new Expression(p), body.entryLabel, succ.entryLabel)));
             flushed = BCat(b, BLast(NJump(body.entryLabel)));
             _ <- modifyGraph{ _ + flushed + testP };
             _ <- cfgStmts(body, testP.entryLabel, abruptNext, toList(loopBody), g);
             _ <- cfgStmts(succ, nextLabel, abruptNext, xs, g))
        yield ()
      case q"try $expr" :: xs =>
        cfgStmts(b, nextLabel, abruptNext, toList(expr) ++ xs, g)
      case q"try $expr catch { case $_ }" :: xs => 
        println("try-catch".toUpperCase)
        println(toList(expr))
        cfgStmts(b, nextLabel, abruptNext, toList(expr) ++ xs, g)
      case q"try $expr finally $fin" :: xs => 
        cfgStmts(b, nextLabel, abruptNext, toList(expr) ++ toList(fin) ++ xs, g)
      case q"try $expr catch { case ..$_ } finally $fin" :: xs => 
        cfgStmts(b, nextLabel, abruptNext, toList(expr) ++ toList(fin) ++ xs, g)
      case q"$expr match { case ..$cases }" :: xs =>
        for (succ <- emptyBlock;
             withExpr <- g(expr, b);
             done <- State.gets[St, BLabel](_._3.done);
             _ <- cfgCases(cases, withExpr, succ.entryLabel, abruptNext, g);
             _ <- cfgStmts(succ, nextLabel, abruptNext, xs, g))
        yield ()
      case (ret @ q"return $e") :: _ =>
        for (eEvaled <- g(e, b);
             l <- genSLabel;
             _ <- modifyGraph{ graph => 
                    val flushed = BCat(eEvaled, BLast(NReturn(l, ret, graph.done)))
                    graph + flushed })
        yield ()
      case (thr @ q"throw $e") :: _ => { 
        for (eEvaled <- g(e, b);
             l <- genSLabel;
             flushed = BCat(eEvaled, BLast(NThrow(l, thr, abruptNext)));
             _ <- modifyGraph{ _ + flushed })
        yield ()
      }
      case x :: xs if x.isInstanceOf[compiler.Block] =>
        cfgStmts(b, nextLabel, abruptNext, toList(x) ++ xs, g)
      case x :: xs => {
        for (b2 <- g(x, b);
             _ <- cfgStmts(b2, nextLabel, abruptNext, xs, g))
        yield ()
      }
      case List() => {
        val flushed = BCat(b, BLast(NJump(nextLabel)))
        modifyGraph{ _ + flushed }
      }
    }
  }

  def cfgCases(cases : List[compiler.CaseDef], current : Block[Node,C,O], next : BLabel, abruptNext : BLabel, g : (Tree, Block[Node,C,O]) => CFGGen[Block[Node,C,O]] ) : CFGGen[Unit] = {
    cases match {
      case List(c) => 
        c match {
          case cq"$pat => $res" =>
            for (t <- emptyBlock;
                 f <- emptyBlock;
                 exceptionL <- genSLabel;
                 error = BCat(f, BLast(new NException(exceptionL, classOf[MatchError], abruptNext)));
                 flushed = BCat(current, BLast(NCond(new Expression(pat), t.entryLabel, f.entryLabel)));
                 _ <- modifyGraph{ _ + flushed + error };
                 _ <- cfgStmts(t, next, abruptNext, toList(res), g))
            yield ()
          case cq"$pat if $pred => $res" =>
            for (predEmpty <- emptyBlock;
                 t <- emptyBlock;
                 f <- emptyBlock;
                 exceptionL <- genSLabel;
                 error = BCat(f, BLast(new NException(exceptionL, classOf[MatchError], abruptNext)));
                 flushed = BCat(current, BLast(NCond(new Expression(pat), predEmpty.entryLabel, f.entryLabel)));
                 pred2 = BCat(predEmpty, BLast(NCond(new Expression(pred), t.entryLabel, f.entryLabel)));
                 _ <- modifyGraph{ _ + flushed + error + pred2 };
                 _ <- cfgStmts(t, next, abruptNext, toList(res), g))
            yield ()
        }
      case c :: cs => 
        c match {
          case cq"$pat => $res" =>
            for (t <- emptyBlock;
                 f <- emptyBlock;
                 flushed = BCat(current, BLast(NCond(new Expression(pat), t.entryLabel, f.entryLabel)));
                 _ <- modifyGraph{ _ + flushed };
                 _ <- cfgStmts(t, next, abruptNext, toList(res), g);
                 _ <- cfgCases(cs, f, next, abruptNext, g))
            yield ()
          case cq"$pat if $pred => $res" =>
            for (predEmpty <- emptyBlock;
                 t <- emptyBlock;
                 f <- emptyBlock;
                 flushed = BCat(current, BLast(NCond(new Expression(pat), predEmpty.entryLabel, f.entryLabel)));
                 pred2 = BCat(predEmpty, BLast(NCond(new Expression(pred), t.entryLabel, f.entryLabel)));
                 _ <- modifyGraph{ _ + flushed + pred2 };
                 _ <- cfgStmts(t, next, abruptNext, toList(res), g);
                 _ <- cfgCases(cs, f, next, abruptNext, g))
            yield ()
        }
      case List() =>
        State.state(())
    }
  }

  def simpleStmtCfg(stmt : Tree, current : Block[Node,C,O]) : CFGGen[Block[Node,C,O]] = {
    for (l <- genSLabel)
    yield BCat(current, BMiddle(NStmt(l, stmt)))
  }

  def foldM[A, B, S](f : (A, B) => State[S, A], e : A, l : List[B]) : State[S,A] = {
    l match {
      case List() => State.state(e)
      case x :: xs => 
        for (e2 <- f(e, x);
             e3 <- foldM(f, e2, xs))
        yield e3
    }
  }

  def detailedStmtCfg(stmt : Tree, current : Block[Node,C,O]) : CFGGen[Block[Node,C,O]] = {
    println(stmt)
    def app(b : Block[Node,C,O], f : SLabel => Node[O,O]) : CFGGen[Block[Node,C,O]] =
       for (l <- genSLabel)
       yield BCat(b, BMiddle(f(l)))
        
    tree.Control.exprCata(
      components => {
        println("  a tuple")
        for (b <- foldM((acc : Block[Node,C,O], e : Tree) => detailedStmtCfg(e, acc), current, components.reverse);
             l <- genSLabel)
        yield BCat(b, BMiddle(NExpr(l, stmt))) 
      },
      (earlydefns, parents, stats) => {
        val q"$_(...$args)" :: _ = parents
        for (b <- foldM((acc : Block[Node,C,O], e : Tree) => detailedStmtCfg(e, acc), current, args.flatten.reverse);
             b2 <- app(b, NExpr(_ : SLabel, stmt)))
        yield b2
      },
      (expr, tname) => {
        println("  a selection")
        for (b <- detailedStmtCfg(expr, current);
             b2 <- app(b, NExpr(_ : SLabel, stmt)))
        yield b2
      },
      (expr, args) => {
        println("  an application")
        println("  " + expr)
        println("  " + args)
        for (b <- detailedStmtCfg(expr, current); 
             b2 <- foldM((acc : Block[Node,C,O], e : Tree) => detailedStmtCfg(e, acc), b, args.flatten.reverse);
             b3 <- app(b2, NExpr(_ : SLabel, stmt)))
        yield b3
      },
      (lexpr, rexpr) => {
        for (b <- detailedStmtCfg(lexpr, current);
             b2 <- detailedStmtCfg(rexpr, b);
             b3 <- app(b2, NExpr(_ : SLabel, stmt)))
        yield b3
      },
      (_mods, name, expr) => {
        println("  an identifier definition")
        for (b <- detailedStmtCfg(expr, current);
             b2 <- app(b, NStmt(_ : SLabel, stmt)))
        yield b2
      },
      (other : Tree) => {
        println("  other")
        simpleStmtCfg(stmt, current)
      },
      stmt
    )
  }
}

class CFGraph (val graph : Map[BLabel, Block[Node,C,C]], val start : BLabel, val done : BLabel) {
  def this(start : Block[Node,C,C], done : Block[Node,C,C]) =
    this(Map(start.entryLabel -> start, done.entryLabel -> done), start.entryLabel, done.entryLabel)

  def get(v : BLabel) : Option[Block[Node,C,C]] = 
    graph.get(v)

  def +(b : Block[Node,C,C]) : CFGraph =
    new CFGraph(graph + (b.entryLabel -> b), start, done)

  def +(bs : List[Block[Node,C,C]]) : CFGraph =
    bs.foldLeft(this)(_ + _)

  def traverse[A](f : (Block[Node,C,C], A) => A, x : A) : A = {
    def go(b : Block[Node,C,C], x : A, visited : Set[Block[Node,C,C]]) : (A, Set[Block[Node,C,C]]) = {
      if (visited(b))
        (x, visited)
      else {
        b.successors.foldLeft((f(b, x), visited + b)){(acc,succ) => 
          val (l, _) = succ
          get(l) match {
            case Some(block) => go(block, acc._1, acc._2)
            case None => acc
          }
        }
      }
    }

    get(start) map {go(_, x, Set.empty)._1} getOrElse x
  }

  def reverse() : ReverseCFGraph = {
    type Edge = (BLabel,BLabel,CEdgeTag.TagType)
    def f (b : Block[Node,C,C], xs : List[Edge]) : List[Edge] = {
      val l : BLabel = b.entryLabel
      (b.successors map {case (succ, tag) => (succ, l, tag)}) ++ xs
    }
    new ReverseCFGraph(this, traverse(f, List.empty))
  }
}

class DomTree(val tree : Map[BLabel, Option[BLabel]]) {
  val root : Option[BLabel] =
    tree find {case (_, parent) => parent.isEmpty} map (_._1)

  def parent(l : BLabel) : Option[BLabel] = 
    tree find {case (child, parent) => child == l} map (_._2) getOrElse None

  def children(l : BLabel) : Iterable[BLabel] = {
    for ((child, Some(parent)) <- tree if (parent == l)) yield child
  }

  def postOrder[A](f : (BLabel, A) => A, x : A) : A = {
    def doTraverse(y : A, l : BLabel) : A =
      f(l, children(l).foldLeft(y)(doTraverse _))

    root map (doTraverse(x, _)) getOrElse x
  }

  override def equals(o : Any) : Boolean =
    o match {
      case t : DomTree => tree == t.tree
      case _ => false
    }

  override def toString : String = "DomTree(" + tree.toString + ")"
}

class ReverseCFGraph(val g : CFGraph, val edges : List[(BLabel,BLabel,CEdgeTag.TagType)]) {
  private val start : BLabel = g.done

  def until[A](p : A => Boolean, f : A => A, x : A) : A =
    if (p(x)) x else until(p, f, f(x))

  def dominators() : Map[BLabel, Set[BLabel]] = {
    val labels : Set[BLabel] = g.graph.keySet
    val domInit : Map[BLabel, Set[BLabel]] = labels.toIterator.map{(_,labels)}.toMap.updated(start, Set(start))
    val noStart : Set[BLabel] = labels - start

    def approximate(acc : (Map[BLabel, Set[BLabel]], Boolean)) : (Map[BLabel, Set[BLabel]], Boolean) = {
      noStart.foldLeft((acc._1, false)){(ac, l) => 
        val (dom, changed) = ac
        val d : Set[BLabel] = pred(l).foldLeft(labels){(a, pred) => a & dom(pred)} + l
        if (d != dom(l))
          (dom.updated(l, d), true)
        else
          (dom, changed)
      }
    }
    def noChange[T](x : (T, Boolean)) : Boolean = !x._2

    until(noChange, approximate, (domInit, true))._1
  }

  def immediateDominators(domin : Map[BLabel, Set[BLabel]]) : DomTree = {
    val initIdom : Map[BLabel, Set[BLabel]] = (domin.toIterator map {x => (x._1, x._2 - x._1)}).toMap
    val noStart : Set[BLabel] = g.graph.keySet - start
    val idom : Map[BLabel, Set[BLabel]] = noStart.foldLeft(initIdom){(idom, l) =>
      idom(l).foldLeft(idom){(idomm, dom) =>
        val others : Set[BLabel] = idomm(l) - dom
        others.foldLeft(idomm){(idommm, otherDom) =>
          if (idommm(dom)(otherDom))
            idommm.updated(l, idommm(l) - otherDom)
          else
            idommm
        }
      }
    }
    
    new DomTree(idom mapValues (_.headOption))
  }

  def pred(l : BLabel) : List[BLabel] =
    for ((s, t, _) <- edges; if (l == t)) yield s

  type CEdge = (BLabel, BLabel, CEdgeTag.TagType)

  def controlDependency() : List[CEdge] = {
    type Path = List[BLabel]

    def walkUp(from : BLabel, to : BLabel, t : DomTree) : Path = {
      def reachedOrTop(x : (Path, BLabel)) : Boolean =
        t.parent(x._2).isEmpty || x._2 == to

      def step(x : (Path, BLabel)) : (Path, BLabel) = {
        val (p, l) = x
        (l :: p, t.parent(l).get)
      }

      until(reachedOrTop, step, (List.empty, from))._1
    }

    def candidates(b : Block[Node,C,C], xs : List[CEdge]) : List[CEdge] = {
      val bl : BLabel = b.entryLabel
      b.successors match {
        case List((x, xTag), (y, yTag)) => (bl, x, xTag) :: (bl, y, yTag) :: xs
        case _ => xs
      }
    }
    
    val idom : DomTree = immediateDominators(dominators)
    val es : List[CEdge] = g.traverse(candidates, List.empty)
    es flatMap { case (a,b,tag) => 
      val to : BLabel = idom.parent(a) getOrElse a
      walkUp(b, to, idom).map{l => (a, l, tag)}
    }
  }
}

class ControlDependency(val g : CFGraph, val entry : BLabel, val edges : List[(BLabel, BLabel, CEdgeTag.TagType)]) {
  def dependsOn(l : BLabel) : List[BLabel] =
    for ((s,t,_) <- edges if t == l) yield s

  def controls(l : BLabel) : List[BLabel] = 
    for ((s,t,_) <- edges if s == l) yield t

  override def equals(o : Any) : Boolean = 
    o match {
      case cd : ControlDependency => entry == cd.entry && edges.toSet == cd.edges.toSet
      case _ => false
    }

  override def toString : String =
    "ControlDependency(%s, %s, %s)".format(g, entry, edges)
}

trait NonLocal[E,X] {
  def entryLabel(implicit evidence : E =:= C) : BLabel
  def successors(implicit evidence : X =:= C) : List[(BLabel, CEdgeTag.TagType)]
}

sealed abstract class Node[E,X] extends NonLocal[E,X]

case class NLabel(val label : BLabel) extends Node[C,O] {
  def entryLabel(implicit evidence : C =:= C) = label
  def successors(implicit evidence : O =:= C) : List[(BLabel, CEdgeTag.TagType)] = ???
}

case class NAssign(val variable : Variable, val expr : Expression) extends Node[O,O] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : O =:= C) : List[(BLabel, CEdgeTag.TagType)] = ???
}

case class NExpr(val l : SLabel, val expr : Tree) extends Node[O,O] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : O =:= C) : List[(BLabel, CEdgeTag.TagType)] = ???
}

case class NStmt(val l : SLabel, val stmt : Tree) extends Node[O,O] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : O =:= C) : List[(BLabel, CEdgeTag.TagType)] = ???
}

case class NCond(val expr : Expression, val t : BLabel, val f : BLabel) extends Node[O,C] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : C =:= C) : List[(BLabel, CEdgeTag.TagType)] = List((t, CEdgeTag.T), (f, CEdgeTag.F))
}

case class NBranch(val s1 : BLabel, val s2 : BLabel) extends Node[O,C] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : C =:= C) : List[(BLabel, CEdgeTag.TagType)] = List((s1, CEdgeTag.T), (s2, CEdgeTag.F))
}

case class NJump(val target : BLabel) extends Node[O,C] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : C =:= C) : List[(BLabel, CEdgeTag.TagType)] = List((target, CEdgeTag.NoLabel))
}

case class NReturn(val l : SLabel, val e : Tree, val next : BLabel) extends Node[O,C] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : C =:= C) : List[(BLabel, CEdgeTag.TagType)] = List((next, CEdgeTag.NoLabel))
}

case class NThrow(val l : SLabel, val e : Tree, val next : BLabel) extends Node[O,C] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : C =:= C) : List[(BLabel, CEdgeTag.TagType)] = List((next, CEdgeTag.NoLabel))
}

case class NException[T <: Throwable](val l : SLabel, val e : Predef.Class[T], val next : BLabel) extends Node[O,C] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : C =:= C) : List[(BLabel, CEdgeTag.TagType)] = List((next, CEdgeTag.NoLabel))
}

case class NDone() extends Node[O,C] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : C =:= C) : List[(BLabel, CEdgeTag.TagType)] = List()
}

sealed abstract class Block[A[_,_],E,X] extends NonLocal[E,X]

case class BFirst[A[E,X] <: NonLocal[E,X]](val n : A[C,O]) extends Block[A,C,O] {
  def entryLabel(implicit evidence : C =:= C) : BLabel = n.entryLabel
  def successors(implicit evidence : O =:= C) : List[(BLabel, CEdgeTag.TagType)] = ???
}

case class BMiddle[A[_,_]](val n : A[O,O]) extends Block[A,O,O] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : O =:= C) : List[(BLabel, CEdgeTag.TagType)] = ???
}

case class BLast[A[E,X] <: NonLocal[E,X]](val n : A[O,C]) extends Block[A,O,C] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : C =:= C) : List[(BLabel, CEdgeTag.TagType)] = n.successors
}

case class BCat[A[_,_],E,X](val n1 : Block[A,E,O], val n2 : Block[A,O,X]) extends Block[A,E,X] {
  override def entryLabel(implicit evidence : E =:= C) : BLabel = n1.entryLabel
  override def successors(implicit evidence : X =:= C) : List[(BLabel, CEdgeTag.TagType)] = n2.successors
}
