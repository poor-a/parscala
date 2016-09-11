package parscala

import scala.language.higherKinds
import scala.collection.immutable.Stream

import tree._

abstract class O
abstract class C

object CFGraph {
  import compiler.Quasiquote

  def apply(m : Method) : CFGraph = {
    def toList(t : Tree) : List[Tree] = 
      t match {
        case _ if t.isInstanceOf[compiler.Block] => {
          val q"{ ..$stmts }" = t
          stmts
        }
        case q"$stmt" => List(stmt)
      }

    def emptyBlockWithLabel(label : Label) : Block[Node, C, O] = 
      BFirst(NLabel(label))

    def emptyBlock(gen : Stream[Int]) : (Block[Node, C, O], Stream[Int]) = {
      val (n, gen2) = mkLabel(gen)
      (emptyBlockWithLabel(n), gen2)
    }
    
    def mkLabel(gen : Stream[Int]) : (Int, Stream[Int]) = {
      val n = gen.head
      (n, gen.tail)
    }

    def cfgStmts(graph : CFGraph, b : Block[Node,C,O], nextLabel : Label, gen : Stream[Int], stmts : List[Tree]) : (CFGraph, Stream[Int]) = {
      def g(stmt : Tree) : Node[O,O] = NStmt(stmt)

      stmts match {
        case q"if ($p) $t else $f" :: xs => {
          val (tBranch, gen2) = emptyBlock(gen)
          val (fBranch, gen3) = emptyBlock(gen2)
          val (succ, gen4) = emptyBlock(gen3)
          val (graph1, gen5) = cfgStmts(graph, tBranch, succ.entryLabel, gen4, toList(t))
          val (graph2, gen6) = cfgStmts(graph1, fBranch, succ.entryLabel, gen5, toList(f))
          val flushed = BCat(b, BLast(NCond(new Expression(p), tBranch.entryLabel, fBranch.entryLabel)))
          val graph3 = graph2 + flushed
          cfgStmts(graph3, succ, nextLabel, gen6, xs)
        }
        case q"while ($p) $loopBody" :: xs => {
          val (succ, gen2) = emptyBlock(gen)
          val (body, gen3) = emptyBlock(gen2)
          val (testPBegin, gen4) = emptyBlock(gen3)
          val testP = BCat(testPBegin, BLast(NCond(new Expression(p), body.entryLabel, succ.entryLabel)))
          val flushed = BCat(b, BLast(NJump(testP.entryLabel)))
          val (graph1, gen5) = cfgStmts(graph, body, testP.entryLabel, gen4, toList(loopBody))
          val graph2 = graph1 + testP + flushed
          cfgStmts(graph2, succ, nextLabel, gen5, xs)
        }
        case q"do $loopBody while ($p)" :: xs => {
          val (succ, gen2) = emptyBlock(gen)
          val (body, gen3) = emptyBlock(gen2)
          val (testPBegin, gen4) = emptyBlock(gen3)
          val testP = BCat(testPBegin, BLast(NCond(new Expression(p), body.entryLabel, succ.entryLabel)))
          val flushed = BCat(b, BLast(NJump(body.entryLabel)))
          val (graph1, gen5) = cfgStmts(graph, body, testP.entryLabel, gen4, toList(loopBody))
          val graph2 = graph1 + testP + flushed
          cfgStmts(graph2, succ, nextLabel, gen5, xs)
        }
        case q"return $_" :: _ => {
          val b1 = BCat(b, BLast(NJump(graph.done)))
          (graph + b1, gen)
        }
        case q"throw $_" :: _ => {
          val b1 = BCat(b, BLast(NJump(graph.done)))
          (graph + b1, gen)
        }
        case x :: xs => {
          val n : Node[O,O] = g(x)
          cfgStmts(graph, BCat(b, BMiddle(n)), nextLabel, gen, xs)
        }
        case List() => {
          val flushed = BCat(b, BLast(NJump(nextLabel)))
          (graph + flushed, gen)
        }
      }
    }

    val gen = Stream.from(0)
    val (first, gen2) = emptyBlock(gen)
    val (s, gen3) = mkLabel(gen2)
    val (d, gen4) = mkLabel(gen3)
    val start = BCat(emptyBlockWithLabel(s), BLast(NJump(first.entryLabel)))
    val done = BCat(emptyBlockWithLabel(d), BLast(NReturn()))
    val graph = new CFGraph(s, d) + start + done

    m.body match {
      case Some(ast) => 
        cfgStmts(graph, first, d, gen4, toList(ast))._1
      case None =>
        cfgStmts(graph, first, d, gen4, List.empty)._1
    }
  }
}

class CFGraph (val graph : Map[Label, Block[Node,C,C]], val start : Label, val done : Label) {
  def this(start : Label, done : Label) = this(Map(), start, done)

  def +(block : Block[Node,C,C]) : CFGraph = 
    new CFGraph(graph + (block.entryLabel -> block), start, done)

  def +(blocks : List[Block[Node,C,C]]) : CFGraph = 
    blocks.foldLeft(this)(_ + _)

  def get(v : Label) : Option[Block[Node,C,C]] = 
    graph.get(v)

  def traverse[A](f : (Block[Node,C,C], A) => A, x : A) : A = {
    def go(b : Block[Node,C,C], x : A, visited : Set[Block[Node,C,C]]) : (A, Set[Block[Node,C,C]]) = {
      if (visited(b))
        (x, visited)
      else {
        b.successors.foldLeft((f(b, x), visited + b)){(acc,succ) => 
          get(succ) match {
            case Some(block) => go(block, acc._1, acc._2)
            case None => acc
          }
        }
      }
    }

    get(start) map {go(_, x, Set.empty)._1} getOrElse x
  }

  def reverse() : ReverseCFGraph = {
    type Edge = (Label,Label)
    def f (b : Block[Node,C,C], xs : List[Edge]) : List[Edge] = {
      val l : Label = b.entryLabel
      (b.successors map {(_ : Label) -> l}) ++ xs
    }
    new ReverseCFGraph(this, traverse(f, List.empty))
  }
}

class ReverseCFGraph(val g : CFGraph, val edges : List[(Label,Label)]) {
  private val start : Label = g.done

  type DomTree = Map[Label, Option[Label]]

  def until[A](p : A => Boolean, f : A => A, x : A) : A =
    if (p(x)) x else until(p, f, f(x))

  def dominators() : Map[Label, Set[Label]] = {
    val labels : Set[Label] = g.graph.keySet
    val domInit : Map[Label, Set[Label]] = labels.toIterator.map{(_,labels)}.toMap.updated(start, Set(start))
    val noStart : Set[Label] = labels - start

    def approximate(acc : (Map[Label, Set[Label]], Boolean)) : (Map[Label, Set[Label]], Boolean) = {
      noStart.foldLeft((acc._1, false)){(ac, l) => 
        val (dom, changed) = ac
        val d : Set[Label] = pred(l).foldLeft(labels){(a, pred) => a & dom(pred)} + l
        if (d != dom(l))
          (dom.updated(l, d), true)
        else
          (dom, changed)
      }
    }
    def noChange[T](x : (T, Boolean)) : Boolean = !x._2

    until(noChange, approximate, (domInit, true))._1
  }

  def immediateDominators(domin : Map[Label, Set[Label]]) : DomTree = {
    val initIdom : Map[Label, Set[Label]] = (domin.toIterator map {x => (x._1, x._2 - x._1)}).toMap
    val noStart : Set[Label] = g.graph.keySet - start
    val idom : Map[Label, Set[Label]] = noStart.foldLeft(initIdom){(idom, l) =>
      idom(l).foldLeft(idom){(idomm, dom) =>
        val others : Set[Label] = idomm(l) - dom
        others.foldLeft(idomm){(idommm, otherDom) =>
          if (idommm(dom)(otherDom))
            idommm.updated(l, idommm(l) - otherDom)
          else
            idommm
        }
      }
    }
    
    idom mapValues (_.headOption)
  }

  def pred(l : Label) : List[Label] =
    for ((s, t) <- edges; if (l == t)) yield s

  def controlDependency() : ControlDependency = {
    type ControlEdge = (Label, Label)

    type Path = List[Label]

    def walkUp(from : Label, to : Label, t : DomTree) : Path = {
      def reachedOrTop(x : (Path, Label)) : Boolean =
        t(x._2).isEmpty || x._2 == to

      def step(x : (Path, Label)) : (Path, Label) = {
        val (p, l) = x
        (l :: p, t(l).get)
      }

      until(reachedOrTop, step, (List.empty, from))._1
    }

    def candidates(b : Block[Node,C,C], xs : List[ControlEdge]) : List[ControlEdge] = {
      val bl : Label = b.entryLabel
      b.successors match {
        case List(x,y) => (bl -> x) :: (bl -> y) :: xs
        case _ => xs
      }
    }
    
    val idom : DomTree = immediateDominators(dominators)
    val es : List[ControlEdge] = g.traverse(candidates, List.empty)
    val controlEdges : List[ControlEdge] = es flatMap {x => val (a,b) = x; walkUp(b, a, idom) map (a -> (_ : Label))}
    
    new ControlDependency(g, controlEdges)
  }
}

class ControlDependency(val g : CFGraph, val edges : List[(Label,Label)])

trait NonLocal[E,X] {
  def entryLabel(implicit evidence : E =:= C) : Label
  def successors(implicit evidence : X =:= C) : List[Label]
}

sealed abstract class Node[E,X] extends NonLocal[E,X]

case class NLabel(val label : Label) extends Node[C,O] {
  def entryLabel(implicit evidence : C =:= C) = label
  def successors(implicit evidence : O =:= C) : List[Label] = ???
}

case class NAssign(val variable : Variable, val expr : Expression) extends Node[O,O] {
  def entryLabel(implicit evidence : O =:= C) : Label = ???
  def successors(implicit evidence : O =:= C) : List[Label] = ???
}

case class NStmt(val stmt : Tree) extends Node[O,O] {
  def entryLabel(implicit evidence : O =:= C) : Label = ???
  def successors(implicit evidence : O =:= C) : List[Label] = ???
}

case class NCond(val expr : Expression, val t : Label, val f : Label) extends Node[O,C] {
  def entryLabel(implicit evidence : O =:= C) : Label = ???
  def successors(implicit evidence : C =:= C) : List[Label] = List(t,f)
}

case class NJump(val target : Label) extends Node[O,C] {
  def entryLabel(implicit evidence : O =:= C) : Label = ???
  def successors(implicit evidence : C =:= C) : List[Label] = List(target)
}

case class NReturn() extends Node[O,C] {
  def entryLabel(implicit evidence : O =:= C) : Label = ???
  def successors(implicit evidence : C =:= C) : List[Label] = List()
}

sealed abstract class Block[A[_,_],E,X] extends NonLocal[E,X]

case class BFirst[A[E,X] <: NonLocal[E,X]](val n : A[C,O]) extends Block[A,C,O] {
  def entryLabel(implicit evidence : C =:= C) : Label = n.entryLabel
  def successors(implicit evidence : O =:= C) : List[Label] = ???
}

case class BMiddle[A[_,_]](val n : A[O,O]) extends Block[A,O,O] {
  def entryLabel(implicit evidence : O =:= C) : Label = ???
  def successors(implicit evidence : O =:= C) : List[Label] = ???
}

case class BLast[A[E,X] <: NonLocal[E,X]](val n : A[O,C]) extends Block[A,O,C] {
  def entryLabel(implicit evidence : O =:= C) : Label = ???
  def successors(implicit evidence : C =:= C) : List[Label] = n.successors
}

case class BCat[A[_,_],E,X](val n1 : Block[A,E,O], val n2 : Block[A,O,X]) extends Block[A,E,X] {
  override def entryLabel(implicit evidence : E =:= C) : Label = n1.entryLabel
  override def successors(implicit evidence : X =:= C) : List[Label] = n2.successors
}
