package parscala

import scala.language.higherKinds
import tree._

abstract class O
abstract class C

class Label

object Done extends Label
object Start extends Label

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

    def cfgStmts(graph : CFGraph, b : Block[Node,C,O], nextLabel : Label, stmts : List[Tree]) : CFGraph = {
      def g(stmt : Tree) : Node[O,O] = NStmt(stmt)

      stmts match {
        case q"if ($p) $t else $f" :: xs => {
          val tBranch, fBranch : Block[Node, C, O] = Block.empty
          val next : Block[Node, C, O] = Block.empty
          val graph1 = cfgStmts(graph, tBranch, next.entryLabel, toList(t))
          val graph2 = cfgStmts(graph1, fBranch, next.entryLabel, toList(f))
          val flushed = BCat(b, BLast(NCond(new Expression(p), tBranch.entryLabel, fBranch.entryLabel)))
          val graph3 = graph2 + (flushed.entryLabel -> flushed)
          cfgStmts(graph3, next, nextLabel, xs)
        }
        case q"while ($p) $loopBody" :: xs => {
          val next : Block[Node, C, O] = Block.empty
          val body : Block[Node, C, O] = Block.empty
          val testP = BCat(Block.empty[Node], BLast(NCond(new Expression(p), body.entryLabel, next.entryLabel)))
          val flushed = BCat(b, BLast(NJump(testP.entryLabel)))
          val graph1 = cfgStmts(graph, body, testP.entryLabel, toList(loopBody))
          val graph2 = graph1 + ((testP.entryLabel -> testP),(flushed.entryLabel -> flushed))
          cfgStmts(graph2, next, nextLabel, xs)
        }
        case q"do $loopBody while ($p)" :: xs => {
          val next, body : Block[Node, C, O] = Block.empty
          val testP = BCat(Block.empty[Node], BLast(NCond(new Expression(p), body.entryLabel, next.entryLabel)))
          val flushed = BCat(b, BLast(NJump(body.entryLabel)))
          val graph1 = cfgStmts(graph, body, testP.entryLabel, toList(loopBody))
          val graph2 = graph1 + ((testP.entryLabel -> testP),(flushed.entryLabel -> flushed))
          cfgStmts(graph2, next, nextLabel, xs)
        }
        case q"return $_" :: _ => {
          val b1 = BCat(b, BLast(NJump(Done)))
          graph + (b.entryLabel -> b1)
        }
        case q"throw $_" :: _ => {
          val b1 = BCat(b, BLast(NJump(Done)))
          graph + (b.entryLabel -> b1)
        }
        case x :: xs => {
          val n : Node[O,O] = g(x)
          cfgStmts(graph, BCat(b, BMiddle(n)), nextLabel, xs)
        }
        case List() => {
          val flushed = BCat(b, BLast(NJump(nextLabel)))
          graph + (flushed.entryLabel -> flushed)
        }
      }
    }

    val first = Block.empty[Node]
    val start = BCat(Block.empty(Start), BLast(NJump(first.entryLabel)))
    val graph = new CFGraph() + start

    m.body match {
      case Some(ast) => 
        cfgStmts(graph, first, Done, toList(ast))
      case None =>
        cfgStmts(graph, first, Done, List.empty)
    }
  }
}

class CFGraph private (private[this] val graph : Map[Label, Block[Node,C,C]]) {
  def this() = this(Map())

  def +(block : Block[Node,C,C]) : CFGraph = new CFGraph(graph + (block.entryLabel -> block))
  def +(kv : (Label, Block[Node,C,C])) : CFGraph = new CFGraph(graph + kv)
  def +(kvs : (Label, Block[Node,C,C])*) : CFGraph = new CFGraph(graph ++ kvs)
  def apply(v : Label) : Block[Node,C,C] = graph(v)
}

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

object Block {
  def empty[A[_,_]] : Block[Node,C,O] = BFirst(NLabel(new Label))
  def empty[A[_,_]](label : Label) : Block[Node,C,O] = 
    BFirst(NLabel(label))
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
