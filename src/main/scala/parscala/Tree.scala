package parscala

import scala.language.higherKinds

import compiler.TermSymbol

class Class(val ast : Tree) {
  import compiler.Quasiquote

  private val details : (Name,List[Tree],List[Tree],List[Tree]) = ast match {
    case q"$_ class $name extends ..$earlydefs { ..$defs }" => (name,earlydefs,List(),defs)
    case q"$_ class $name extends $earlydefs with ..$parents { ..$defs }" => (name,List(earlydefs),parents,defs)
    case q"$_ object $name extends ..$earlydefs { ..$defs }" => (name,earlydefs,List(),defs)
    case q"$_ object $name extends $earlydefs with ..$parents { ..$defs }" => (name,List(earlydefs),parents,defs)
    case q"package object $name extends ..$earlydefs { ..$defs }" => (name,earlydefs,List(),defs)
    case q"package object $name extends $earlydefs with ..$parents { ..$defs }" => (name,List(earlydefs),parents,defs)
    case _ => throw new RuntimeException("not a class")
  }

  val name : String = details._1.toString()

  override def toString : String = s"class $name"

  lazy val methods : List[Method] = {
    def isMethod (definition : Tree) : Boolean = definition match {
      case q"$_ def $_ (...$_) : $_ = $_" => true
      case _ => false
    }

    details._4 filter isMethod map {new Method(_, this)}
  }
}

class Package(val ast : Tree) {
  import compiler.Quasiquote

  val name : String = ast match {
    case q"package $p { ..$_ }" => p.toString
    case _ => throw new RuntimeException("not a package")
  }

  lazy val classes : List[Tree] = {
    def classesOf(packageDef : Tree) : List[Tree] = {
      val q"package $_ { ..$topstats }" = packageDef
      topstats.foldLeft(List[Tree]()){(cs, ast) => ast match {
        case q"package $_ { ..$_ }" => classesOf(ast) ++ cs
        case q"$_ class $_ extends $_" => ast :: cs
        case q"$_ class $_ extends $_ { ..$_ }" => ast :: cs
        case q"$_ class $_ extends $_ with $_ { ..$_ }" => ast :: cs
        case q"$_ object $_ extends $_" => ast :: cs
        case q"$_ object $_ extends $_ { ..$_ }" => ast :: cs
        case q"$_ object $_ extends $_ with $_ { ..$_ }" => ast :: cs
        case q"package object $_ extends $_" => ast :: cs
        case q"package object $_ extends $_ { ..$_ }" => ast :: cs
        case q"package object $_ extends $_ with $_ { ..$_ }" => ast :: cs
        case _                    => cs
      }}
    }
    
    classesOf(ast)
  }
}

class Method(val ast : Tree, val parent : Class) {
  import compiler.Quasiquote

  assert(ast != null, "ast of a method may not be null")
  assert(parent != null, "parent of a method may not be null")

  val (name, args, body) : (String, List[List[Tree]], Tree) = ast match {
    case q"$_ def $name (...$args) : $_ = $body" => 
      (name.toString(), args, body)
    case _ => 
      throw new RuntimeException("not a method")
  }

  lazy val vars : List[Variable] = {
    def traverse[A](acc : A, t : Tree)(f : (A,Tree) => A) : A = {
      t match {
        case q"{ ..$stmts}" => 
          stmts.foldLeft(acc)(f)
        case q"if ($p) $a else $b" => {
          val acc1 = traverse(acc, p)(f)
          val acc2 = traverse(acc1, a)(f)
          traverse(acc2, b)(f)
        }
        case q"while ($p) $body" => {
          val acc1 = traverse(acc, p)(f)
          traverse(acc1, body)(f)
        }
      }
    }

    traverse(List[Variable](), body){(acc : List[Variable], t : Tree) => {
        if (Variable.isVariableDef(t))
          new Variable(t) :: acc
        else
          acc
      }
    }
  }

  lazy val cfg : CFGraph = {
    def toList(t : Tree) : List[Tree] = t match {
      case q"{ ..$stmts }" => stmts
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
        case x :: xs =>
          val n : Node[O,O] = g(x)
          cfgStmts(graph, BCat(b, BMiddle(n)), nextLabel, xs)
        case List() => 
          val flushed = BCat(b, BLast(NJump(nextLabel)))
          graph + (flushed.entryLabel -> flushed)
      }
    }

    val first = Block.empty[Node]
    val start = BCat(Block.empty(Start), BLast(NJump(first.entryLabel)))
    val graph = new CFGraph() + start
    
    cfgStmts(graph, first, Done, toList(body))
  }
  

  override def toString : String = s"method ${parent.name}.$name"
}

object Variable {
  def isVariableDef(ast : Tree) : Boolean = {
    val isTerm : Boolean = ast.symbol != null && ast.symbol.isTerm
    if (isTerm) {
      val symbol : TermSymbol = ast.symbol.asTerm
      symbol.isVar || symbol.isVal
    }
    else {
      false
    }
  }
}

class Variable (val definition : Tree) {
  assert(Variable.isVariableDef(definition), "not a variable")
  val info : TermSymbol = definition.symbol.asTerm
  val name : String = info.name.toString

  override def toString : String = s"variable $name"
}

class Expression(val ast : Tree) {
  override def toString : String = s"expression $ast"
}

abstract class O
abstract class C

class Label

object Done extends Label
object Start extends Label

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

abstract class Node[E,X] extends NonLocal[E,X]

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

abstract class Block[A[_,_],E,X] extends NonLocal[E,X] {
  
}

object Block {
  def empty[A[_,_]] : Block[Node,C,O] = BFirst(NLabel(new Label))
  def empty[A[_,_]](label : Label) : Block[Node,C,O] = 
    BFirst(NLabel(label))
}

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

/*
case class Block(stmts : List[Tree]) extends CFGraph {
  override val successors : List[CFGraph] = List(cont)
}

case class If(thenBranch : CFGraph, elseBranch : CFGraph) extends CFGraph
case class While(var body : CFGraph, cont : CFGraph) extends CFGraph {
  override def toString() : String = s"While(<body>, $cont)"
}
case class Entry() extends CFGraph {
  private[this] var Succs : Set[CFGraph] = HashSet()
}

case class Exit() extends CFGraph

*/
