package parscala
package controlflow

sealed abstract class Node[E,X] extends NonLocal[E,X] {
  def sLabel : SLabel

  def sLabels : List[SLabel] = List(sLabel)
}

case class NLabel(val l : SLabel, val label : BLabel) extends Node[C,O] {
  def entryLabel(implicit evidence : C =:= C) = label
  def successors(implicit evidence : O =:= C) : List[(BLabel, EdgeLabel.TagType)] = ???
  def sLabel : SLabel = l
}

case class NLiteral(val l : SLabel, val lit : Tree) extends Node[O,O] {
  def entryLabel(implicit evidence : O =:= C) = ???
  def successors(implicit evidence : O =:= C) : List[(BLabel, EdgeLabel.TagType)] = ???
  def sLabel : SLabel = l
}

case class NVariable(val l : SLabel, val variable : Tree) extends Node[O,O] {
  def entryLabel(implicit evidence : O =:= C) = ???
  def successors(implicit evidence : O =:= C) : List[(BLabel, EdgeLabel.TagType)] = ???
  def sLabel : SLabel = l
}

case class NValDef(val l : SLabel, val rhs : Node[O,O], val valdef : Tree) extends Node[O,O] {
  def entryLabel(implicit evidence : O =:= C) = ???
  def successors(implicit evidence : O =:= C) : List[(BLabel, EdgeLabel.TagType)] = ???
  def sLabel : SLabel = l
}

case class NAssign(val l : SLabel, val lhs : Node[O,O], val rhs : Node[O,O], val tr : Tree) extends Node[O,O] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : O =:= C) : List[(BLabel, EdgeLabel.TagType)] = ???
  def sLabel : SLabel = l
}

case class NApp(val l : SLabel, val method : Node[O,O], val args : List[List[Node[O,O]]], val tr : Tree) extends Node[O,O] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : O =:= C) : List[(BLabel, EdgeLabel.TagType)] = ???
  def sLabel : SLabel = l
}

case class NNew(val l : SLabel, val constructor : Tree, val args : List[List[Node[O,O]]], val tr : Tree) extends Node[O,O] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : O =:= C) : List[(BLabel, EdgeLabel.TagType)] = ???
  def sLabel : SLabel = l
}

case class NSelect(val l : SLabel, val expr : Node[O,O], val sel : TermName, val tr : Tree) extends Node[O,O] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : O =:= C) : List[(BLabel, EdgeLabel.TagType)] = ???
  def sLabel : SLabel = l
}

case class NThis(val l : SLabel, val obj : Node[O,O], val expr : Node[O,O], val tr : Tree) extends Node[O,O] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : O =:= C) : List[(BLabel, EdgeLabel.TagType)] = ???
  def sLabel : SLabel = l
}

case class NTuple(val l : SLabel, val components : List[Node[O,O]], val tr : Tree) extends Node[O,O] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : O =:= C) : List[(BLabel, EdgeLabel.TagType)] = ???
  def sLabel : SLabel = l
}

case class NPattern(val l : SLabel, val pat : Tree, val succ : BLabel, val fail : BLabel) extends Node[O,C] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : C =:= C) : List[(BLabel, EdgeLabel.TagType)] = List((succ, EdgeLabel.T),(fail, EdgeLabel.F))
  def sLabel : SLabel = l
}

case class NExpr(val l : SLabel, val expr : Tree) extends Node[O,O] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : O =:= C) : List[(BLabel, EdgeLabel.TagType)] = ???
  def sLabel : SLabel = l
}

case class NCond(val l : SLabel, val expr : Node[O,O], val t : BLabel, val f : BLabel, val tr : Tree) extends Node[O,C] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : C =:= C) : List[(BLabel, EdgeLabel.TagType)] = List((t, EdgeLabel.T), (f, EdgeLabel.F))
  def sLabel : SLabel = l
}

case class NBranch(val l : SLabel, val s1 : BLabel, val s2 : BLabel) extends Node[O,C] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : C =:= C) : List[(BLabel, EdgeLabel.TagType)] = List((s1, EdgeLabel.T), (s2, EdgeLabel.F))

  def sLabel : SLabel = l
}

case class NJump(val l : SLabel, val target : BLabel) extends Node[O,C] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : C =:= C) : List[(BLabel, EdgeLabel.TagType)] = List((target, EdgeLabel.NoLabel))
  def sLabel : SLabel = l
}

case class NReturn(val l : SLabel, val e : Node[O,O], val next : BLabel, val tr : Tree) extends Node[O,C] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : C =:= C) : List[(BLabel, EdgeLabel.TagType)] = List((next, EdgeLabel.NoLabel))
  def sLabel : SLabel = l
}

case class NThrow(val l : SLabel, val e : Node[O,O], val next : BLabel, val tr : Tree) extends Node[O,C] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : C =:= C) : List[(BLabel, EdgeLabel.TagType)] = List((next, EdgeLabel.NoLabel))
  def sLabel : SLabel = l
}

case class NException[T <: Throwable](val l : SLabel, val e : Predef.Class[T], val next : BLabel) extends Node[O,C] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : C =:= C) : List[(BLabel, EdgeLabel.TagType)] = List((next, EdgeLabel.NoLabel))
  def sLabel : SLabel = l
}

case class NDone(val l : SLabel) extends Node[O,C] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : C =:= C) : List[(BLabel, EdgeLabel.TagType)] = List()
  def sLabel : SLabel = l
}
