package parscala
package controlflow

sealed abstract class Node[E,X] extends NonLocal[E,X]

case class Label(val label : BLabel) extends Node[C,O] {
  def entryLabel(implicit evidence : C =:= C) = label
  def successors(implicit evidence : O =:= C) : List[(BLabel, EdgeLabel.TagType)] = ???
}

case class Pattern(val pat : SLabel, val succ : BLabel, val fail : BLabel) extends Node[O,C] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : C =:= C) : List[(BLabel, EdgeLabel.TagType)] = List((succ, EdgeLabel.T),(fail, EdgeLabel.F))
}

case class Expr(val expr : SLabel) extends Node[O,O] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : O =:= C) : List[(BLabel, EdgeLabel.TagType)] = ???
}

case class Cond(val expr : SLabel, val t : BLabel, val f : BLabel) extends Node[O,C] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : C =:= C) : List[(BLabel, EdgeLabel.TagType)] = List((t, EdgeLabel.T), (f, EdgeLabel.F))
}

case class Branch(val s1 : BLabel, val s2 : BLabel) extends Node[O,C] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : C =:= C) : List[(BLabel, EdgeLabel.TagType)] = List((s1, EdgeLabel.T), (s2, EdgeLabel.F))
}

case class Jump(val target : BLabel) extends Node[O,C] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : C =:= C) : List[(BLabel, EdgeLabel.TagType)] = List((target, EdgeLabel.NoLabel))
}

case class Done() extends Node[O,C] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : C =:= C) : List[(BLabel, EdgeLabel.TagType)] = List()
}
