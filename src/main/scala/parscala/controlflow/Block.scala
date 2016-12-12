package parscala
package controlflow

import scala.language.higherKinds

sealed abstract class Block[A[_,_],E,X] extends NonLocal[E,X]

case class BFirst[A[E,X] <: NonLocal[E,X]](val n : A[C,O]) extends Block[A,C,O] {
  def entryLabel(implicit evidence : C =:= C) : BLabel = n.entryLabel
  def successors(implicit evidence : O =:= C) : List[(BLabel, EdgeLabel.TagType)] = ???

  def sLabels : List[SLabel] = n.sLabels
}

case class BMiddle[A[E,X] <: NonLocal[E,X]](val n : A[O,O]) extends Block[A,O,O] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : O =:= C) : List[(BLabel, EdgeLabel.TagType)] = ???

  def sLabels : List[SLabel] = n.sLabels
}

case class BLast[A[E,X] <: NonLocal[E,X]](val n : A[O,C]) extends Block[A,O,C] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : C =:= C) : List[(BLabel, EdgeLabel.TagType)] = n.successors

  def sLabels : List[SLabel] = n.sLabels
}

case class BCat[A[_,_],E,X](val b1 : Block[A,E,O], val b2 : Block[A,O,X]) extends Block[A,E,X] {
  override def entryLabel(implicit evidence : E =:= C) : BLabel = b1.entryLabel
  override def successors(implicit evidence : X =:= C) : List[(BLabel, EdgeLabel.TagType)] = b2.successors

  def sLabels : List[SLabel] = b1.sLabels ++ b2.sLabels
}
