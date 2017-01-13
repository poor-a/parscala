package parscala
package controlflow

import scala.language.higherKinds

sealed abstract class Block[A[_,_],E,X] extends NonLocal[E,X] {
  def apply(l : SLabel) : Option[UNode[A]]

  def fold[B](first : (A[C,O], B) => B,
              middle : (A[O,O], B) => B,
              last : (A[O,C], B) => B,
              b : B) : B
}

case class BFirst[A[E,X] <: NonLocal[E,X]](val n : A[C,O]) extends Block[A,C,O] {
  def entryLabel(implicit evidence : C =:= C) : BLabel = n.entryLabel
  def successors(implicit evidence : O =:= C) : List[(BLabel, EdgeLabel.TagType)] = ???

  def apply(l : SLabel) : Option[UNode[A]] = Some(UFirst(n))

  def fold[B](first : (A[C,O], B) => B,
              middle : (A[O,O], B) => B,
              last : (A[O,C], B) => B,
              b : B) : B =
    first(n, b)
}

case class BMiddle[A[E,X] <: NonLocal[E,X]](val n : A[O,O]) extends Block[A,O,O] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : O =:= C) : List[(BLabel, EdgeLabel.TagType)] = ???

  def apply(l : SLabel) : Option[UNode[A]] = Some(UMiddle(n))

  def fold[B](first : (A[C,O], B) => B,
              middle : (A[O,O], B) => B,
              last : (A[O,C], B) => B,
              b : B) : B = middle(n, b)
}

case class BLast[A[E,X] <: NonLocal[E,X]](val n : A[O,C]) extends Block[A,O,C] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : C =:= C) : List[(BLabel, EdgeLabel.TagType)] = n.successors

  def apply(l : SLabel) : Option[UNode[A]] = Some(ULast(n))

  def fold[B](first : (A[C,O], B) => B,
              middle : (A[O,O], B) => B,
              last : (A[O,C], B) => B,
              b : B) : B = last(n, b)
}

case class BCat[A[_,_],E,X](val b1 : Block[A,E,O], val b2 : Block[A,O,X]) extends Block[A,E,X] {
  def entryLabel(implicit evidence : E =:= C) : BLabel = b1.entryLabel
  def successors(implicit evidence : X =:= C) : List[(BLabel, EdgeLabel.TagType)] = b2.successors

  def apply(l : SLabel) : Option[UNode[A]] = b1(l) orElse b2(l)

  def fold[B](first : (A[C,O], B) => B,
              middle : (A[O,O], B) => B,
              last : (A[O,C], B) => B,
              b : B) : B = {
    val b1b = b1.fold(first, middle, last, b)
    b2.fold(first, middle, last, b1b)
  }
}

object Block {
  def lastNode[A[_,_]](b : Block[A,C,C]) : A[O,C] = {
    def f(b1 : Block[A,_,C]) : A[O,C] =
      b1 match {
        case BLast(n) => n
        case BCat(_, b2) => f(b2)
      }

    f(b)
  }

  def sLabels(b : Block[Node,_,_]) : List[SLabel] = {
    val sl = b.fold[List[SLabel]](
                      (_, ls) => ls
                    , (n, ls) => Control.nodeOOCata(l => l :: ls, n)
                    , (_, ls) => ls
                    , List.empty[SLabel]
                    )
    sl.reverse
  }
}
