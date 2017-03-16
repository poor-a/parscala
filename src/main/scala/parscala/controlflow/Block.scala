package parscala
package controlflow

import scala.language.higherKinds

sealed abstract class Block[A[_,_],E,X] extends NonLocal[E,X] {
  def fold[B](first : (B, A[C,O]) => B,
              middle : (B, A[O,O]) => B,
              last : (B, A[O,C]) => B,
              b : B) : B

  def foldRight[B](first : (A[C,O], B) => B,
                   middle : (A[O,O], B) => B,
                   last : (A[O,C], B) => B,
                   b : B) : B
}

case class BFirst[A[E,X] <: NonLocal[E,X]](val n : A[C,O]) extends Block[A,C,O] {
  def entryLabel(implicit evidence : C =:= C) : BLabel = n.entryLabel
  def successors(implicit evidence : O =:= C) : List[(BLabel, EdgeLabel.TagType)] = ???

  def fold[B](first : (B, A[C,O]) => B,
              middle : (B, A[O,O]) => B,
              last : (B, A[O,C]) => B,
              b : B) : B =
    first(b, n)

  def foldRight[B](first : (A[C,O], B) => B,
                   middle : (A[O,O], B) => B,
                   last : (A[O,C], B) => B,
                   b : B) : B =
    first(n, b)
}

case class BMiddle[A[E,X] <: NonLocal[E,X]](val n : A[O,O]) extends Block[A,O,O] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : O =:= C) : List[(BLabel, EdgeLabel.TagType)] = ???

  def fold[B](first : (B, A[C,O]) => B,
              middle : (B, A[O,O]) => B,
              last : (B, A[O,C]) => B,
              b : B) : B =
    middle(b, n)

  def foldRight[B](first : (A[C,O], B) => B,
                   middle : (A[O,O], B) => B,
                   last : (A[O,C], B) => B,
                   b : B) : B =
    middle(n, b)
}

case class BLast[A[E,X] <: NonLocal[E,X]](val n : A[O,C]) extends Block[A,O,C] {
  def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  def successors(implicit evidence : C =:= C) : List[(BLabel, EdgeLabel.TagType)] = n.successors

  def fold[B](first : (B, A[C,O]) => B,
              middle : (B, A[O,O]) => B,
              last : (B, A[O,C]) => B,
              b : B) : B =
    last(b, n)

  def foldRight[B](first : (A[C,O], B) => B,
                   middle : (A[O,O], B) => B,
                   last : (A[O,C], B) => B,
                   b : B) : B =
    last(n, b)
}

case class BCat[A[_,_],E,X](val b1 : Block[A,E,O], val b2 : Block[A,O,X]) extends Block[A,E,X] {
  def entryLabel(implicit evidence : E =:= C) : BLabel = b1.entryLabel
  def successors(implicit evidence : X =:= C) : List[(BLabel, EdgeLabel.TagType)] = b2.successors

  def fold[B](first : (B, A[C,O]) => B,
              middle : (B, A[O,O]) => B,
              last : (B, A[O,C]) => B,
              b : B) : B = {
    val b1b = b1.fold(first, middle, last, b)
    b2.fold(first, middle, last, b1b)
  }

  def foldRight[B](first : (A[C,O], B) => B,
                   middle : (A[O,O], B) => B,
                   last : (A[O,C], B) => B,
                   b : B) : B = {
    val b2b = b2.foldRight(first, middle, last, b)
    b1.foldRight(first, middle, last, b2b)
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
    b.foldRight[List[SLabel]](
       (_, ls) => ls
     , (n, ls) => Control.nodeOOCata(l => l :: ls, n)
     , (_, ls) => ls
     , List.empty[SLabel]
     )
  }
}
