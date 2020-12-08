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

case class BFirst[A[E,X]<:NonLocal[E,X]](n : A[C,O]) extends Block[A,C,O] {
  override def entryLabel(implicit evidence : C =:= C) : BLabel = n.entryLabel
  override def successors(implicit evidence : O =:= C) : List[(BLabel, EdgeLabel.TagType)] = ???

  override def fold[B](first : (B, A[C,O]) => B,
                       middle : (B, A[O,O]) => B,
                       last : (B, A[O,C]) => B,
                       b : B) : B =
    first(b, n)

  override def foldRight[B](first : (A[C,O], B) => B,
                            middle : (A[O,O], B) => B,
                            last : (A[O,C], B) => B,
                            b : B) : B =
    first(n, b)
}

case class BMiddle[A[E,X]](n : A[O,O]) extends Block[A,O,O] {
  override def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  override def successors(implicit evidence : O =:= C) : List[(BLabel, EdgeLabel.TagType)] = ???

  override def fold[B](first : (B, A[C,O]) => B,
                       middle : (B, A[O,O]) => B,
                       last : (B, A[O,C]) => B,
                       b : B) : B =
    middle(b, n)

  override def foldRight[B](first : (A[C,O], B) => B,
                            middle : (A[O,O], B) => B,
                            last : (A[O,C], B) => B,
                            b : B) : B =
    middle(n, b)
}

case class BLast[A[E,X] <: NonLocal[E,X]](n : A[O,C]) extends Block[A,O,C] {
  override def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  override def successors(implicit evidence : C =:= C) : List[(BLabel, EdgeLabel.TagType)] = n.successors

  override def fold[B](first : (B, A[C,O]) => B,
                       middle : (B, A[O,O]) => B,
                       last : (B, A[O,C]) => B,
                       b : B) : B =
    last(b, n)

  override def foldRight[B](first : (A[C,O], B) => B,
                            middle : (A[O,O], B) => B,
                            last : (A[O,C], B) => B,
                            b : B) : B =
    last(n, b)
}

case class BCat[A[_,_], E, X](b1 : Block[A,E,O], b2 : Block[A,O,X]) extends Block[A,E,X] {
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
  def firstNode[A[_,_]](b : Block[A,C,C]) : A[C,O] = {
    def f(b1 : Block[A,C,_]) : A[C,O] =
      b1 match {
        case BFirst(n) => n
        case BCat(b1, _) => f(b1)
      }

    f(b)
  }

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
     , (n, ls) => Node.OOCata(l => l :: ls, _ => ls, n)
     , (_, ls) => ls
     , List.empty[SLabel]
     )
  }

/*
  def toLocal[E, X](b : Block[Node, E, X, G]) : Block[Node, E, X, L] = {
    def toLocalNodes[E_, X_](n : Node[E_, X_]) : Block[Node, E_, X_,] =
      n match {
        case Label(l) => 
          BFirst(Label(l))
        case Pattern(pat, succ, fail) => 
          BLast(Pattern(pat, succ, fail)) // pattern
        case Expr(l) =>
          BMiddle(Expr(l))  // expression
        case Call(expr, _, returnPoint) =>                 // method call
          BCat(BMiddle(Expr(expr)), BLast(Jump(returnPoint)))
        case Return(l, _, _) =>                              // return
          BFirst(Label(l))
        case Cond(expr, t, f) => 
          BLast(Cond(expr, t, f))    // conditional branch
        case Branch(s1, s2) => 
          BLast(Branch(s1, s2))  // branch
        case Jump(target) => 
          BLast(Jump(target))    // jump
        case Done(succ) =>
          BLast(Done(succ))    // done
      }

    b match {
      case BFirst(n) => toLocalNodes(n)
      case BMiddle(n) => toLocalNodes(n)
      case BLast(n) => toLocalNodes(n)
      case BCat(b1, b2) => BCat(toLocal(b1), toLocal(b2))
    }
  }
  */
}

