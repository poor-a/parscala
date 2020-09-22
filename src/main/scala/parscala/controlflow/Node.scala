package parscala
package controlflow

sealed abstract class Node[E,X] extends NonLocal[E,X]

case class Label(label : BLabel) extends Node[C,O] {
  override def entryLabel(implicit evidence : C =:= C) : BLabel = label
  override def successors(implicit evidence : O =:= C) : List[(BLabel, EdgeLabel.TagType)] = ???
}

case class Pattern(pat : PLabel, succ : BLabel, fail : BLabel) extends Node[O,C] {
  override def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  override def successors(implicit evidence : C =:= C) : List[(BLabel, EdgeLabel.TagType)] = List((succ, EdgeLabel.T),(fail, EdgeLabel.F))
}

case class Expr(expr : SLabel) extends Node[O,O] {
  override def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  override def successors(implicit evidence : O =:= C) : List[(BLabel, EdgeLabel.TagType)] = ???
}

case class Defn(defn : DLabel) extends Node[O,O] {
  override def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  override def successors(implicit evidence : O =:= C) : List[(BLabel, EdgeLabel.TagType)] = ???
}

/**
 * Represents a call of the method `m`.
 *
 * @param expr The method application expression in the program.
 * @param ms Methods may be invoked.
 */
case class Call(expr : SLabel, ms : List[BLabel], returnPoint : BLabel) extends Node[O,C] {
  override def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  override def successors(implicit evidence : C =:= C) : List[(BLabel, EdgeLabel.TagType)] = ms.map((_, EdgeLabel.NoLabel))
}

/**
 * Represents a return point. The execution continues from here after
 * the execution of the called method.
 */
case class Return(l : BLabel, from : List[BLabel], call : Call) extends Node[C,O] {
  override def entryLabel(implicit evidence : C =:= C) : BLabel = l
  override def successors(implicit evidence : O =:= C) : List[(BLabel, EdgeLabel.TagType)] = ???
}

/**
 * Branching based on a known condition `expr`. When `expr` evaluates
 * to `true`, we take the `t` branch, otherwise the `f` branch.
 */
case class Cond(expr : SLabel, t : BLabel, f : BLabel) extends Node[O,C] {
  override def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  override def successors(implicit evidence : C =:= C) : List[(BLabel, EdgeLabel.TagType)] = List((t, EdgeLabel.T), (f, EdgeLabel.F))
}

case class Branch(s1 : BLabel, s2 : BLabel) extends Node[O,C] {
  override def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  override def successors(implicit evidence : C =:= C) : List[(BLabel, EdgeLabel.TagType)] = List((s1, EdgeLabel.T), (s2, EdgeLabel.F))
}

case class Jump(target : BLabel) extends Node[O,C] {
  override def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  override def successors(implicit evidence : C =:= C) : List[(BLabel, EdgeLabel.TagType)] = List((target, EdgeLabel.NoLabel))
}

/**
 * Represents the end of execution of a method.
 */
case class Done(succ : List[BLabel]) extends Node[O,C] {
  override def entryLabel(implicit evidence : O =:= C) : BLabel = ???
  override def successors(implicit evidence : C =:= C) : List[(BLabel, EdgeLabel.TagType)] = succ.zip(List.fill(succ.length)(EdgeLabel.NoLabel))

  def addSucc(s : BLabel) : Done =
    Done(s :: succ)
}

object Node {
  def cata[A](label_ : (BLabel) => A,
              pattern_ : (PLabel, BLabel, BLabel) => A,
              expr_ : (SLabel) => A,
              call_ : (SLabel, List[BLabel], BLabel) => A,
              return_ : (BLabel, List[BLabel], Call) => A,
              cond_ : (SLabel, BLabel, BLabel) => A,
              branch_ : (BLabel, BLabel) => A,
              jump_ : (BLabel) => A,
              done_ : (List[BLabel]) => A,
              n : Node[_,_]) : A =
    n match {
      case Label(bl) => label_(bl)
      case Pattern(sl, success, failure) => pattern_(sl, success, failure)
      case Expr(sl) => expr_(sl)
      case Call(expr, methods, returnPoint) => call_(expr, methods, returnPoint)
      case Return(l, methods, call) => return_(l, methods, call)
      case Cond(sl, t, f) => cond_(sl, t, f)
      case Branch(succ1, succ2) => branch_(succ1, succ2)
      case Jump(target) => jump_(target)
      case Done(succs) => done_(succs)
    }

  def OOCata[A](nExpr : (SLabel) => A,
                n : Node[O,O]) : A = 
    n match {
      case Expr(sl) => nExpr(sl)
    }

  def OCCata[A](pattern_ : (PLabel, BLabel, BLabel) => A,
                call_ : (SLabel, List[BLabel], BLabel) => A,
                cond_ : (SLabel, BLabel, BLabel) => A,
                branch_ : (BLabel, BLabel) => A,
                jump_ : (BLabel) => A,
                done_ : (List[BLabel]) => A,
                n : Node[O,C]) : A = 
    n match {
      case Pattern(sl, success, failure) => pattern_(sl, success, failure)
      case Call(expr, methods, returnPoint) => call_(expr, methods, returnPoint)
      case Cond(sl, t, f) => cond_(sl, t, f)
      case Branch(succ1, succ2) => branch_(succ1, succ2)
      case Jump(target) => jump_(target)
      case Done(succs) => done_(succs)
    }
}
