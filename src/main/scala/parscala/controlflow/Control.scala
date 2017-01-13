package parscala
package controlflow

object Control {
  def nodeCata[A](nLabel : (BLabel) => A,
                  nPattern : (SLabel, BLabel, BLabel) => A,
                  nExpr : (SLabel) => A,
                  nCond : (SLabel, BLabel, BLabel) => A,
                  nBranch : (BLabel, BLabel) => A,
                  nJump : (BLabel) => A,
                  nDone : () => A,
                  n : Node[_,_]) : A =
    n match {
      case Label(bl) => nLabel(bl)
      case Pattern(sl, success, failure) => nPattern(sl, success, failure)
      case Expr(sl) => nExpr(sl)
      case Cond(sl, t, f) => nCond(sl, t, f)
      case Branch(succ1, succ2) => nBranch(succ1, succ2)
      case Jump(target) => nJump(target)
      case Done() => nDone()
    }

  def nodeOOCata[A](nExpr : (SLabel) => A,
                    n : Node[O,O]) : A = 
    n match {
      case Expr(sl) => nExpr(sl)
    }
}
