package parscala
package df

import parscala.controlflow._

import scalaz.State

/** 
 *  Dataflow edge label.
 */
object DEdgeLabel extends Enumeration {
  type LabelType = Value

  val F, D, C, S = Value
}


object DFGraph {
  private type ReachingDef = List[(Symbol, SLabel)]
  private type St = (ReachingDef, SLabelGen, DFGraph)
  private type DFGen[A] = State[St, A]

  def killReachingDef(t : Tree) : ReachingDef = ???
  def genReachingDef(t : Tree) : ReachingDef = ???

  def apply(cfg : CFGraph, gen : SLabelGen) : DFGraph = {
    ???
  }

  def mkDFGraph(b : Block[Node,_,_], cfg : CFGraph) : DFGen[Unit] = {
    b match {
      case BFirst(_) => State.state(())
      case BMiddle(n) => stmtMiddleDF(n, cfg)
      case BLast(n) => stmtLastDF(n, cfg)
      case BCat(b1, b2) => for (_ <- mkDFGraph(b1, cfg);
                                _ <- mkDFGraph(b2, cfg))
                           yield ()
    }
  }

  def stmtMiddleDF(n : Node[O,O], cfg : CFGraph) : DFGen[Unit] = {
    n match {
      case NExpr(_, e) =>
        ???
      case NAssign(_, _, _, _) =>
        ???
    }
  }

  def stmtLastDF(n : Node[O,C], cfg : CFGraph) : DFGen[Unit] = {
    n match {
      case NCond(_, e, _, _, _) => 
        ???
      case NBranch(_, _, _) =>
        State.state(())
      case NReturn(_, e, _, _) =>
        ???
      case NThrow(_, e, _, _) =>
        ???
      case NException(_, _, _) =>
        ???
      case NJump(_, _) =>
        State.state(())
      case NDone(_) =>
        State.state(())
    }
  }
}

class DFGraph(val graph : List[(SLabel, DEdgeLabel.LabelType, SLabel)], val cfg : CFGraph) {
  def +(e : (SLabel, DEdgeLabel.LabelType, SLabel)) : DFGraph =
    new DFGraph(e :: graph, cfg)
}
