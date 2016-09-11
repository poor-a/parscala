import org.scalatest.FunSuite

import parscala._
import parscala.tree.Expression

class ControlDependencySuite extends FunSuite {
  test("intermediate dominators of a test CFG are correct")  {
    val e : Expression = {
      import parscala.compiler.Quasiquote
      new Expression(q"1 + 2")
    }

    val entry : Label = -1
    val start : Label = 0
    val done : Label = 8

    def empty0(s : Int) : Block[Node,C,C] = BCat(BFirst(NLabel(s)), BLast(NReturn()))
    def empty(s : Int, n : Int) : Block[Node,C,C] = BCat(BFirst(NLabel(s)), BLast(NJump(n)))
    def empty2(s : Int, n1 : Int, n2 : Int) = BCat(BFirst(NLabel(s)), BLast(NCond(e, n1, n2)))
    val nodes : List[Block[Node,C,C]] = List(empty(0,1), empty2(1,2,3), empty2(2,4,5), empty2(3,5,7), empty(4,6), empty(5,6), empty(6,7),empty(7,8), empty2(entry,start,done), empty0(done))
    val g : CFGraph = new CFGraph(entry, done) + nodes
    val rev : ReverseCFGraph = g.reverse
    val idoms : Map[Label, Option[Label]] = Map(entry -> Some(done), start -> Some(1), 1 -> Some(7), 2 -> Some(6), 3 -> Some(7), 4 -> Some(6), 5 -> Some(6), 6 -> Some(7), 7 -> Some(done), 8 -> None)

    assertResult(idoms)(rev.immediateDominators(rev.dominators))
  }
}
