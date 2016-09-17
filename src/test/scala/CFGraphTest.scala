import org.scalatest.FunSuite

import parscala._
import parscala.tree.Expression

import scala.collection.immutable.Stream

class ControlDependencySuite extends FunSuite {
  val entry : Label = -1
  val start : Label = 0
  val done : Label = 8

  val cfg : CFGraph = {
    val e : Expression = {
      import parscala.compiler.Quasiquote
      new Expression(q"1 + 2")
    }

    def empty0(s : Int) : Block[Node,C,C] = BCat(BFirst(NLabel(s)), BLast(NReturn()))
    def empty(s : Int, n : Int) : Block[Node,C,C] = BCat(BFirst(NLabel(s)), BLast(NJump(n)))
    def empty2(s : Int, n1 : Int, n2 : Int) = BCat(BFirst(NLabel(s)), BLast(NCond(e, n1, n2)))

    val nodes : List[Block[Node,C,C]] = List(empty(start, 1), empty2(1,2,3), empty2(2,4,5), empty2(3,5,7), empty(4,6), empty(5,6), empty(6,7),empty(7,8))
    new CFGraph(empty2(entry, start, done), empty0(done)) + nodes
  }

  val ecfg : ExtensibleCFGraph = {
    val e : Expression = {
      import parscala.compiler.Quasiquote
      new Expression(q"1 + 2")
    }

    def empty0(s : Int) : Block[Node,C,C] = BCat(BFirst(NLabel(s)), BLast(NReturn()))
    def empty(s : Int, n : Int) : Block[Node,C,C] = BCat(BFirst(NLabel(s)), BLast(NJump(n)))
    def empty2(s : Int, n1 : Int, n2 : Int) = BCat(BFirst(NLabel(s)), BLast(NCond(e, n1, n2)))

    val nodes : List[Block[Node,C,C]] = List(empty(start, 1), empty2(1,2,3), empty2(2,4,5), empty2(3,5,7), empty(4,6), empty(5,6), empty(6,7),empty(7,8))
    new ExtensibleCFGraph(new CFGraph(empty(start, 1), empty0(done)) + nodes, Stream.from(done + 1))
  }

  test("the precedessor of entry is done and start in the reverse cfg") {
    assertResult(List(start,done))(cfg.reverse.pred(entry))
  }

  test("dominators of a test CFG are correct") {
    val doms : Map[Label, Set[Label]] = Map(
        done -> Set(done)
      , entry -> Set(entry, done)
      , 7 -> Set(7, done)
      , 6 -> Set(6, 7, done)
      , 3 -> Set(3, 7, done)
      , 1 -> Set(1, 7, done)
      , 4 -> Set(4, 6, 7, done)
      , 5 -> Set(5, 6, 7, done)
      , 2 -> Set(2, 6, 7, done)
      , start -> Set(start, 1, 7, done))
  
    assertResult(doms)(cfg.reverse.dominators)
  }

  test("intermediate dominators of a test CFG are correct")  {
    val rev : ReverseCFGraph = cfg.reverse
    val idoms : DomTree = new DomTree(Map(entry -> Some(done), start -> Some(1), 1 -> Some(7), 2 -> Some(6), 3 -> Some(7), 4 -> Some(6), 5 -> Some(6), 6 -> Some(7), 7 -> Some(done), 8 -> None))

    assertResult(idoms)(rev.immediateDominators(rev.dominators))
  }

/*
  test("control edges of a test CFG are correct") {
    val (actCd, ecfg2) = ecfg.controlDependency
    val cEdges : List[(Label,Label,CEdgeTag.TagType)] = List(
        
      )
    val cd = new ControlDependency(ecfg2.freeze, actCd.entry, )
    assertResult()()
  }
*/
}
