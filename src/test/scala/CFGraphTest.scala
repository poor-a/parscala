import org.scalatest.FunSuite

import parscala._
import parscala.controlflow._
import parscala.{tree => tr}

class ControlDependencySuite extends FunSuite {
  val labels : Array[BLabel] = BLabel.stream.take(10).toArray

  val entry : BLabel = labels(0)
  val start : BLabel = labels(1)
  val done : BLabel = labels(9)

  val (pgraph, condLabel) : (ProgramGraph, SLabel) =
    parscala.ParScala.astOfExpr("1 < 2") match {
      case Some(ast) =>
        tr.Node.fromTree(ast) match {
          case (pgraph, Some(tree)) => (pgraph, tree.root.label)
          case _ => fail("could not create NodeTree from an expression")
        }
      case _ =>
        fail("Could not create a Tree")
    }

  def empty0(s : BLabel) : Block[Node,C,C] = BCat(BFirst(Label(s)), BLast(Jump(done)))
  def empty(s : BLabel, n : BLabel) : Block[Node,C,C] = BCat(BFirst(Label(s)), BLast(Jump(n)))
  def empty2(s : BLabel, n1 : BLabel, n2 : BLabel) : Block[Node,C,C] = BCat(BFirst(Label(s)), BLast(Cond(condLabel, n1, n2)))

  val nodes : List[Block[Node,C,C]] = List(empty(start, labels(2)), empty2(labels(2),labels(3),labels(4)), empty2(labels(3),labels(5),labels(6)), empty2(labels(4),labels(6),labels(8)), empty(labels(5),labels(7)), empty(labels(6),labels(7)), empty(labels(7),labels(8)),empty(labels(8),done))

  val cfg : CFGraph = new CFGraph(empty2(entry, start, done), empty0(done), pgraph) + nodes

  val ecfg : ExtensibleCFGraph = new ExtensibleCFGraph(new CFGraph(empty(start, labels(2)), empty0(done), pgraph) + nodes, BLabel.stream.drop(10), SLabel.stream)

  test("the precedessors of entry are done and start in the reverse cfg") {
    assertResult(List(start,done))(cfg.reverse.pred(entry))
  }

  test("dominators of a test CFG are correct") {
    val doms : Map[BLabel, Set[BLabel]] = Map(
        done -> Set(done)
      , entry -> Set(entry, done)
      , labels(8) -> Set(labels(8), done)
      , labels(7) -> Set(labels(7), labels(8), done)
      , labels(4) -> Set(labels(4), labels(8), done)
      , labels(2) -> Set(labels(2), labels(8), done)
      , labels(5) -> Set(labels(5), labels(7), labels(8), done)
      , labels(6) -> Set(labels(6), labels(7), labels(8), done)
      , labels(3) -> Set(labels(3), labels(7), labels(8), done)
      , start -> Set(start, labels(2), labels(8), done))
  
    assertResult(doms)(cfg.reverse.dominators)
  }

  test("intermediate dominators of a test CFG are correct")  {
    val rev : ReverseCFGraph = cfg.reverse
    val idoms : DomTree = new DomTree(Map(entry -> Some(done), start -> Some(labels(2)), labels(2) -> Some(labels(8)), labels(3) -> Some(labels(7)), labels(4) -> Some(labels(8)), labels(5) -> Some(labels(7)), labels(6) -> Some(labels(7)), labels(7) -> Some(labels(8)), labels(8) -> Some(done), done -> None))

    assertResult(idoms)(rev.immediateDominators(rev.dominators))
  }

  test("local CFG does not contain method application") {
    import parscala.compiler.Quasiquote
    tr.Node.fromTree(q"""
        val n : Int = 2
        val m : Int = n + 1
        m
      """) match {
        case (pgraph, Some(tree)) =>
          val cfg_ : CFGraph = CFGraph.fromExpression(tree.root, pgraph)
          val local : CFGraph = cfg_.local
          def hasMethodApp (b : Block[Node, C, C]) : Boolean =
            Block.lastNode(b) match {
              case Call(_, _, _) => true
              case _ => false
            }
          assert(local.traverse[Boolean]((b, soFar) => soFar && !hasMethodApp(b), true))
        case _ =>
          fail("could not create a NodeTree from an expression")
      }
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
