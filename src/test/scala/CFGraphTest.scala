import org.scalatest.flatspec.AnyFlatSpec

import parscala._
import parscala.controlflow._
import parscala.{controlflow => cf}
import parscala.{tree => tr}

class ControlDependencySuite extends AnyFlatSpec {
  val condLabel : SLabel = new SLabel(0)
  val cond : tr.TypedExpr = tr.Literal(condLabel, scala.meta.Lit.Boolean(true), List(parscala.ParScala.astOfExpr("true").get.tpe))
  val pgraph : ProgramGraph = ProgramGraph(Map(), Map(), Map(), Map(), Map(), List(), Map())

  val labels : Array[BLabel] = BLabel.stream.take(10).toArray

  val entry : BLabel = labels(0)
  val start : BLabel = labels(1)
  val done : BLabel = labels(9)

  def empty0(s : BLabel) : Block[Node,cf.C,cf.C] = BCat(BFirst(Label(s)), BLast(Jump(done)))
  def empty(s : BLabel, n : BLabel) : Block[Node,cf.C,cf.C] = BCat(BFirst(Label(s)), BLast(Jump(n)))
  def empty2(s : BLabel, n1 : BLabel, n2 : BLabel) : Block[Node,cf.C,cf.C] = BCat(BFirst(Label(s)), BLast(Cond(condLabel, n1, n2)))

  val nodes : List[Block[Node,cf.C,cf.C]] = List(empty(start, labels(2)), empty2(labels(2),labels(3),labels(4)), empty2(labels(3),labels(5),labels(6)), empty2(labels(4),labels(6),labels(8)), empty(labels(5),labels(7)), empty(labels(6),labels(7)), empty(labels(7),labels(8)),empty(labels(8),done))

  val cfg : CFGraph = new CFGraph(Map(), empty2(entry, start, done), empty0(done), Map(), pgraph) + nodes

  val ecfg : ExtensibleCFGraph = new ExtensibleCFGraph(new CFGraph(Map(), empty(start, labels(2)), empty0(done), Map(), pgraph) + nodes, BLabel.stream.drop(10), SLabel.stream)

  "the precedessors of entry" should "be done and start in the reverse cfg" in {
    assertResult(List(start,done))(cfg.reverse.pred(entry))
  }

  "dominators of a test CFG" should "be correct" in {
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

  "intermediate dominators of a test CFG" should "be correct" in {
    val rev : ReverseCFGraph = cfg.reverse
    val idoms : DomTree = new DomTree(Map(entry -> Some(done), start -> Some(labels(2)), labels(2) -> Some(labels(8)), labels(3) -> Some(labels(7)), labels(4) -> Some(labels(8)), labels(5) -> Some(labels(7)), labels(6) -> Some(labels(7)), labels(7) -> Some(labels(8)), labels(8) -> Some(done), done -> None))

    assertResult(idoms)(rev.immediateDominators(rev.dominators))
  }

  "local CFG" should "not contain method application" in {
    val testInput : java.nio.file.Path = TestConfiguration.testInputDir.resolve("CFGraphTestInput.scala")
    parscala.ParScala.analyse(List(testInput), None) match {
      case Right((pgraph, warnings @ _)) =>
          val Some(obj) /*: tr.Defn.Object*/ = pgraph.objects.find(_.name == meta.Term.Name("TestNoMethodInvocations"))
          val List(Right(f)) = obj.methods
          val cfg_ : CFGraph = CFGraph.fromMethod(f, pgraph)
          def hasMethodApp (b : Block[Node, cf.C, cf.C]) : Boolean =
            Block.lastNode(b) match {
              case Call(_, _, _) => true
              case _ => false
            }
          assert(cfg_.traverse[Boolean]((b, soFar) => soFar && !hasMethodApp(b), true))
        case _ =>
          fail("could not create a ProgramGraph from a source file")
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
