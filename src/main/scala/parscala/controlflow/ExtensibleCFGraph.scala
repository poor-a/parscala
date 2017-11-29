package parscala
package controlflow

class ExtensibleCFGraph(graph : CFGraph, val bGen : BLabelGen, val sGen : SLabelGen){
  def emptyBlock(gen : BLabelGen) : (Block[Node,C,O], BLabelGen) =
    (BFirst(Label(gen.head)), gen.tail)

  def mkBLabel : (BLabel, BLabelGen) = 
    (bGen.head, bGen.tail)

  def mkSLabel : (SLabel, SLabelGen) = 
    (sGen.head, sGen.tail)

  val (start, done) : (BLabel, BLabel) = (graph.start.entryLabel, graph.done.entryLabel)

  def +(block : Block[Node,C,C]) : ExtensibleCFGraph = 
    new ExtensibleCFGraph(graph + block, bGen, sGen)

  def +(blocks : List[Block[Node,C,C]]) : ExtensibleCFGraph = 
    blocks.foldLeft(this)(_ + _)

  def update(l : BLabel, f : Block[Node,C,C] => Block[Node,C,C]) =
    graph.get(l) map {b => new ExtensibleCFGraph(new CFGraph(graph.graph.updated(l, f(b)), graph.start, graph.done, graph.pgraph), bGen, sGen)} getOrElse this

  def freeze : CFGraph = 
    graph

  type CEdge = (BLabel, BLabel, EdgeLabel.TagType)
  type CEdges = List[CEdge]
  type BLabelGen = Stream[BLabel]
  type SLabelGen = Stream[SLabel]
  type EdgeTag = EdgeLabel.TagType

  def insertEntry : ExtensibleCFGraph = {
    val (empty, bGen2) = emptyBlock(bGen)
    val entry : Block[Node,C,C] = BCat(empty, BLast(Branch(start, done)))
    val extended : CFGraph = new CFGraph(graph.graph + (entry.entryLabel -> entry), entry, graph.done, graph.pgraph)
    new ExtensibleCFGraph(extended, bGen2, sGen)
  }

  def controlPrecedessors(l : BLabel, cEdges : CEdges) : List[(BLabel, EdgeTag)] =
    for ((s, t, tag) <- cEdges if (t == l)) yield (s, tag)

  def insertRegions(ls : Iterable[BLabel], cEdges : CEdges, regions : Map[Set[(BLabel, EdgeTag)], BLabel], gen : BLabelGen) : (CEdges, Map[Set[(BLabel, EdgeTag)], BLabel], BLabelGen) = 
    ls.foldLeft((cEdges, regions, gen)){ (acc, l) => insertRegion(l, acc._1, acc._2, acc._3) }

  def insertRegion(l : BLabel, cEdges : CEdges, regions : Map[Set[(BLabel, EdgeTag)], BLabel], gen : BLabelGen) : (CEdges, Map[Set[(BLabel, EdgeTag)], BLabel], BLabelGen) = {
    val prec : List[(BLabel, EdgeTag)] = controlPrecedessors(l, cEdges)
    val precS : Set[(BLabel, EdgeTag)] = prec.toSet
    if (!precS.isEmpty) {
      val otherEdges : CEdges = cEdges filter (_._2 != l)
      (regions get precS) match {
        case Some(region : BLabel) => 
          ((region, l, EdgeLabel.NoLabel) :: otherEdges, regions, gen)
        case None =>
          val (region, gen2) = (gen.head, gen.tail)
          val regionEdges : CEdges = prec map {case (s, tag) => (s, region, tag)} 
          ((region, l, EdgeLabel.NoLabel) :: regionEdges ++ otherEdges, regions + (precS -> region), gen2)
      }
    } else
      (cEdges, regions, gen)
  }

  type Edge = (BLabel, BLabel)
  type Region = BLabel

  def factor(cEdges : CEdges, regions : Map[Set[(BLabel, EdgeTag)], BLabel], cEdgesWithRegion : CEdges, domTree : DomTree) : CEdges = {
    def doFactor(l : BLabel, g : CEdges) : CEdges = {
      val prec : List[(BLabel, EdgeTag)] = controlPrecedessors(l, cEdges)
      val precS : Set[(BLabel, EdgeTag)] = prec.toSet
      domTree.children(l).foldLeft(g){(acc, child) => 
        val childPrec = controlPrecedessors(child, cEdges).toSet
        val inter : Set[(BLabel, EdgeTag)] = precS & childPrec
        if (inter == precS) {
          (regions get precS, regions get childPrec) match {
            case (Some(regionParent), Some(regionChild)) if (regionParent != regionChild) =>
              val otherEdges : CEdges = acc filterNot {case (s,t,tag) => t == regionChild && inter((s,tag))}
              (regionParent, regionChild, EdgeLabel.NoLabel) :: otherEdges
            case _ =>
                acc
          }
        } else if (inter == childPrec) {
          (regions get precS, regions get childPrec) match {
            case (Some(regionParent), Some(regionChild)) if (regionParent != regionChild) =>
              val otherEdges : CEdges = acc filterNot {case (s, t, tag) => t == regionParent && inter((s, tag))}
              (regionChild, regionParent, EdgeLabel.NoLabel) :: otherEdges
            case _ =>
              acc
          }
        } else
            acc
      }
    }

    domTree.postOrder(doFactor, cEdgesWithRegion)
  }

  def merge(cEdges : CEdges, gen : BLabelGen) : (CEdges, BLabelGen) = {
    val (pEdges, nonPredicateEdges) = cEdges partition {case (_,_,tag) => tag == EdgeLabel.T || tag == EdgeLabel.F}
    val edges : Map[(BLabel, EdgeTag), List[(BLabel, BLabel, EdgeTag)]] = pEdges groupBy { case (s, _, tag) => (s, tag) }
    val (merged : List[CEdge], gen2) = edges.foldLeft((List.empty[CEdge], gen)){ (acc, g) =>
      val (edges, gen) = acc
      g._2 match {
        case List(e) => (e :: edges, gen)
        case es => 
          val (region, gen2) = (gen.head, gen.tail)
          val (s, tag) = g._1
          val merged : List[CEdge] = es map { case (_, t, _) => (region, t, EdgeLabel.NoLabel) }
          ((s, region, tag) :: merged ++ edges, gen2)
      }
    }
    (merged ++ nonPredicateEdges, gen2)
  }

  def controlDependency : (ControlDependency, ExtensibleCFGraph) = {
    val (e, bGen2) = emptyBlock(bGen)
    val entry : Block[Node,C,C] = BCat(e, BLast(Branch(start, done)))
    val extended : CFGraph = new CFGraph((graph + entry).graph, entry, graph.done, graph.pgraph)
    val revCFG : ReverseCFGraph = extended.reverse
    val domTree : DomTree = revCFG.immediateDominators(revCFG.dominators)
    val controlEdges : CEdges = revCFG.controlDependency

    val labels : Set[BLabel] = extended.graph.keySet

    val (cEdges, regions, bGen3) = insertRegions(labels, controlEdges, Map.empty, bGen2)
    val cEdges2 : CEdges = factor(controlEdges, regions, cEdges, domTree)
    val (cEdges3, bGen4) = merge(cEdges2, bGen3)

    (new ControlDependency(extended, e.entryLabel, cEdges3), new ExtensibleCFGraph(extended, bGen4, sGen))
  }
}
