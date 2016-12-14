package parscala
package controlflow

import parscala.dot._

object CFGPrinter {
  def topoSort(graph : CFGraph) : List[BLabel] = {
    def sort(from : BLabel, marked : Set[BLabel], sorted : List[BLabel]) : (Set[BLabel],List[BLabel]) = {
      if (marked(from)) {
          (marked, sorted)
      } else {
        if (from == graph.done) {
          (marked + from, from :: sorted)
        } else {
          graph.get(from) map (_.successors) match {
            case Some(succs : List[(BLabel, EdgeLabel.TagType)]) => 
              succs.foldLeft((marked + from, from :: sorted)){(acc, succ) =>
                val (l, _) = succ
                val (m, s) = acc
                sort(l, m, s)
              }
            case None => (marked, sorted)
          }
        }
      }
    }

    sort(graph.start, Set(), List())._2.reverse
  }

  def formatLabels(graph : CFGraph, labels : List[(BLabel,Int)]) : (List[DotNode], List[DotEdge]) = {
    def formatLabel(x : (BLabel, Int)) : Option[(DotNode, List[DotEdge])] = {
      val (l, n) = x
      l match {
        case graph.start => {
          graph.get(l) match {
            case Some(b) => {
              val succs : List[BLabel] = b.successors.map(_._1)
              val edges : List[DotEdge] = for (succ <- succs; 
                                               mId = labels.find(_._1 == succ); 
                                               if (!mId.isEmpty);
                                               targetId = mId.get._2)
                                         yield edge(node(n), node(targetId))
              Some((DotNode(n.toString) !! DotAttr.label("Start"), edges))
            }
            case None =>
              None
          }
        }
        case graph.done => {
          if (!graph.get(l).isEmpty)
            Some((DotNode(n.toString) !! DotAttr.label("Done"), List.empty))
          else
            None
        }
        case _ => {
          def contents(block : Block[Node,_,_]) : List[String] = block match {
            case BFirst(node) => List(formatNode(node, n))
            case BMiddle(node) => List(formatNode(node, n))
            case BLast(node) => List(formatNode(node, n))
            case BCat(node1, node2) => contents(node1) ++ contents(node2)
          }
          def edges(block : Block[Node,_,C]) : List[DotEdge] = block match {
            case BLast(node) => formatEdges(node, n, labels)
            case BCat(node1, node2) => edges(node2)
            case _ => List()
          }
          
          graph.get(l) match {
            case Some(b : Block[Node,C,C]) => {
              val header :: stmts = contents(b)
              val prettyStmts : String = stmts.mkString("\\l")
              val content : String = "{ %s | %s }".format(header, prettyStmts)

              Some((DotNode(n.toString) !! DotAttr.shape("record") !! DotAttr.labelWithPorts(content), edges(b)))
            }
            case None => None
          }
        }
      }
    }

    labels.foldLeft(List[DotNode](), List[DotEdge]()) {(acc, x) =>
      val (nodes, edges) = acc
      formatLabel(x) map (x => (x._1 :: nodes, x._2 ++ edges)) getOrElse acc
    }
  }

  def formatEdges(n : Node[O,C], id : Int, labels : List[(BLabel,Int)]) : List[DotEdge] = {
    def getId(label : BLabel) : Option[Int] = labels.find(_._1 == label).map(_._2)

    for ((Some(targetId), label) <- n.successors map { x => (getId(x._1), x._2) })
    yield label match {
            case EdgeLabel.T => 
              edgeP(node(id), pTrue, node(targetId))
            case EdgeLabel.F =>
              edgeP(node(id), pFalse, node(targetId))
            case EdgeLabel.NoLabel =>
              edge(node(id), node(targetId))
          }
  }

  def formatNode(n : Node[_,_], i : Int) : String = {
    n match {
      case NLabel(_, _) => "Block " + i.toString
      case NLiteral(_, lit) => Dot.dotEscape(lit.toString)
      case NVariable(_, variable) => Dot.dotEscape(variable.toString)
      case NValDef(_, _, valdef) => Dot.dotEscape(valdef.toString)
      case NAssign(_, _, _, assign) => Dot.dotEscape(assign.toString)
      case NApp(_, _, _, app) => Dot.dotEscape(app.toString)
      case NNew(_, _, _, n) => Dot.dotEscape(n.toString)
      case NSelect(_, _, _, sel) => Dot.dotEscape(sel.toString)
      case NThis(_, _, _, t) => Dot.dotEscape(t.toString)
      case NTuple(_, _, tuple) => Dot.dotEscape(tuple.toString)
      case NPattern(_, pat, _, _) => Dot.dotEscape(pat.toString)
      case NCond(_, expr, _, _, _) => formatNode(expr, i) + (" | {<%s> T | <%s> F}".format(pTrue, pFalse))
      case NExpr(_, expr) => Dot.dotEscape(expr.toString)
      case NException(_, exception,_ ) => "throw " + Dot.dotEscape(exception.toString)
      case NBranch(_, _, _) => ""
      case NJump(_, _) => ""
      case NReturn(_, _, _, ret) => Dot.dotEscape(ret.toString)
      case NThrow(_, _, _, thr) => Dot.dotEscape(thr.toString)
      case NDone(_) => ""
    }
  }

  def formatGraph(graph : CFGraph) : DotGraph = {
    val labels : List[BLabel] = topoSort(graph)
    val (nodes, edges) = formatLabels(graph, labels zip (1 to labels.length))
    DotGraph("CFG", nodes, edges)
  }

  private val pTrue : String = "p0"
  private val pFalse : String = "p1"

  private def node(n : Int) : DotNode = 
    DotNode(n.toString)
  private def edge(source : DotNode, target : DotNode) : DotEdge = 
    DotEdge(source, target, List())
  private def edgeP(source : DotNode, port : String, target : DotNode) : DotEdge = 
    edge(source, target) sourcePort port
}
