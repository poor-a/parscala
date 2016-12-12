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
    n match {
      case NJump(_, l) => 
        getId(l) match {
          case Some(targetId) => 
            List(edge(node(id), node(targetId)))
          case None =>
            List()
        }
      case NCond(_, _, t, f, _) => 
        for ((Some(targetId),port) <- List((getId(t), pTrue),(getId(f), pFalse)))
        yield edgeP(node(id), port, node(targetId))
      case NBranch(_, l1, l2) =>
        for (Some(targetId) <- List(getId(l1),getId(l2)))
        yield edge(node(id), node(targetId))
      case NReturn(_, _, target, _) =>
        getId(target)
          .map{ targetId => List(edge(node(id), node(targetId))) } 
          .getOrElse(List.empty)
      case NException(_, _, handler) =>
        getId(handler)
          .map{ handlerId => List(edge(node(id), node(handlerId))) } 
          .getOrElse(List())
      case NThrow(_, _, _, _) =>
        ???
      case NDone(_) =>
        ???
    }
  }

  def formatNode(n : Node[_,_], i : Int) : String = {
    n match {
      case NLabel(_, _) => "Block " + i.toString
      case NCond(_, expr, _, _, _) => Dot.dotEscape(formatNode(expr, i)) + (" | {<%s> T | <%s> F}".format(pTrue, pFalse))
      case NExpr(_, expr) => Dot.dotEscape(expr.toString)
      case NException(_, exception,_ ) => "throw " + Dot.dotEscape(exception.toString)
      case _ => ""
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
