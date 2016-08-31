package parscala;

import parscala.dot._

object CFGPrinter {
  def topoSort(graph : CFGraph) : List[Label] = {
    def sort(from : Label, marked : Set[Label], sorted : List[Label]) : (Set[Label],List[Label]) = {
      if (marked(from)) {
          (marked, sorted)
      } else {
        if (from == Done) {
          (marked + from, from :: sorted)
        } else {
          val succs : List[Label] = graph(from).successors
          succs.foldLeft((marked + from, from :: sorted)){(acc, l) =>
            val (m, s) = acc
            sort(l, m, s)
          }
        }
      }
    }

    sort(Start, Set(), List())._2.reverse
  }

  def formatLabels(graph : CFGraph, labels : List[(Label,Int)]) : (List[DotNode], List[DotEdge]) = {
    def formatLabel(x : (Label, Int)) : (DotNode, List[DotEdge]) = {
      val (l, n) = x
      l match {
        case Start => {
          val succs : List[Label] = graph(l).successors
          val edges : List[DotEdge] = for (succ <- succs; 
                                          mId = labels.find(_._1 == succ); 
                                          if (!mId.isEmpty);
                                          targetId = mId.get._2)
                                     yield edge(DotNode(n.toString), DotNode(targetId.toString))

          (DotNode(n.toString) !! DotAttr.label("Start"), edges)
        }
        case Done => {
          (DotNode(n.toString) !! DotAttr.label("Done"), List.empty)
        }
        case _ => {
          val b : Block[Node,C,C] = graph(l)
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
          val header :: stmts = contents(b)
          val prettyStmts : String = stmts.mkString("\\l")
          val content : String = "{ %s | %s }".format(header, prettyStmts)

          (DotNode(n.toString) !! DotAttr.shape("record") !! DotAttr.labelWithPorts(content), edges(b))
        }
      }
    }

    labels.foldLeft(List[DotNode](), List[DotEdge]()) {(acc, x) =>
      val (nodes, edges) = acc
      val (xNode, xEdges) = formatLabel(x)
      (xNode :: nodes, xEdges ++ edges)
    }
  }

  def formatEdges(node : Node[O,C], id : Int, labels : List[(Label,Int)]) : List[DotEdge] = {
    def getId(label : Label) : Option[Int] = labels.find(_._1 == label).map(_._2)
    node match {
      case NJump(l) => 
        getId(l) match {
          case Some(targetId) => List(edge(DotNode(id.toString), DotNode(targetId.toString)))
          case None => List()
        }
      case NCond(_,t,f) => 
        for ((targetId,port) <- List((getId(t),"p0"),(getId(f),"p1"));
             if (!targetId.isEmpty))
        yield edgeP(DotNode(id.toString), port, DotNode(targetId.get.toString))
    }
  }

  def formatNode(n : Node[_,_], i : Int) : String = {
    n match {
      case NLabel(_) => "Block " + i.toString
      case NStmt(stmt) => Dot.dotEscape(stmt.toString)
      case NCond(expr,_,_) => Dot.dotEscape(expr.ast.toString) + " | {<p0> T | <p1> F}"
      case NJump(_) => ""
      case _ => ""
    }
  }

  def formatGraph(graph : CFGraph) : DotGraph = {
    val labels : List[Label] = topoSort(graph)
    val (nodes, edges) = formatLabels(graph, labels zip (1 to labels.length))
    DotGraph("CFG", nodes, edges)
  }

  private def edge(source : DotNode, target : DotNode) : DotEdge = 
    DotEdge(source, target, List())
  private def edgeP(source : DotNode, port : String, target : DotNode) : DotEdge = 
    edge(source, target) sourcePort port
}
