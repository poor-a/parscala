package parscala;

import java.io._
import java.nio.charset.Charset
import javax.swing.{JLabel,JScrollPane,JComponent,ImageIcon}
import javax.imageio.ImageIO
import scala.sys.process.{Process, ProcessBuilder}

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

  def formatLabels(graph : CFGraph, labels : List[(Label,Int)]) : String = {
    def formatLabel(x : (Label, Int)) : String = {
      val (l, n) = x
      l match {
        case Start => {
          val id : String = block(n)
          val succs : List[Label] = graph(l).successors
          val edges : List[String] = for (succ <- succs; 
                                          mId = labels.find(_._1 == succ); 
                                          if (!mId.isEmpty);
                                          targetId = mId.get._2)
                                     yield edge(id, block(targetId))

          String.format("%s [label=\"Start\"]\n%s", id, edges.mkString("\n"))
        }
        case Done => {
          val id : String = block(n)
          String.format("%s [label=\"Done\"]", id)
        }
        case _ => {
          val b : Block[Node,C,C] = graph(l)
          val id : String = block(n)
          def contents(block : Block[Node,_,_]) : List[String] = block match {
            case BFirst(node) => List(formatNode(node, n))
            case BMiddle(node) => List(formatNode(node, n))
            case BLast(node) => List(formatNode(node, n))
            case BCat(node1, node2) => contents(node1) ++ contents(node2)
          }
          def edges(block : Block[Node,_,C]) : List[String] = block match {
            case BLast(node) => formatEdges(node, n, labels)
            case BCat(node1, node2) => edges(node2)
            case _ => List()
          }
          val header :: stmts = contents(b)
          val prettyStmts : String = stmts.mkString("\\l")
          val prettyEdges : String = edges(b).mkString("\n")

          String.format("%s [shape=record, label=\"{ %s | %s }\"];\n%s", id, header, prettyStmts, prettyEdges)
        }
      }
    }

    labels.map(formatLabel).mkString("\n")
  }

  def formatEdges(node : Node[O,C], id : Int, labels : List[(Label,Int)]) : List[String] = {
    def getId(label : Label) : Option[Int] = labels.find(_._1 == label).map(_._2)
    node match {
      case NJump(l) => 
        getId(l) match {
          case Some(targetId) => List(edge(block(id), block(targetId)))
          case None => List()
        }
      case NCond(_,t,f) => 
        for ((targetId,port) <- List((getId(t),"p0"),(getId(f),"p1"));
             if (!targetId.isEmpty))
        yield edgeP(block(id), port, block(targetId.get))
    }
  }

  def formatNode(n : Node[_,_], i : Int) : String = { 
    def dotEscape(s : String) : String = {
      val subs : List[(String,String)] = 
        List(("\"" -> "\\\""),
             ("<" -> "\\<"),
             (">" -> "\\>"))
      subs.foldLeft(s)((acc,p) => acc.replaceAllLiterally(p._1, p._2))
    }

    n match {
      case NLabel(_) => "Block " + i.toString
      case NStmt(stmt) => dotEscape(stmt.toString)
      case NCond(expr,_,_) => dotEscape(expr.ast.toString) + " | {<p0> T | <p1> F}"
      case NJump(_) => ""
      case _ => ""
    }
  }

  def formatGraph(graph : CFGraph) : String = {
    val labels : List[Label] = topoSort(graph)
    val dotGraph : String = formatLabels(graph, labels zip (1 to labels.length))
    s"digraph CFG {\n$dotGraph\n}"
  }

  def drawGraph(graph : CFGraph) : JComponent = {
    invokeDot(formatGraph(graph)) match {
      case Some(image) =>
        new JScrollPane(new JLabel(new ImageIcon(ImageIO.read(image))))
      case None => 
        new JLabel("Error happened during image generation.")
    }
  }


  private def invokeDot(input : String) : Option[InputStream] = {
    val bytes : Array[Byte] = input.getBytes(Charset.forName("UTF-8"))
    val dotInput : InputStream = new BufferedInputStream(new ByteArrayInputStream(bytes))
    val image : ByteArrayOutputStream = new ByteArrayOutputStream
    val dotOutput : BufferedOutputStream = new BufferedOutputStream(image)
    val dot : ProcessBuilder = Process("dot", List("-Tpng"))
    val exitCode : Int = (dot #< dotInput #> dotOutput).!
    val readOnlyImage : Option[InputStream] = 
      if (exitCode == 0) {
        dotOutput.flush()
        Some(new BufferedInputStream(new ByteArrayInputStream(image.toByteArray())))
      } else {
        None
      }
    dotInput.close()
    dotOutput.close()
    readOnlyImage
  }

  private def block(n : Int) : String = s"block$n"
  private def edge(source : String, target : String) : String = 
    s"$source -> $target;"
  private def edgeP(source : String, port : String, target : String) : String = 
    s"$source:$port -> $target;"
}
