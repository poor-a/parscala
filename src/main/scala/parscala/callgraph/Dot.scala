package parscala
package callgraph

import java.io._
import java.nio.charset.Charset
import javax.swing.{JLabel,JScrollPane,JComponent,ImageIcon}
import javax.imageio.ImageIO
import scala.sys.process.{Process, ProcessBuilder}

import parscala.tree._

class Dot {
  case class DotNode(id : String, attr : List[DotAttr]) {
    def !!(a : DotAttr) : DotNode = 
      DotNode(id, a :: attr)

    val name : String = "node_%s".format(id)

    override def toString : String = 
      "%s %s;".format(name, attr.mkString("[", ",", "]"))
  }

  case class DotAttr(key : String, value : String) {
    override def toString : String = 
      "%s=%s".format(key, value)
  }

  case class DotEdge(source : DotNode, target : DotNode, attr : List[DotAttr]) {
    override def toString : String = 
      "%s -> %s %s;".format(source.name, target.name, attr.mkString("[", ",", "]"))
  }

  case class DotGraph(name : String, blocks : TraversableOnce[DotNode], edges : TraversableOnce[DotEdge]) {
    override def toString : String = 
      "digraph %s {\n%s\n%s\n}".format(name, blocks.mkString("\n"), edges.mkString("\n"))
  }

  private def shape(s : String) : DotAttr =
    DotAttr("shape", s)

  private def label(l : String) : DotAttr = 
    DotAttr("label", "\"%s\"".format(l))

  private def formatMethod(m : Method) : DotNode = {
    val name = m.symbol.fullName.toString
    val id : String = dotLegaliseId(name)
    val l : DotAttr = label(dotEscape(name))
    DotNode(id, List(shape("rectangle"), l))
  }

  def dotEscape(s : String) : String = {
    val subs : List[(String,String)] = 
      List("\"" -> "\\\"",
           "<" -> "\\<",
           ">" -> "\\>")
    subs.foldLeft(s)((acc,p) => acc.replaceAllLiterally(p._1, p._2))
  }

  def dotLegaliseId(id : String) : String = {
    val toRemove : List[String] = List("\"", "<", ">", "$")
    val noDots : String = id.replaceAll("\\.", "_")
    toRemove.foldLeft(noDots)((acc,s) => acc.replaceAllLiterally(s, ""))
  }

  def drawGraph(graph : DotGraph) : JComponent = {
    invokeDot(graph) match {
      case Some(image) =>
        new JScrollPane(new JLabel(new ImageIcon(ImageIO.read(image))))
      case None => 
        new JLabel("Error happened during image generation.")
    }
  }

  private def invokeDot(input : DotGraph) : Option[InputStream] = {
    val bytes : Array[Byte] = input.toString.getBytes(Charset.forName("UTF-8"))
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

  private def formatEdge(e : callgraph.Edge) : DotEdge =
    DotEdge(formatMethod(e.caller), formatMethod(e.callee), List.empty)

  def format(g : CallGraph) : DotGraph = {
    val methods : Set[Method] = g.calls.foldLeft(Set.empty[Method]){(acc, call) => acc + call.caller + call.callee}
    val nodes : Set[DotNode] = methods map formatMethod
    val edges : List[DotEdge] = g.calls map formatEdge
    DotGraph("callgraph", nodes, edges)
  }
}
