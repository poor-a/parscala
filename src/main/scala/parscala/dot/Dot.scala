package parscala
package dot

import java.io._
import java.nio.charset.Charset
import javax.swing.{JLabel,JScrollPane,JComponent,ImageIcon}
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.util.concurrent.TimeUnit

import scala.io.{Source,Codec}

class DotNode(val name : String, val attrs : List[DotAttr]) {
  def !!(a : DotAttr) : DotNode = 
    new DotNode(name, a :: attrs)

  def !!(as : List[DotAttr]) : DotNode =
    new DotNode(name, as ++ attrs)

  override def toString : String = 
    "%s %s;".format(name, attrs.mkString("[", ",", "]"))
}

object DotNode {
  def dotLegaliseId(id : String) : String = {
    val toRemove : List[String] = List("\"", "<", ">", "$")
    val noDots : String = id.replaceAll("\\.", "_")
    toRemove.foldLeft(noDots)((acc,s) => acc.replaceAllLiterally(s, ""))
  }
  
  def apply(id : String) : DotNode =
    new DotNode("node_%s".format(dotLegaliseId(id)), List.empty)
}

class DotAttr(val key : String, val value : String) {
  override def toString : String = 
    "%s=%s".format(key, value)
}

object DotAttr {
  def shape(s : String) : DotAttr =
    new DotAttr("shape", s)

  def label(l : String) : DotAttr = 
    new DotAttr("label", "\"%s\"".format(Dot.dotEscape(l)))

  def labelWithPorts(l : String) : DotAttr = 
    new DotAttr("label", "\"%s\"".format(l))
}

class DotEdge(val source : DotNode, val target : DotNode, val sPort : Option[String], val attrs : List[DotAttr]) {
  override def toString : String = {
    val addPort = (port : String) =>
      "%s%s -> %s %s;".format(source.name, port, target.name, attrs.mkString("[", ",", "]"))
    sPort match {
      case Some(port) => addPort(":" + port)
      case None => addPort("")
    }
  }

  def sourcePort(port : String) : DotEdge = {
    new DotEdge(source, target, Some(port), attrs)
  }
}

object DotEdge {
  def apply(source : DotNode, target : DotNode, attr : List[DotAttr]) : DotEdge =
    new DotEdge(source, target, None, attr)
}

class DotGraph(val name : String, val blocks : TraversableOnce[DotNode], val edges : TraversableOnce[DotEdge]) {
  override def toString : String = 
    "digraph %s {\n%s\n%s\n}".format(name, blocks.mkString("\n"), edges.mkString("\n"))
}

object DotGraph {
  def apply(name : String, blocks : TraversableOnce[DotNode], edges : TraversableOnce[DotEdge]) : DotGraph =
    new DotGraph(name, blocks, edges)
}

object Dot {
  def dotEscape(s : String) : String = {
    val subs : List[(String,String)] = 
      List("\"" -> "\\\"",
           "<" -> "\\<",
           ">" -> "\\>",
           "\n" -> "\\l",
           "{" -> "\\{",
           "}" -> "\\}")
    subs.foldLeft(s)((acc,p) => acc.replaceAllLiterally(p._1, p._2))
  }
}

class Dot {
  def drawGraph(graph : DotGraph) : Either[String, JComponent] = {
    invokeDot(graph) match {
      case Right(image) =>
        Right(new JScrollPane(new JLabel(new ImageIcon(image))))
      case Left(err) => 
        Left(err)
    }
  }

  private def invokeDot(g : DotGraph) : Either[String, BufferedImage] = {
    val utf8 : Charset = Charset.forName("UTF-8")
    try {
      val dot : Process = new ProcessBuilder("dot", "-Tpng").start()
      val dotInput = new PrintWriter(new OutputStreamWriter(dot.getOutputStream, utf8))
      dotInput.print(g.toString)
      dotInput.flush()
      dotInput.close()
      val image : BufferedImage = ImageIO.read(dot.getInputStream)
      if (dot.waitFor(5, TimeUnit.SECONDS))
        if (dot.exitValue == 0) {
          Right(image)
        } else {
          val log = Source.fromInputStream(dot.getErrorStream)(Codec(utf8))
          Left("Error happened during image generation. May indicate syntax error in the dot graph.\nDot error log:\n%s\nInput: %s\n".format(log.mkString,g.toString))
        }
      else {
        dot.destroy()
        Left("Dot did not terminate. Destroyed.")
      }
    } 
    catch {
      case e : IOException =>
        Left("Could not find dot. Perhaps it is not installed?")
    } 
  }
}
