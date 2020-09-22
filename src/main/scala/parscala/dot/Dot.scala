package parscala
package dot

import java.io._
import java.nio.charset.Charset
import javax.swing.{JLabel,JScrollPane,JComponent,ImageIcon}
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import java.util.concurrent.TimeUnit

import scala.io.{Source,Codec}
import scala.collection.immutable.Traversable

import scalaz.Show

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

  def fromSLabel(id : SLabel) : DotNode =
    new DotNode("node_%s".format(dotLegaliseId(id.toString)), List.empty)

  def fromDLabel(id : DLabel) : DotNode =
    new DotNode("node_%s".format(dotLegaliseId(id.toString)), List.empty)

  def record[A](l : A, header : String, body : String)(implicit show : Show[A]) : DotNode =
    (DotNode(show.shows(l)) 
      !! DotAttr.shape(Shape.Record) 
      !! DotAttr.labelWithPorts("{ %s - %s | %s }".format(l.toString, header, Dot.dotEscape(body))))
}

class DotAttr(val key : String, val value : String) {
  override def toString : String = 
    "%s=%s".format(key, value)
}

object RankDir extends Enumeration {
  val TB, BT, LR, RL = Value
}

object Color extends Enumeration {
  val Purple, Red = Value
}

object Shape extends Enumeration {
  val Rectangle, Record = Value
}

object DotAttr {
  def shape(s : Shape.Value) : DotAttr =
    new DotAttr("shape", s.toString.toLowerCase)

  def label(l : String) : DotAttr = 
    new DotAttr("label", "\"%s\"".format(Dot.dotEscape(l)))

  def labelWithPorts(l : String) : DotAttr = 
    new DotAttr("label", "\"%s\"".format(l))

  def rankDir(dir : RankDir.Value) = 
    new DotAttr("rankdir", dir.toString)

  def color(c : Color.Value) = 
    new DotAttr("color", c.toString.toLowerCase)

  def fontColor(c : Color.Value) = 
    new DotAttr("fontcolor", c.toString.toLowerCase)
}

class DotEdge(val source : DotNode, val target : DotNode, val sPort : Option[String], val attrs : List[DotAttr]) {
  def !!(a : DotAttr) : DotEdge = 
    new DotEdge(source, target, sPort, a :: attrs)

  def !!(as : List[DotAttr]) : DotEdge = 
    new DotEdge(source, target, sPort, as ++ attrs)

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
  def apply(source : DotNode, target : DotNode) : DotEdge =
    new DotEdge(source, target, None, List.empty)
}

class DotGraph(val name : String, val blocks : Traversable[DotNode], val edges : Traversable[DotEdge], val attrs : List[DotAttr]) {
  def addEdges(es : Traversable[DotEdge]) : DotGraph = {
    new DotGraph(name, blocks, es ++ edges, attrs)
  }

  override def toString : String = 
    "digraph %s {\n%s\n%s\n%s\n}".format(name, attrs.mkString("\n"), blocks.mkString("\n"), edges.mkString("\n"))
}

object DotGraph {
  def apply(name : String, blocks : Traversable[DotNode], edges : Traversable[DotEdge]) : DotGraph =
    new DotGraph(name, blocks, edges, List.empty)

  def empty() : DotGraph = 
    new DotGraph("", List.empty, List.empty, List.empty)
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
      case _ : IOException =>
        Left("Could not find dot. Perhaps it is not installed?")
    } 
  }
}
