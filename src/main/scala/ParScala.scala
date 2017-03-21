import parscala._
import parscala.file.DirectoryTraverser
import parscala.tree._
import parscala.controlflow.{CFGraph, CFGPrinter}
import parscala.df.UseDefinition
import parscala.dot.DotGraph

import scala.collection.JavaConverters

import java.util.stream.{Stream, Collectors}
import java.io.{File,PrintWriter}
import java.nio.file._

case class Config(
  val method : Option[String],
  val showCfg : Boolean,
  val showCallGraph : Boolean,
  val showDataflowGraph : Boolean,
  val dotOutput : Option[String],
  val files : List[String],
  val directories : List[String],
  val classpath : Option[String],
  val showHelp : Boolean
)

object ParScala {
  private def dumpDot(path : String, g : dot.DotGraph) : Unit = {
    val p : Path = Paths.get(path)
    try {
      val out = new PrintWriter(Files.newBufferedWriter(p, StandardOpenOption.CREATE_NEW))
      out.print(g)
      out.flush()
      out.close()
    } catch {
        case e : FileAlreadyExistsException =>
          println("The file \"" + p + "\" already exists!")
    }
  }

  private def expandPath(path : String) : String = 
    if (path startsWith "~")
      new File(System.getProperty("user.home"), path.tail).getPath()
    else
      path

  def main(args : Array[String]) : Unit = {
    val cli = new Cli
    cli.parse(args) match {
      case Left(err) => {
        println(err)
        cli.printHelp()
      }
      case Right(c) => {
        if (c.showHelp)
          cli.printHelp()
        else {
          val existingFiles : List[String] = c.files filter {new File(_).exists()}
          val scalaSourceFilesInDirs : Stream[String] = c.directories.foldLeft(Stream.empty[String])((files, dir) => Stream.concat(DirectoryTraverser.getScalaSources(dir), files))
          val scalaSourceFiles = (JavaConverters.asScalaBuffer(scalaSourceFilesInDirs.collect(Collectors.toList[String])).toList) ++ existingFiles
          if (c.files.isEmpty && c.directories.isEmpty)
            Console.err.println("No Scala source is given.")
          if (!scalaSourceFiles.isEmpty) {
            val g : ProgramGraph = parscala.ParScala.analyse(scalaSourceFiles, c.classpath)
            if (c.showCallGraph) {
              MainWindow.showCallGraph(g.callGraph._1)
            }
            val classes : Set[Class] = g.packages flatMap (_.classes)
            println(s"classes (${classes.size}): ${classes.mkString(", ")}")
            val methods : Set[Method] = g.methods
            println(s"methods: ${methods.mkString(", ")}")
            scalaz.std.option.cata(c.method)(
                mName => {
                  val oMethod : Option[Method] = methods.find(_.name == mName)
                  oMethod match {
                    case Some(method) => {
                      if (c.showCfg) {
                        MainWindow.showCfg(method)
                      }
                      if (c.showDataflowGraph) {
                        val bodyAndCfg : Option[(Tree, CFGraph)] = for (body <- method.body; cfg <- method.cfg) yield (body, cfg)
                        scalaz.std.option.cata(bodyAndCfg)(
                          {  case (body, cfg) =>
                              val ast : NodeTree = Node.fromTree(body)
                              val usedef : UseDefinition = UseDefinition.fromCFGraph(cfg)
                              val dataflow : DotGraph = Node.toDot(ast.root).addEdges(usedef.toDotEdges)
                              MainWindow.showDotWithTitle(dataflow, "Data flow graph of %s".format(method.name))
                          }
                          , Console.err.println("The body of %s is not available, could not generate the data flow graph.".format(method.name))
                          )
                      }
                      if (!c.dotOutput.isEmpty) {
                        scalaz.std.option.cata(method.cfg)(
                            cfg => dumpDot(c.dotOutput.get, CFGPrinter.formatGraph(cfg))
                          , Console.err.println("The body of %s is not available.".format(method.name))
                        )
                      }
                    }
                    case None =>
                      println("Method %s is not found.".format(mName))
                  }
                }
              , Console.err.println("No method is specified. Try using -m")
              )
          } 
        }
      }
    }
  }
}
