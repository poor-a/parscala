import parscala._
import parscala.callgraph.CallGraphBuilder
import parscala.controlflow.{CFGraph, CFGPrinter}
import parscala.df.{UseDefinition, DFGraph}
import parscala.file.DirectoryTraverser
import parscala.tree

import scala.collection.JavaConverters

import java.util.stream.{Stream, Collectors}
import java.io.PrintWriter
import java.nio.file.{Path, Paths, Files, StandardOpenOption, FileAlreadyExistsException}

case class Config (
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
        case _ : FileAlreadyExistsException =>
          println("The file \"" + p + "\" already exists!")
    }
  }

  private def expandPath(path : String) : Path = 
    if (path startsWith "~")
      Paths.get(System.getProperty("user.home"), path.tail)
    else
      Paths.get(path)

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
          if (c.files.isEmpty && c.directories.isEmpty)
            Console.err.println("No Scala source is given.")
          else {
            val existingFiles : List[Path] = 
              for (f <- c.files;
                   p = Paths.get(f);
                   if (Files.exists(p)))
              yield p
            val scalaSourceFilesInDirs : Stream[Path] = c.directories.foldLeft(Stream.empty[Path])((files, dir) => Stream.concat(DirectoryTraverser.getScalaSources(dir), files))
            val scalaSourceFiles = existingFiles ++ (JavaConverters.asScalaBuffer(scalaSourceFilesInDirs.collect(Collectors.toList[Path])).toList)
            if (scalaSourceFiles.isEmpty) {
              Console.err.println("No Scala files are found.")
            } else {
              val (pgraph, oErr) : (ProgramGraph, Option[String]) = parscala.ParScala.analyse(scalaSourceFiles, c.classpath)
              oErr foreach { err => println("ERROR: " + err) }
              println("decls: " + pgraph.declarations)
              println("defns: " + pgraph.definitions)
              if (c.showCallGraph) {
                MainWindow.showCallGraph(CallGraphBuilder.fullCallGraph(pgraph))
              }
              val classes : List[tree.Defn.Class] = pgraph.packages flatMap (_.classes)
              println(s"classes (${classes.size}): ${classes.mkString(", ")}")
              val methods : List[Either[tree.Decl.Method, tree.Defn.Method]] = classes flatMap (_.methods)
              println(s"methods: ${methods.mkString(", ")}")
              scalaz.std.option.cata(c.method)(
                  mName => {
                    val oMethod : Option[Either[tree.Decl.Method, tree.Defn.Method]] = 
                      methods.find(m => m.fold(_.symbol, _.symbol).fullName == mName)
                    oMethod match {
                      case Some(Right(method)) => {
                        println("found method")
                        if (c.showCfg || c.showDataflowGraph || !c.dotOutput.isEmpty) {
                          val body : tree.Expr = method.body
                          val cfg = CFGraph.fromExpression(body, pgraph)
                          if (c.showCfg)
                            MainWindow.showDotWithTitle(CFGPrinter.formatGraph(cfg), "Control flow graph of %s".format(method.name))
                          if (c.showDataflowGraph) {
                            val usedef : UseDefinition = UseDefinition.fromCFGraph(cfg)
                            val dataflow : DFGraph = DFGraph(body, usedef)
                            MainWindow.showDotWithTitle(tree.Expr.toDot(body).addEdges(dataflow.toDotEdges), 
                                                        "Data flow graph of %s".format(method.name))
                          }
                          if (!c.dotOutput.isEmpty)
                            dumpDot(c.dotOutput.get, CFGPrinter.formatGraph(cfg))
                        }
                      }
                      case Some(Left(method)) => {
                        val what : String = 
                          if (c.showCfg || !c.dotOutput.isEmpty)
                            "The body of %s is not available, could not generate the control flow graph.".format(method.name)
                          else if (c.showDataflowGraph)
                            "The body of %s is not available, could not generate the data flow graph.".format(method.name)
                          else
                            "The body of %s is not available."
                        Console.err.println(what)
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
}
