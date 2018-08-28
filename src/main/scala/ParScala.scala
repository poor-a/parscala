import parscala._
import parscala.callgraph.{CallGraphBuilder, CallGraphVisualiser}
import parscala.controlflow.{CFGraph, CFGPrinter}
import parscala.df.{UseDefinition, DFGraph}
import parscala.dot.DotGraph
import parscala.file.DirectoryTraverser
import parscala.tree

import scala.collection.JavaConverters

import java.util.stream.{Stream, Collectors}
import java.io.PrintWriter
import java.nio.file.{Path, Paths, Files, StandardOpenOption, FileAlreadyExistsException}

case class Config (
  val method : Option[String],
  val showAst : Boolean,
  val showCfg : Boolean,
  val showCallGraph : Boolean,
  val showDataflowGraph : Boolean,
  val dotOutput : Option[Path],
  val files : List[Path],
  val directories : List[Path],
  val classpath : Option[String],
  val showHelp : Boolean
)

object ParScala {
  private def dumpDot(p : Path, g : dot.DotGraph) : Unit = {
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

  private def mkProgramGraph(scalaSourceFiles : List[Path], classpath : Option[String]) : ProgramGraph = {
    val (pgraph, oErr) : (ProgramGraph, Option[String]) = parscala.ParScala.analyse(scalaSourceFiles, classpath)
    oErr foreach { err => println("ERROR: " + err) }
    pgraph
  }

  private def mkAst(pg : ProgramGraph) : DotGraph = pg.toDot

  private def mkCallGraph(pg : ProgramGraph) : DotGraph =
    CallGraphVisualiser.format(CallGraphBuilder.fullCallGraph(pg))

  private def findMethod(name : String, pg : ProgramGraph) : Option[Either[tree.Decl.Method, tree.Defn.Method]] = {
    val methods : List[Either[tree.Decl.Method, tree.Defn.Method]] = pg.classes.flatMap(_.methods)
    methods.find(_.fold(_.symbols, _.symbols).map(_.fullName) contains name)
  }

  private def onMethod[A](name : String, defn : tree.Defn.Method => A, decl : tree.Decl.Method => A, notFound : => A, pg : ProgramGraph) : A =
    findMethod(name, pg) match {
      case Some(Right(m)) => defn(m)
      case Some(Left(m)) => decl(m)
      case None => notFound
    }

  private def mkCfg(m : tree.Defn.Method, pg : ProgramGraph) : CFGraph =
    CFGraph.fromExpression(m.body, pg)

  private def mkDataflow(m : tree.Defn.Method, cfg : CFGraph) : DotGraph = {
    val usedef : UseDefinition = UseDefinition.fromCFGraph(cfg)
    val dataflow : DFGraph = DFGraph(m.body, usedef)
    tree.Expr.toDot(m.body).addEdges(dataflow.toDotEdges)
  }

  private def noBody(name : String) : String =
    s"The body of $name is not available."

  private def noCfg(name : String) : String =
    noBody(name) + " Could not generate the control flow graph."

  private def noDataflow(name : String) : String =
    noBody(name) + " Could not generate the data flow graph."

  private def noMethod(name : String) : String =
    s"Method $name is not found."

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
            val existingFiles : List[Path] = c.files.filter(Files.isRegularFile(_))
            val (sbtProjects, dirs) : (List[Path], List[Path]) = c.directories.partition(Sbt.isSbtProject)
            val scalaSourceFilesInDirs : List[Path] = JavaConverters.asScalaBuffer(dirs.foldLeft(Stream.empty[Path])((files, dir) => Stream.concat(DirectoryTraverser.getScalaSources(dir), files)).collect(Collectors.toList[Path])).toList
            val classPathSeparator : String = if (scala.util.Properties.isWin) ";" else ":"
            val (projectFiles, projectClassPath) : (List[Path], List[Path]) = scalaz.std.tuple.tuple2Bitraverse.bimap(sbtProjects.map(Sbt.loadProject).unzip)(_.flatten, _.flatten)
            val scalaSourceFiles = existingFiles ++ scalaSourceFilesInDirs ++ projectFiles
            if (scalaSourceFiles.isEmpty) {
              Console.err.println("No Scala files are found.")
            } else {
              val analyse : () => ProgramGraph = () => mkProgramGraph(scalaSourceFiles, c.classpath.map(_ + projectClassPath.mkString(classPathSeparator)))
              val pgraph : ProgramGraph = analyse()
              if (c.showAst)
                MainWindow.showDotWithTitle(mkAst(pgraph), "AST", () => mkAst(analyse()))
              if (c.showCallGraph) {
                MainWindow.showDotWithTitle(mkCallGraph(pgraph), "Call graph", () => mkCallGraph(analyse()))
              }
              if (!c.dotOutput.isEmpty)
                dumpDot(c.dotOutput.get, mkAst(pgraph))
              scalaz.std.option.cata(c.method)(
                  mName => {
                    val oMethod : Option[Either[tree.Decl.Method, tree.Defn.Method]] = findMethod(mName, pgraph)
                    oMethod match {
                      case Some(Right(method)) =>
                        if (c.showCfg || c.showDataflowGraph || !c.dotOutput.isEmpty) {
                          val cfg : CFGraph = mkCfg(method, pgraph)
                          if (c.showCfg) {
                            val refreshCfg : () => DotGraph = () => {
                              val pg = analyse()
                              onMethod(mName, m => CFGPrinter.formatGraph(mkCfg(m, pg)), _ => { println(noCfg(mName)); DotGraph.empty }, { println(noMethod(mName)); DotGraph.empty }, pg)
                            }
                            MainWindow.showDotWithTitle(CFGPrinter.formatGraph(cfg), "Control flow graph of %s".format(method.name), refreshCfg)
                          }
                          if (c.showDataflowGraph) {
                            val refreshDf : () => DotGraph = () => {
                              val pg = analyse()
                              onMethod(mName, m => mkDataflow(m, mkCfg(m, pg)), _ => { println(noDataflow(mName)); DotGraph.empty }, { println(noMethod(mName)); DotGraph.empty }, pg)
                            }
                            MainWindow.showDotWithTitle(mkDataflow(method, cfg), "Data flow graph of %s".format(method.name), refreshDf)
                          }
                          if (!c.dotOutput.isEmpty)
                            dumpDot(c.dotOutput.get, CFGPrinter.formatGraph(cfg))
                        }
                      case Some(Left(method)) =>
                        val what : String = 
                          if (c.showCfg || !c.dotOutput.isEmpty)
                            noCfg(method.name.toString)
                          else if (c.showDataflowGraph)
                            noDataflow(method.name.toString)
                          else
                            noBody(method.name.toString)
                        Console.err.println(what)
                      case None =>
                        println(noMethod(mName))
                    }
                  }
                , if (c.showCfg || c.showDataflowGraph)
                    println("No method name is given. Specify one with -m <name>")
                )
            }
          } 
        }
      }
    }
  }
}
