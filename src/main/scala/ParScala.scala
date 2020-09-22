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
  val checkMapLike : Boolean,
  val removeUnusedVariables : Boolean,
  val files : List[Path],
  val directories : List[Path],
  val classpath : Option[String],
  val prettyPrint : Boolean,
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

  private def expandPath(path : Path) : Path = 
    if (path startsWith "~")
      Paths.get(util.Properties.userHome).resolve(path.subpath(1, path.getNameCount))
    else
      path

  private def mkProgramGraph(scalaSourceFiles : List[Path], classpath : Option[String]) : Option[ProgramGraph] = {
    parscala.ParScala.analyse(scalaSourceFiles, classpath) match {
      case Left(err) =>
        Console.err.println("ERROR: " + err)
        None
      case Right((pgraph, warnings @ _)) =>
        Some(pgraph)
    }
  }

  private def mkAst(pg : ProgramGraph) : DotGraph = pg.toDot

  private def mkCallGraph(pg : ProgramGraph) : DotGraph =
    CallGraphVisualiser.format(CallGraphBuilder.fullCallGraph(pg))

  private def findMethod(name : String, pg : ProgramGraph) : Option[Either[tree.Decl.Method, tree.Defn.Method]] = {
    val methods : List[Either[tree.Decl.Method, tree.Defn.Method]] =
      pg.classes.flatMap(_.methods) ++ pg.objects.flatMap(_.methods)
    methods.find(_.fold(_.symbols, _.symbols).map(_.fullName) contains name)
  }

  private def onMethod[A](name : String, defn : tree.Defn.Method => A, decl : tree.Decl.Method => A, notFound : => A, pg : ProgramGraph) : A =
    findMethod(name, pg) match {
      case Some(Right(m)) => defn(m)
      case Some(Left(m)) => decl(m)
      case None => notFound
    }

  private def mkCfg(m : tree.Defn.Method, pg : ProgramGraph) : CFGraph =
    CFGraph.fromMethod(m, pg)

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

  private def noMapLike(name : String) : String =
    noBody(name) + " Could not check whether the method is like map."

  private def noMethod(name : String) : String =
    s"Method $name is not found."

  private def rewriteFileOf(m : tree.Defn.Method, pgraph : ProgramGraph) : Unit = {
    def path(s : Symbol) : String = s.pos.source.path
    m.symbols match {
      case s :: _ =>
        val p : String = path(s)
        val topLevels : List[Either[tree.Decl, tree.Defn]] = pgraph.topLevels.filter(_.fold(decl => tree.Decl.symbols(decl).exists(s => path(s) == p), defn => tree.Defn.symbols(defn).exists(s => path(s) == p)))
        val code : List[String] = topLevels.map(_.fold(tree.Decl.prettyPrint(_).render(80), tree.Defn.prettyPrint(_).render(80)))
        if (!code.isEmpty) {
          val f = new java.io.PrintWriter(new java.io.BufferedWriter(new java.io.FileWriter(p)))
          f.println(code.mkString("\n\n"))
          f.flush()
          f.close()
        }
      case _ => ()
    }
  }

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
            val existingFiles : List[Path] = c.files.map(expandPath).filter(Files.isRegularFile(_))
            val (sbtProjects, dirs) : (List[Path], List[Path]) = c.directories.partition(Sbt.isSbtProject)
            val scalaSourceFilesInDirs : List[Path] = JavaConverters.asScalaBuffer(dirs.foldLeft(Stream.empty[Path])((files, dir) => Stream.concat(DirectoryTraverser.getScalaSources(dir), files)).collect(Collectors.toList[Path])).toList
            val classPathSeparator : String = if (scala.util.Properties.isWin) ";" else ":"
            val (projectFiles, projectClassPath) : (List[Path], List[Path]) = scalaz.std.tuple.tuple2Bitraverse.bimap(sbtProjects.map(Sbt.loadProject).unzip)(_.flatten, _.flatten)
            val scalaSourceFiles = existingFiles ++ scalaSourceFilesInDirs ++ projectFiles
            if (scalaSourceFiles.isEmpty) {
              Console.err.println("No Scala files are found.")
            } else {
              val fullClassPath : Option[String] = c.classpath.map(_ + projectClassPath.mkString(classPathSeparator))
              val analyse : () => Option[ProgramGraph] = () => mkProgramGraph(scalaSourceFiles, fullClassPath)
              analyse() match {
                case Some(pgraph) =>
                  if (c.showAst)
                    MainWindow.showDotWithTitle(mkAst(pgraph), "AST", () => analyse().map(mkAst(_)))
                  if (c.showCallGraph)
                    MainWindow.showDotWithTitle(mkCallGraph(pgraph), "Call graph", () => analyse().map(mkCallGraph(_)))
                  if (c.prettyPrint)
                    pgraph.topLevels.foreach(d => println(d.fold(tree.Decl.prettyPrint, tree.Defn.prettyPrint).render(100)))
                  scalaz.std.option.cata(c.method) (
                    mName => {
                      val oMethod : Option[Either[tree.Decl.Method, tree.Defn.Method]] = findMethod(mName, pgraph)
                      oMethod match {
                        case Some(Right(method)) =>
                          println(s"$mName refers to a method definition.")
                          val pgraphTransformed : ProgramGraph =
                            if (c.removeUnusedVariables) {
                              val pg : ProgramGraph = parscala.transformation.RemoveUnusedVariables.in(method, pgraph)
                              rewriteFileOf(method, pg)
                              pg
                            } else
                              pgraph
                            if (c.showCfg || c.showDataflowGraph || !c.dotOutput.isEmpty || c.checkMapLike) {
                              val cfg : CFGraph = mkCfg(method, pgraphTransformed)
                              if (c.showCfg) {
                                val refreshCfg : () => Option[DotGraph] = () => {
                                  analyse() match {
                                    case Some(pgraph) =>
                                      onMethod(mName, m => Some(CFGPrinter.formatGraph(mkCfg(m, pgraph))), _ => { println(noCfg(mName)); None }, { println(noMethod(mName)); None }, pgraph)
                                    case None =>
                                      None
                                  }
                                }
                                MainWindow.showDotWithTitle(CFGPrinter.formatGraph(cfg), "Control flow graph of %s".format(method.name), refreshCfg)
                              }
                              if (c.showDataflowGraph) {
                                val refreshDf : () => Option[DotGraph] = () => {
                                  analyse() match {
                                    case Some(pgraph) =>
                                      onMethod(mName, m => Some(mkDataflow(m, mkCfg(m, pgraph))), _ => { println(noDataflow(mName)); None }, { println(noMethod(mName)); None }, pgraph)
                                    case None =>
                                      None
                                  }
                                }
                                MainWindow.showDotWithTitle(mkDataflow(method, cfg), "Data flow graph of %s".format(method.name), refreshDf)
                              }
                              if (!c.dotOutput.isEmpty)
                                dumpDot(c.dotOutput.get, CFGPrinter.formatGraph(cfg))
                              if (c.checkMapLike)
                                if (MapLike.isMapLike(method.l,cfg))
                                  println(s"Yes, $mName is a map-like function.")
                                else
                                  println(s"No, $mName is not a map-like function.")
                            }
                        case Some(Left(method)) =>
                          println(s"$mName refers to an abstract method declaration.")
                          if (c.showCfg)
                            Console.err.println(noCfg(method.name.toString))
                          else if (c.showDataflowGraph)
                            Console.err.println(noDataflow(method.name.toString))
                          else if (c.checkMapLike)
                            Console.err.println(noMapLike(method.name.toString))
                        case None =>
                          println(noMethod(mName))
                      }
                    }
                    , if (c.showCfg || c.showDataflowGraph || c.checkMapLike)
                      println("No method name is given. Specify one with -m <name>")
                  )
                case None =>
                  ()
              }
            }
          } 
        }
      }
    }
  }
}
