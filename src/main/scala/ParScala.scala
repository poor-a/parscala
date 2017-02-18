import java.io.{File,PrintWriter}
import java.nio.file._

import parscala._
import parscala.tree._
import parscala.controlflow.CFGPrinter
import parscala.df

case class Config(
  val method : Option[String],
  val showCfg : Boolean,
  val showCallGraph : Boolean,
  val dotOutput : Option[String],
  val files : List[String],
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
          println("args exist?")
          val xs : List[(String, Boolean)] = c.files zip c.files.map{new File(_).exists()}
          xs foreach {x => println("%s - %s".format(x._1, x._2))}
          if (xs.forall(_._2)) {
            val g : ProgramGraph = parscala.ParScala.analyse(c.files, c.classpath)
            if (c.showCallGraph) {
              MainWindow.showCallGraph(g.callGraph._1)
            }
            val classes : Set[Class] = g.packages flatMap (_.classes)
            println(s"classes (${classes.size}): ${classes.mkString(", ")}")
            val methods : Set[Method] = g.methods
            println(s"methods: ${methods.mkString(", ")}")
            scalaz.std.option.cata(c.method)(
                m => {
                  val oMethod : Option[Method] = methods.find(_.symbol.fullName == c.method)
                  oMethod match {
                    case Some(method) => {
                      if (c.showCfg) {
                        MainWindow.showCfg(method)
                      }
                      if (!c.dotOutput.isEmpty) {
                        scalaz.std.option.cata(method.cfg)(
                            cfg => dumpDot(c.dotOutput.get, CFGPrinter.formatGraph(cfg))
                          , Console.err.println("The body of %s is not available.".format(c.method))
                        )
                      }
                    }
                    case None =>
                      println("Method %s is not found.".format(c.method))
                  }
                }
              , ()
              )
          }
        }
      }
    }
  }
}
