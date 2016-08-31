import java.io.{File,PrintWriter}
import java.nio.file.{Paths,Files,Path,StandardOpenOption}

import parscala._
import parscala.tree._

case class Config(val method : String,
             val showCfg : Boolean,
             val showCallGraph : Boolean,
             val dotOutput : String,
             val files : List[String],
             val showHelp : Boolean
)


object ParScala {
  private def dumpDot(path : String, cfg : CFGraph) : Unit = {
    val p : Path = Paths.get(path)
    val out = new PrintWriter(Files.newBufferedWriter(p, StandardOpenOption.CREATE_NEW))
    out.print(CFGPrinter.formatGraph(cfg))
    out.flush()
    out.close()
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
          println("args exists?")
          val pathes : List[String] = c.files map expandPath
          val xs : List[(String, Boolean)] = c.files zip pathes.map{new File(_).exists()}
          xs foreach {x => println("%s - %s".format(x._1, x._2))}
          if (xs.forall(_._2)) {
            val g : ProgramGraph = parscala.ParScala.analyse(pathes)
            if (c.showCallGraph) {
              MainWindow.showCallGraph(g.callGraph._1)
            }
            val classes : Set[Class] = g.packages flatMap (_.classes)
            println(s"classes (${classes.size}): ${classes.mkString(", ")}")
            val methods : Set[Method] = classes.flatMap(_.methods)
            println(s"methods: ${methods.mkString(", ")}")
            val parts : Array[String] = c.method.split('.')
            val oMethod : Option[Method] = methods.find(_.name == parts.last)
            oMethod match {
              case Some(method) => {
                if (c.showCfg) {
                  MainWindow.showCfg(method)
                }
                if (!c.dotOutput.isEmpty) {
                  dumpDot(c.dotOutput, method.cfg)
                }
              }
              case None => ()
            }
          }
        }
      }
    }
  }
}
