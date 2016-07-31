import java.io.File
import javax.swing.{JFrame,WindowConstants,SwingUtilities}
import java.awt.BorderLayout

import parscala._

object ParScala {
  def mainWindow(title : String) : JFrame = {
    val frame : JFrame = new JFrame(title)
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    frame
  }

  def expandPath(path : String) : String = 
    if (path startsWith "~")
      new File(System.getProperty("user.home"), path.tail).getPath()
    else
      path

  def main(args : Array[String]) : Unit = {
    println("args exists?")
    val pathes : Array[String] = args map expandPath
    val xs : Array[(String, Boolean)] = args zip pathes.map{new File(_).exists()}
    xs foreach {x => println(s"${x._1} - ${x._2}")}
    if (!xs.isEmpty && xs.forall(_._2)) {
      val units : List[CompilationUnit] = parscala.ParScala.analyse(pathes.toList).toList
      println(s"units: ${units.length}")
      val classes : List[Class] = units.map(_.body).map(new Package(_)).flatMap(_.classes).map(new Class(_)).toList
      println(s"classes (${classes.length}): ${classes.mkString(", ")}")
      val methods : List[Method] = classes.flatMap(_.methods)
      println(s"methods: ${methods.mkString(", ")}")
      val mainMethod : Option[Method] = methods.find(_.name == "main")
      //val vars : Option[List[Variable]] = mainMethod.map(_.vars)
      //println(vars)
      //compiler.treeBrowser.browse("?", units)
      mainMethod match {
        case Some(method) => {
          println(CFGPrinter.formatGraph(method.cfg))
          SwingUtilities.invokeLater(new Runnable() {
            def run() : Unit = {
              val w : JFrame = mainWindow(s"Control flow graph of ${method.parent.name}.${method.name}")
              w.add(CFGPrinter.drawGraph(method.cfg), BorderLayout.CENTER)
              w.pack()
              w.setVisible(true)
            }
          });
        }
        case None => ()
      }
    }
  }
}
