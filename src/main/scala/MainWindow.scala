import javax.swing.{JFrame,WindowConstants,SwingUtilities}
import java.awt.{BorderLayout,Dimension}

import parscala.tree.Method
import parscala.controlflow.CFGPrinter
import parscala.callgraph.{CallGraph,CallGraphVisualiser}
import parscala.dot.{Dot, DotGraph}

object MainWindow {
  def showCfg(method : Method) {
    val d : Dot = new Dot
    val name : String = method.symbol.fullName.toString
    method.cfg.map(cfg => d.drawGraph(CFGPrinter.formatGraph(cfg))) match {
      case Some(Right(image)) => {
        showWindow { w => {
          w.setTitle("Control flow graph of %s".format(name))
          w.add(image, BorderLayout.CENTER)
        }}
      }
      case Some(Left(err)) => {
        Console.err.println(err)
      }
      case None =>
        Console.err.println("The body of %s is not available.".format(name))
    }
  }

  def showCallGraph(g : CallGraph) {
    val d : Dot = new Dot
    d.drawGraph(CallGraphVisualiser.format(g)) match {
      case Right(image) => 
        showWindow { w => {
          w.setTitle("Call graph")
          w.add(image, BorderLayout.CENTER)
        }}
      case Left(err) =>
        Console.err.println(err)
    }
  }

  def showDot(g : DotGraph) {
    println(g.toString)
    val d : Dot = new Dot
    d.drawGraph(g) match {
      case Right(image) => 
        showWindow { w => {
          w.setTitle("Dot graph")
          w.add(image, BorderLayout.CENTER)
        }}
      case Left(err) =>
        Console.err.println(err)
    }
  }

  private def showWindow(setUp : JFrame => Unit) {
    SwingUtilities.invokeLater(new Runnable() {
      def run() : Unit = {
        val window : JFrame = createWindow()
        setUp(window)
        window.pack()
        window.setVisible(true)
      }
    })
  }

  private def createWindow() : JFrame = {
    val frame : JFrame = new JFrame
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    frame.setPreferredSize(new Dimension(800, 600))
    frame
  }
}
