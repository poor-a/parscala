import javax.swing.{JFrame,WindowConstants,SwingUtilities}
import java.awt.{BorderLayout,Dimension}

import parscala.tree.Method
import parscala.controlflow.CFGPrinter
import parscala.callgraph.{CallGraph,CallGraphVisualiser}
import parscala.dot.Dot

object MainWindow {
  def showCfg(method : Method) {
    val d : Dot = new Dot
    d.drawGraph(CFGPrinter.formatGraph(method.cfg)) match {
      case Right(image) => {
        showWindow { w => {
          w.setTitle("Control flow graph of %s".format(method.symbol.fullName.toString))
          w.add(image, BorderLayout.CENTER)
        }}
      }
      case Left(err) => {
        Console.err.println(err)
      }
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
