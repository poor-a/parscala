import javax.swing.{JFrame,WindowConstants,SwingUtilities}
import java.awt.{BorderLayout,Dimension}

import parscala.tree.Method
import parscala.CFGPrinter
import parscala.callgraph.{CallGraph,Dot}

object MainWindow {
  def showCfg(method : Method) : Unit = {
    SwingUtilities.invokeLater(new Runnable() {
      def run() : Unit = {
        val window : JFrame = createWindow("Control flow graph of %s.%s".format(method.parent.name, method.name))
        window.add(CFGPrinter.drawGraph(method.cfg), BorderLayout.CENTER)
        window.setPreferredSize(new Dimension(800, 600))
        window.pack()
        window.setVisible(true)
      }
    });
  }

  def showCallGraph(g : CallGraph) : Unit = {
    SwingUtilities.invokeLater(new Runnable() {
      def run() : Unit = {
        val window : JFrame = createWindow("Call graph")
        val d : Dot = new Dot
        window.add(d.drawGraph(d.format(g)), BorderLayout.CENTER)
        window.setPreferredSize(new Dimension(800, 600))
        window.pack()
        window.setVisible(true)
      }
    });
  }

  private def createWindow(title : String) : JFrame = {
    val frame : JFrame = new JFrame(title)
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    frame
  }
}
