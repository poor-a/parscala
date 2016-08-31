import javax.swing.{JFrame,WindowConstants,SwingUtilities}
import java.awt.{BorderLayout,Dimension}

import parscala.tree.Method
import parscala.CFGPrinter
import parscala.callgraph.{CallGraph,CallGraphVisualiser}
import parscala.dot.Dot

object MainWindow {
  def showCfg(method : Method) : Unit = {
    val d : Dot = new Dot
    d.drawGraph(CFGPrinter.formatGraph(method.cfg)) match {
      case Right(image) => {
        SwingUtilities.invokeLater(new Runnable() {
          def run() : Unit = {
            val window : JFrame = createWindow("Control flow graph of %s".format(method.symbol.fullName.toString))
            window.add(image, BorderLayout.CENTER)
            window.pack()
            window.setVisible(true)
         }
        })
      }
      case Left(err) => {
        Console.err.println(err)
      }
    }
  }

  def showCallGraph(g : CallGraph) : Unit = {
    val d : Dot = new Dot
    d.drawGraph(CallGraphVisualiser.format(g)) match {
      case Right(image) => 
        SwingUtilities.invokeLater(new Runnable() {
          def run() : Unit = {
            val window : JFrame = createWindow("Call graph")
            window.add(image, BorderLayout.CENTER)
            window.pack()
            window.setVisible(true)
          }
        })
      case Left(err) =>
        Console.err.println(err)
    }

  }

  private def createWindow(title : String) : JFrame = {
    val frame : JFrame = new JFrame(title)
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    frame.setPreferredSize(new Dimension(800, 600))
    frame
  }
}
