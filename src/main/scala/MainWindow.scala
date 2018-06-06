import javax.{swing => sw}
import java.awt.{BorderLayout,Dimension}
import java.awt.event.ActionEvent

import parscala.controlflow.{CFGraph, CFGPrinter}
import parscala.callgraph.{CallGraph,CallGraphVisualiser}
import parscala.dot.{Dot, DotGraph}

object MainWindow {
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

  def showDotWithTitle(g : DotGraph, title : String, update : () => DotGraph) {
    val d : Dot = new Dot

    def putImage(image : sw.JComponent, w : sw.JFrame) : Unit = {
      w.add(image, BorderLayout.CENTER)
      setKeyShortcut(image, "refresh", 'r'){
        d.drawGraph(update()) match {
          case Right(image_) =>
            w.remove(image)
            putImage(image_, w)
            w.revalidate()
            w.repaint()
          case Left(err) =>
            Console.err.println(err)
        }  
      }
    }

    d.drawGraph(g) match {
      case Right(image) =>
        showWindow { w => {
          w.setTitle(title)
          putImage(image, w)
        }}
      case Left(err) =>
        Console.err.println(err)
    }
  }

  def showDot(g : DotGraph, update : () => DotGraph) {
    showDotWithTitle(g, "Dot graph", update)
  }

  private def showWindow(setUp : sw.JFrame => Unit) {
    sw.SwingUtilities.invokeLater(new Runnable() {
      def run() : Unit = {
        val window : sw.JFrame = createWindow()
        setUp(window)
        setKeyShortcut(window.getRootPane(), "quit", 'q')(window.dispose())
        window.pack()
        window.setVisible(true)
      }
    })
  }

  private def setKeyShortcut(c : sw.JComponent, actionName : String, key : Char)(action : => Unit) = {
    c.getInputMap(sw.JComponent.WHEN_IN_FOCUSED_WINDOW).put(sw.KeyStroke.getKeyStroke(key), actionName)
    c.getActionMap().put(actionName, new sw.AbstractAction {
      override def actionPerformed(e : ActionEvent) : Unit = {
        action
      }
    })
  }

  private def createWindow() : sw.JFrame = {
    val frame : sw.JFrame = new sw.JFrame
    frame.setDefaultCloseOperation(sw.WindowConstants.DISPOSE_ON_CLOSE)
    frame.setPreferredSize(new Dimension(800, 600))
    frame
  }
}
