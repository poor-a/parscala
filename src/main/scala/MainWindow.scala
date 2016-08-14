import javax.swing.{JFrame,WindowConstants,SwingUtilities}
import java.awt.{BorderLayout,Dimension}

import parscala._

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

  private def createWindow(title : String) : JFrame = {
    val frame : JFrame = new JFrame(title)
    frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE)
    frame
  }
}
