package parscala

import scalaz.{Show, Cord}
import scalaz.syntax.ShowSyntax

/**
 * Label type for declarations (packages, classes, methods, etc.).
 */
class DLabel (val l : Int) extends AnyVal {
  override def toString : String = l.toString
}

/**
 * Helper object for creating source of [[DLabel]]s
 */
object DLabel {
  /**
   * Creates infinite source of [[DLabel]]s
   */
  def stream : Stream[DLabel] = Stream.from(0) map (new DLabel(_))

  implicit val showInstance : Show[DLabel] = new Show[DLabel] {
    override def show(l : DLabel) : Cord = Cord.fromStrings(Seq(l.toString))

    override val showSyntax : ShowSyntax[DLabel] = new ShowSyntax[DLabel] {
      override def F : Show[DLabel] = showInstance
    }

    override def shows(l : DLabel) : String = l.toString
  }
}
