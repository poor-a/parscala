package parscala

import scalaz.{Show, Cord}

/**
 * Label type for declarations (packages, classes, methods, etc.).
 */
class DLabel (val l : Int) extends AnyVal {
  override def toString : String = s"DLabel_${l.toString}"

  def toShortString : String = l.toString

  def toInt : Int = l
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
    override def show(l : DLabel) : Cord = Cord(l.toString)
  }
}
