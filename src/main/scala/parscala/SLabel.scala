package parscala

import scalaz.{Show, Cord}

/**
 * Label type for (sub)expressions and statements
 */
class SLabel (val l : Int) extends AnyVal {
  override def toString : String = s"SLabel_${l.toString}"

  def toShortString : String = l.toString

  def toInt : Int = l
}

/**
 * Helper object for creating source of [[SLabel]]s
 */
object SLabel {
  /**
   * Creates infinite source of [[SLabel]]s
   */
  def stream : Stream[SLabel] = Stream.from(0) map (new SLabel(_))

  implicit val showInstance : Show[SLabel] = new Show[SLabel] {
    override def show(l : SLabel) : Cord = Cord(l.toString)
  }
}
