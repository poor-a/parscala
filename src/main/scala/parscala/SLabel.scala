package parscala

import scalaz.{Show, Cord}
import scalaz.syntax.ShowSyntax

/**
 * Label type for (sub)expressions and statements
 */
class SLabel (val l : Int) extends AnyVal {
  override def toString : String = l.toString
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
    override def show(l : SLabel) : Cord = Cord.fromStrings(Seq(l.toString))

    override val showSyntax : ShowSyntax[SLabel] = new ShowSyntax[SLabel] {
      override def F : Show[SLabel] = showInstance
    }

    override def shows(l : SLabel) : String = l.toString
  }
    
}
