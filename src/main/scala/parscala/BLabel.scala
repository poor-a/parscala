package parscala

/**
 * Label type for basic blocks
 */
class BLabel (val l : Int) extends AnyVal {
  override def toString : String = l.toString
}

/**
 * Helper object for creating source of [[BLabel]]s
 */
object BLabel {
  /**
   * Creates infinite source of [[BLabel]]s
   */
  def stream : Stream[BLabel] = Stream.from(0) map (new BLabel(_))
}

