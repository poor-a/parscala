package parscala

/**
 * Label type for patterns
 */
class PLabel (val l : Int) extends AnyVal {
  override def toString : String = l.toString
}

/**
 * Helper object for creating source of [[PLabel]]s
 */
object PLabel {
  /**
   * Creates infinite source of [[PLabel]]s
   */
  def stream : Stream[PLabel] = Stream.from(0) map (new PLabel(_))
}
