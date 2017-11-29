package parscala

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
}
