package parscala

/**
 * Label type for basic blocks
 */
class BLabel (val l : Int) extends AnyVal

/**
 * Helper object for creating source of [[SLabel]]s
 */
object BLabel {
  /**
   * Creates infinite source of [[BLabel]]s
   */
  def stream : Stream[BLabel] = Stream.from(0) map (new BLabel(_))
}

