package parscala

/**
 * Label type for (sub)expressions and statements
 */
class SLabel (val l : Int) extends AnyVal

/**
 * Helper object for creating source of [[SLabel]]s
 */
object SLabel {
  /**
   * Creates infinite source of [[SLabel]]s
   */
  def stream : Stream[SLabel] = Stream.from(0) map (new SLabel(_))
}
