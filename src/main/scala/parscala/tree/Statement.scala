package parscala
package tree

/** Trees that can be used in statement context (terms, definitions). */
trait Statement {
  def label : SLabel
}
