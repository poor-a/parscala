package parscala

/**
 * Auxiliary object which includes helper methods for functions.
 */
object Function {
  def const[A](x : A) : Any => A = _ => x
  def const2[A](x : A) : (Any, Any) => A = (_, _) => x
  def const3[A](x : A) : (Any, Any, Any) => A = (_, _, _) => x
  def const4[A](x : A) : (Any, Any, Any, Any) => A = (_, _, _, _) => x
  def const5[A](x : A) : (Any, Any, Any, Any, Any) => A = (_, _, _, _, _) => x
}
