package parscala

object Control {
  def until[A](p : A => Boolean, f : A => A, x : A) : A = {
    if (p(x)) x else until(p, f, f(x))
  }
}
