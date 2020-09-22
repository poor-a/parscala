abstract class ASTTest {
  var x, y : Int

  val a, b, c : Int

  var singleton : Int

  var pairA, pairB : Int

  var tripleA, tripleB, tripleC : Int

  private lazy val aConcrete, bConcrete = 2

  protected def f (a1 : Int, a2 : Int)(b1 : String) : Int

  protected final def fConcrete(n : Int) : Int = n + 1
  
  def this(n : Int) = {
    this()
    println("hello")
    ()
  }

}



object ASTTest {
}


trait T { }



class C

package p {
  class D

  private object O
}


case class CC0()



case class CC2(a : Int, b : String) {
}


object CC2 {
  def unapply(c : CC2) : Option[Int] = {
    if (c.a > 0) Some(c.a) else None
  }

  object For {
    for (n <- List(0,1,2,3,4)) println(Math.pow(n,2))

    def foo() {
      case class P(a : Int, b : Int)

      val P(x,y) = P(1,2)
    }
  }
}

class PO {

val n = 5

object Empty { val x = 0 }

}

package object Foo {

  object O


  type U <: AnyRef


  type OO[L] <: AnyRef

  protected type TwoTwo[A,B] <: AnyRef

  protected type T = Int
  type O[X] = List[X]
  type Two[A,B] = (O[A], O[B])
}
