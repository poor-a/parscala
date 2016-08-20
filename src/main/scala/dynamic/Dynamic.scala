package dynamic

import scala.language.existentials
import scala.reflect.runtime.universe.TypeTag

class Dynamic private[dynamic](
  private[dynamic] val x : A forSome {type A},
  private[dynamic] val tag : TypeTag[_])

object Dynamic {
  def toDynamic[A](x : A)(implicit tag : TypeTag[A]) : Dynamic = new Dynamic(x, tag)
  def fromDynamic[A](d : Dynamic)(implicit tag : TypeTag[A]) : Option[A] = {
    d.x.asInstanceOf[A] match {
      case r if tag == d.tag => Some(r)
      case _ => None
    }
  }
}
