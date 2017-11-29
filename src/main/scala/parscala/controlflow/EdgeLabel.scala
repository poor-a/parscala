package parscala.controlflow

object EdgeLabel extends Enumeration {
  type TagType = Value
  val T, F, NoLabel = Value

  def cata[A]( t : A
             , f : A
             , noLabel : A
             , l : EdgeLabel.TagType
             ) : A =
    l match {
      case T => t
      case F => f
      case NoLabel => noLabel
    }
}

  
