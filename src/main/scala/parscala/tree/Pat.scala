package parscala
package tree

sealed abstract class Pat {
  def label : PLabel
}

case class LiteralPat(val l : PLabel, lit : Lit) extends Pat {
  override def label : PLabel = l
}

case class IdentPat(val l : PLabel, val s : Symbol) extends Pat {
  override def label : PLabel = l
}

case class AsPat(val l : PLabel, val s : Symbol, val pat : Pat) extends Pat {
  override def label : PLabel = l
}

case class UnderscorePat(val l : PLabel) extends Pat {
  override def label : PLabel = l
}

object Pat {
  def patCata[A]( lit : (PLabel, Lit) => A
                , id : (PLabel, Symbol) => A
                , as : (PLabel, Symbol, Pat) => A
                , underscore : PLabel => A
                , p : Pat) : A = 
    p match {
      case LiteralPat(label, l) => lit(label, l)
      case IdentPat(label, sym) => id(label, sym)
      case AsPat(label, sym, pat) => as(label, sym, pat)
      case UnderscorePat(label) => underscore(label)
    }

  def identifiers(p : Pat) : List[Symbol] = 
    patCata((_, _) =>        // literal
              List()      
          , (_, sym) =>      // identifier
              List(sym)
          , (_, sym, pat) => // as-pattern
              sym :: identifiers(pat)
          , _ =>             // underscore
              List()
          , p)
}
