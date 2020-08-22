package parscala
package tree

import org.typelevel.paiges.Doc

object PrettyPrint {
    lazy val lbracket : Doc = Doc.str("[")
    lazy val rbracket : Doc = Doc.str("]")
    lazy val lparen : Doc = Doc.str("(")
    lazy val rparen : Doc = Doc.str(")")
    lazy val lcurly : Doc = Doc.str("{")
    lazy val rcurly : Doc = Doc.str("}")

    def when(b : Boolean, d : Doc) = if (b) d else Doc.empty

    def paren(d : Doc) : Doc = lparen + d + rparen

    def bracket(d : Doc) : Doc = lbracket + d + rbracket

    def curly(d : Doc) : Doc = lcurly + d + rcurly

    def sepBy[A](sep : Doc, l : List[A]) : Doc = if (l.isEmpty) Doc.empty else Doc.intercalate(sep, l.map(Doc.str(_)))

    def eachFollowedBy[A](sep : Doc, l : List[A]) : Doc = {
      val d : Doc = sepBy(sep, l)
      if (l.isEmpty) d else d + sep
    }

    def parenMany[A](sep : Doc, l : List[A]) : Doc = encloseMany2(paren, sep, l)

    def bracketMany1[A](sep : Doc, l : List[A]) : Doc = encloseMany1(bracket, sep, l)

    def encloseMany1[A](parentize : Doc => Doc, sep : Doc, l : List[A]) : Doc =
      l match {
        case List() => Doc.empty
        case _ => parentize(sepBy(sep, l))
      }

    def encloseMany2[A](parentize : Doc => Doc, sep : Doc, l : List[A]) : Doc =
      l match {
        case List() => Doc.empty
        case List(d) => Doc.str(d)
        case _ => parentize(sepBy(sep, l))
      }
}
