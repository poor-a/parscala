package parscala
package tree

sealed abstract class Lit

case class IntLit(n : Int) extends Lit
case class BooleanLit(b : Boolean) extends Lit
case class CharLit(c : Char) extends Lit
case class StringLit(s : String) extends Lit
case class FloatLit(f : Float) extends Lit
case class DoubleLit(d : Double) extends Lit
case class SymbolLit(sym : Symbol) extends Lit
case class OtherLit(value : Any) extends Lit
