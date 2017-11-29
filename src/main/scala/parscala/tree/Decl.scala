package parscala
package tree

/**
 * Superclass of declarations and definitions.
 */
sealed abstract class Decl {
  def label : DLabel
}

case class Var(val l : DLabel, symbol : Symbol, name : String, rhs : Option[Node]) extends Decl {
  def label : DLabel = l

  override def toString : String = symbol.toString
}

case class Val(val l : DLabel, symbol : Symbol, name : String, rhs : Option[Node]) extends Decl {
  def label : DLabel = l

  override def toString : String = symbol.toString
}

case class Method(val l : DLabel, symbol : Symbol, name : String, args : List[List[Pat]], body : Option[Node]) extends Decl {
  def label : DLabel = l

  override def toString : String = symbol.toString
}

case class Class(val l : DLabel, symbol : Symbol, name : String, decls : List[Decl]) extends Decl {
  def label : DLabel = l

  override def toString : String = symbol.toString

  def filterDecls(pred : Decl => Boolean) : List[Decl] =
    decls filter pred

  def methods : List[Method] = 
    parscala.Control.catSomes(filterDecls(Decl.isMethod).map(Decl.asMethod))
}

case class Object(val l : DLabel, symbol : Symbol, name : String, decls : List[Decl]) extends Decl {
  def label : DLabel = l

  override def toString : String = symbol.toString
}

case class PackageObject(val l : DLabel, symbol : Symbol, name : String, decls : List[Decl]) extends Decl {
  def label : DLabel = l

  override def toString : String = symbol.toString
}

case class Package(val l : DLabel, symbol : Symbol, name : String, decls : List[Decl]) extends Decl {
  def label : DLabel = l

  override def toString : String = symbol.toString

  def filterDecls(pred : Decl => Boolean) : List[Decl] =
    decls filter pred

  def classes : List[Class] = 
    parscala.Control.catSomes(filterDecls(Decl.isClass).map(Decl.asClass))
}

object Decl {
  def cata[A](fVar : (DLabel, Symbol, String, Option[Node]) => A
             ,fVal : (DLabel, Symbol, String, Option[Node]) => A
             ,fMethod : (DLabel, Symbol, String, List[List[Pat]], Option[Node]) => A
             ,fClass : (DLabel, Symbol, String, List[Decl]) => A
             ,fObject : (DLabel, Symbol, String, List[Decl]) => A
             ,fPObject : (DLabel, Symbol, String, List[Decl]) => A
             ,fPackage : (DLabel, Symbol, String, List[Decl]) => A
             ,decl : Decl
             ) : A =
    decl match {
      case Var(l, s, name, rhs) => fVar(l, s, name, rhs)
      case Val(l, s, name, rhs) => fVal(l, s, name, rhs)
      case Method(l, s, name, argss, body) => fMethod(l, s, name, argss, body)
      case Class(l, s, name, decls) => fClass(l, s, name, decls)
      case Object(l, s, name, decls) => fObject(l, s, name, decls)
      case PackageObject(l, s, name, decls) => fPObject(l, s, name, decls)
      case Package(l, s, name, decls) => fPackage(l, s, name, decls)
    }

  def isClass(d : Decl) : Boolean = {
    val c4False : (Any, Any, Any, Any) => Boolean = Function.const4(false)
    val c5False : (Any, Any, Any, Any, Any) => Boolean = Function.const5(false)
    val c4True : (Any, Any, Any, Any) => Boolean = Function.const4(true)
    Decl.cata(
        c4False
      , c4False
      , c5False
      , c4True
      , c4False
      , c4False
      , c4False
      , d
      )
  }

  def isMethod(d : Decl) : Boolean = {
    val c4False : (Any, Any, Any, Any) => Boolean = Function.const4(false)
    val c5True : (Any, Any, Any, Any, Any) => Boolean = Function.const5(true)
    Decl.cata(
        c4False
      , c4False
      , c5True
      , c4False
      , c4False
      , c4False
      , c4False
      , d
      )
  }

  def asClass(d : Decl) : Option[Class] = {
    val c4None : (Any, Any, Any, Any) => Option[Class] = Function.const4(None)
    val c5None : (Any, Any, Any, Any, Any) => Option[Class] = Function.const5(None)
    Decl.cata(
        c4None // variable
      , c4None // value
      , c5None
      , (_, _, _, _) => Some(d.asInstanceOf[Class]) // class 
      , c4None // object
      , c4None // package object
      , c4None // package
      , d
      )
  }

  def asMethod(d : Decl) : Option[Method] = {
    val c4None : (Any, Any, Any, Any) => Option[Method] = Function.const4(None)
    Decl.cata(
        c4None // variable
      , c4None // value
      , (_, _, _, _, _) => Some(d.asInstanceOf[Method]) // method
      , c4None // class 
      , c4None // object
      , c4None // package object
      , c4None // package
      , d
      )
  }
}
