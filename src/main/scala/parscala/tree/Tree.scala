package parscala
package tree

import parscala.controlflow.CFGraph

case class Class(val symbol : Symbol, val methods : Set[Method], val fields : Set[Field]) {
  val name : String = symbol.name.toString
  override def toString : String = "class %s".format(name)

  def <+> (c : Class) : Class = {
    if (symbol == c.symbol)
      Class(symbol, methods ++ c.methods, fields ++ c.fields)
    else
      this
  }
}

case class Package(val symbol : Symbol, val classes : Set[Class]) {
  val name : String = symbol.name.toString
  override def toString : String = "package %s".format(name)
  override def equals(o : Any) : Boolean = 
    o match {
      case p : Package => name == p.name
      case _ => false
    }

  def <+> (p : Package) : Package = 
    if (symbol == p.symbol)
      Package(symbol, classes ++ p.classes)
    else
      this
}

case class Method(val symbol : Symbol, val mAst : Option[Tree]) {
  val name : String = symbol.name.toString

  override def toString : String = "method %s".format(name)

  val body : Option[Tree] = {
    mAst match {
      case Some(ast) => {
        import compiler.Quasiquote
        val q"$_ def $_ (...$_) : $_ = $b" = ast
        Some(b)
      }
      case None => None
    }
  }

  val nodes : Option[NodeTree] = body.map(Node.mkNode(_))

  val parent : Class = Class(symbol.owner, Set.empty, Set.empty)
  lazy val cfg : Option[CFGraph] = CFGraph(this)
}

case class Field(val ast : Tree)
