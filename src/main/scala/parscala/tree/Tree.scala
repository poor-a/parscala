package parscala
package tree

import parscala.controlflow.CFGraph

import compiler.TermSymbol

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

  val parent : Class = Class(symbol.owner, Set.empty, Set.empty)
  lazy val cfg : CFGraph = CFGraph(this)
}

case class Field(val ast : Tree)

case class Expression(val ast : Tree) {
  override def toString : String = s"expression $ast"
}

case class Variable (val definition : Tree) {
  assert(Variable.isVariableDef(definition), "not a variable")
  val info : TermSymbol = definition.symbol.asTerm
  val name : String = info.name.toString

  override def toString : String = s"variable $name"
}

object Variable {
  def isVariableDef(ast : Tree) : Boolean = {
    val isTerm : Boolean = ast.symbol != null && ast.symbol.isTerm
    if (isTerm) {
      val symbol : TermSymbol = ast.symbol.asTerm
      symbol.isVar || symbol.isVal
    }
    else {
      false
    }
  }
}
