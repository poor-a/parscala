package parscala
package tree

import parscala.dot
import scala.meta

/**
 * Superclass of declarations and definitions.
 */
sealed abstract class Decl {
  def label : DLabel
}

case class Var(val l : DLabel, pats : List[meta.Pat], symbols : Set[Symbol], rhs : Option[Node], sugared : meta.Decl.Var) extends Decl {
  def label : DLabel = l

  override def toString : String = sugared.toString
}

case class Val(val l : DLabel, pats : List[meta.Pat], symbols : Set[Symbol], rhs : Option[Node], sugared : meta.Decl.Val) extends Decl {
  def label : DLabel = l

  override def toString : String = sugared.toString
}

case class Method(val l : DLabel, symbol : Symbol, name : meta.Term.Name, argss : List[List[meta.Term.Param]], body : Option[Node], sugared : meta.Decl.Def) extends Decl {
  def label : DLabel = l

  override def toString : String = symbol.toString
}

case class Class(val l : DLabel, symbol : Symbol, name : String, decls : List[Decl], sugared : meta.Defn.Class) extends Decl {
  def label : DLabel = l

  override def toString : String = symbol.toString

  def filterDecls(pred : Decl => Boolean) : List[Decl] =
    decls filter pred

  def methods : List[Method] = 
    parscala.Control.catSomes(filterDecls(Decl.isMethod).map(Decl.asMethod))
}

case class Object(val l : DLabel,  symbol : Symbol, name : String, decls : List[Decl], sugared : meta.Defn.Object) extends Decl {
  def label : DLabel = l

  override def toString : String = symbol.toString
}

case class PackageObject(val l : DLabel, symbol : Symbol, name : String, decls : List[Decl], sugared : meta.Defn.Object) extends Decl {
  def label : DLabel = l

  override def toString : String = symbol.toString
}

case class Package(val l : DLabel, symbol : Symbol, name : String, decls : List[Decl], sugared : meta.Pkg) extends Decl {
  def label : DLabel = l

  override def toString : String = symbol.toString

  def filterDecls(pred : Decl => Boolean) : List[Decl] =
    decls filter pred

  def classes : List[Class] =
    parscala.Control.catSomes(filterDecls(Decl.isClass).map(Decl.asClass))
}

case class Type(l : DLabel, symbol : Symbol, name : meta.Type.Name, params : List[meta.Type.Param], bounds : meta.Type.Bounds, sugared : meta.Decl.Type) extends Decl {

  override def label : DLabel = l

  override def toString : String = sugared.toString
}

object Decl {
  def cata[A]( fVal : (DLabel, List[meta.Pat], Set[Symbol], Option[Node], meta.Decl.Val) => A
             , fVar : (DLabel, List[meta.Pat], Set[Symbol], Option[Node], meta.Decl.Var) => A
             , fMethod : (DLabel, Symbol, meta.Term.Name, List[List[meta.Term.Param]], Option[Node], meta.Decl.Def) => A
             , fClass : (DLabel, Symbol, String, List[Decl], meta.Defn.Class) => A
             , fObject : (DLabel, Symbol, String, List[Decl], meta.Defn.Object) => A
             , fPObject : (DLabel, Symbol, String, List[Decl], meta.Defn.Object) => A
             , fPackage : (DLabel, Symbol, String, List[Decl], meta.Pkg) => A
             , decl : Decl
             ) : A =
    decl match {
      case Var(l, pats, symbols, rhs, desugared) => fVar(l, pats, symbols, rhs, desugared)
      case Val(l, pats, symbols, rhs, desugared) => fVal(l, pats, symbols, rhs, desugared)
      case Method(l, sym, name, argss, body, desugared) => fMethod(l, sym, name, argss, body, desugared)
      case Class(l, sym, name, decls, desugared) => fClass(l, sym, name, decls, desugared)
      case Object(l, sym, name, decls, desugared) => fObject(l, sym, name, decls, desugared)
      case PackageObject(l, sym, name, decls, desugared) => fPObject(l, sym, name, decls, desugared)
      case Package(l, sym, name, decls, desugared) => fPackage(l, sym, name, decls, desugared)
    }

  /** Converts a declaration into a graph.
   */
  def toDot(d : Decl) : dot.DotGraph = {
    val (_, tree) = toTree(d)
    dot.DotGraph("Program graph", List(), List()) + tree
  }

  /** Helper function of toDot for generating dot nodes and edges
   *  from a declaration.
   *
   *  @returns root and its children with edges of the tree
   */
  private def toTree(decl : Decl) : (dot.DotNode, dot.DotGraph) =
    cata( (l, pats, symbols, mRhs, desugared) => { // value
            val root : dot.DotNode = dot.DotNode(l.toString) !! dot.DotAttr.label(desugared.toString)
            (root, dot.DotGraph("", List(root), List()))
          }
        , (l, pats, symbols, mRhs, desugared) => { // variable
            val root : dot.DotNode = dot.DotNode(l.toString) !! dot.DotAttr.label(desugared.toString)
            (root, dot.DotGraph("", List(root), List()))
          }
        , (l, symbol, name, argss, body, desugared) => { // method
            val root : dot.DotNode = dot.DotNode(l.toString) !! dot.DotAttr.label(symbol.toString)
            (root, dot.DotGraph("", List(root), List()))
          }
        , (l, symbol, name, decls, desugared) => topLevel(l, symbol, decls) // class
        , (l, symbol, name, decls, desugared) => topLevel(l, symbol, decls) // object
        , (l, symbol, name, decls, desugared) => topLevel(l, symbol, decls) // package object
        , (l, symbol, name, decls, desugared) => topLevel(l, symbol, decls) // package
        , decl
        )

  /** Helper function for top-level declarations,
   *  such as class, object, package object and package
   */
  private def topLevel(l : DLabel, symbol : Symbol, decls : List[Decl]) : (dot.DotNode, dot.DotGraph) = {
      val (children, subtrees) : (List[dot.DotNode], List[dot.DotGraph]) = decls.map(toTree).unzip
      val subtree : dot.DotGraph = subtrees.foldLeft(dot.DotGraph.empty(""))(_ + _)
      val current = dot.DotNode(l.toString) !! dot.DotAttr.label(symbol.toString)
      val tree : dot.DotGraph = dot.DotGraph("", current :: children, children.map{child => dot.DotEdge(current, child)}) + subtree
      (current, tree)
    }

  def isClass(d : Decl) : Boolean = {
    val c5False : (Any, Any, Any, Any, Any) => Boolean = Function.const5(false)
    val c6False : (Any, Any, Any, Any, Any, Any) => Boolean = Function.const6(false)
    val c5True : (Any, Any, Any, Any, Any) => Boolean = Function.const5(true)
    Decl.cata(
        c5False // value
      , c5False // variable
      , c6False // method
      , c5True  // class
      , c5False // object
      , c5False // package object
      , c5False // package
      , d
      )
  }

  def isMethod(d : Decl) : Boolean = {
    val c5False : (Any, Any, Any, Any, Any) => Boolean = Function.const5(false)
    val c6True : (Any, Any, Any, Any, Any, Any) => Boolean = Function.const6(true)
    Decl.cata(
        c5False
      , c5False
      , c6True
      , c5False
      , c5False
      , c5False
      , c5False
      , d
      )
  }

  def asClass(d : Decl) : Option[Class] = {
    val c5None : (Any, Any, Any, Any, Any) => Option[Class] = Function.const5(None)
    val c6None : (Any, Any, Any, Any, Any, Any) => Option[Class] = Function.const6(None)
    Decl.cata(
        c5None // value
      , c5None // variable
      , c6None // method
      , (_, _, _, _, _) => Some(d.asInstanceOf[Class]) // class 
      , c5None // object
      , c5None // package object
      , c5None // package
      , d
      )
  }

  def asMethod(d : Decl) : Option[Method] = {
    val c5None : (Any, Any, Any, Any, Any) => Option[Method] = Function.const5(None)
    Decl.cata(
        c5None // value
      , c5None // variable
      , (_, _, _, _, _, _) => Some(d.asInstanceOf[Method]) // method
      , c5None // class 
      , c5None // object
      , c5None // package object
      , c5None // package
      , d
      )
  }
}
