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

object Decl {
  case class Var(val l : DLabel, pats : List[meta.Pat], symbols : Set[Symbol], sugared : meta.Decl.Var) extends Decl {
    def label : DLabel = l

    override def toString : String = sugared.toString
  }

  case class Val(val l : DLabel, pats : List[meta.Pat], symbols : Set[Symbol], sugared : meta.Decl.Val) extends Decl {
    def label : DLabel = l

    override def toString : String = sugared.toString
  }

  case class Method(val l : DLabel, symbol : Symbol, name : meta.Term.Name, argss : List[List[meta.Term.Param]], sugared : meta.Decl.Def) extends Decl {
    def label : DLabel = l

    override def toString : String = symbol.toString
  }

  case class Type(l : DLabel, symbol : Symbol, name : meta.Type.Name, params : List[meta.Type.Param], bounds : meta.Type.Bounds, sugared : meta.Decl.Type) extends Decl {
    override def label : DLabel = l

    override def toString : String = sugared.toString
  }

  case class Import(l : DLabel, sugared : meta.Import) extends Decl {
    override def label : DLabel = l

    override def toString : String = sugared.toString
  }

  def cata[A]( fVal : (DLabel, List[meta.Pat], Set[Symbol], meta.Decl.Val) => A
             , fVar : (DLabel, List[meta.Pat], Set[Symbol], meta.Decl.Var) => A
             , fMethod : (DLabel, Symbol, meta.Term.Name, List[List[meta.Term.Param]], meta.Decl.Def) => A
             , fType : (DLabel, Symbol, meta.Type.Name, List[meta.Type.Param], meta.Type.Bounds, meta.Decl.Type) => A
             , fImport : (DLabel, meta.Import) => A
             , decl : Decl
             ) : A =
    decl match {
      case Var(l, pats, symbols, desugared) => fVar(l, pats, symbols, desugared)
      case Val(l, pats, symbols, desugared) => fVal(l, pats, symbols, desugared)
      case Method(l, sym, name, argss, desugared) => fMethod(l, sym, name, argss, desugared)
      case Type(l, sym, name, params, bounds, sugared) => fType(l, sym, name, params, bounds, sugared)
      case Import(l, sugared) => fImport(l, sugared)
    }

  def kindCata[A]( val_ : Val => A
                 , var_ : Var => A
                 , method_ : Method => A
                 , type_ : Type => A
                 , import_ : Import => A
                 , d : Decl
                 ) : A =
    d match {
      case v : Val => val_(v)
      case v : Var => var_(v)
      case m : Method => method_(m)
      case t : Type => type_(t)
      case i : Import => import_(i)
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
    cata( (l, pats, symbols, sugared) => { // value
            val root : dot.DotNode = dot.DotNode(l.toString) !! dot.DotAttr.label(sugared.toString)
            (root, dot.DotGraph("", List(root), List()))
          }
        , (l, pats, symbols, sugared) => { // variable
            val root : dot.DotNode = dot.DotNode(l.toString) !! dot.DotAttr.label(sugared.toString)
            (root, dot.DotGraph("", List(root), List()))
          }
        , (l, symbol, name, argss, sugared) => { // method
            val root : dot.DotNode = dot.DotNode(l.toString) !! dot.DotAttr.label(symbol.toString)
            (root, dot.DotGraph("", List(root), List()))
          }
        , (l, symbol, name, params, bounds, sugared) => { // type
            val root : dot.DotNode = dot.DotNode(l.toString) !! dot.DotAttr.label(symbol.toString)
            (root, dot.DotGraph("", List(root), List()))
          }
        , (l, sugared) => { // import
            val root : dot.DotNode = dot.DotNode(l.toString) !! dot.DotAttr.label(sugared.toString)
            (root, dot.DotGraph("", List(root), List()))
          }
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

  def isMethod(d : Decl) : Boolean =
    asMethod(d).nonEmpty

  def asMethod(d : Decl) : Option[Method] = {
    val c2None : (Any, Any) => Option[Method] = Function.const2(None)
    val c4None : (Any, Any, Any, Any) => Option[Method] = Function.const4(None)
    val c6None : (Any, Any, Any, Any, Any, Any) => Option[Method] = Function.const6(None)
    Decl.cata(
        c4None // value
      , c4None // variable
      , (_, _, _, _, _) => Some(d.asInstanceOf[Method]) // method
      , c6None // type
      , c2None // import
      , d
      )
  }
}
