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
  case class Var(val l : DLabel, pats : List[meta.Pat], symbols : List[Symbol]) extends Decl {
    def label : DLabel = l

    override def toString : String = pats.toString
  }

  case class Val(val l : DLabel, pats : List[meta.Pat], symbols : List[Symbol]) extends Decl {
    def label : DLabel = l

    override def toString : String = pats.toString
  }

  case class Method(val l : DLabel, symbols : List[Symbol], name : meta.Term.Name, argss : List[List[meta.Term.Param]]) extends Decl {
    def label : DLabel = l

    override def toString : String = name.toString
  }

  case class Type(l : DLabel, symbols : List[Symbol], name : meta.Type.Name, params : List[meta.Type.Param], bounds : meta.Type.Bounds) extends Decl {
    override def label : DLabel = l

    override def toString : String = name.toString
  }

  case class Import(l : DLabel, sugared : meta.Import) extends Decl {
    override def label : DLabel = l

    override def toString : String = sugared.toString
  }

  def cata[A]( fVal : (DLabel, List[meta.Pat], List[Symbol]) => A
             , fVar : (DLabel, List[meta.Pat], List[Symbol]) => A
             , fMethod : (DLabel, List[Symbol], meta.Term.Name, List[List[meta.Term.Param]]) => A
             , fType : (DLabel, List[Symbol], meta.Type.Name, List[meta.Type.Param], meta.Type.Bounds) => A
             , fImport : (DLabel, meta.Import) => A
             , decl : Decl
             ) : A =
    decl match {
      case Var(l, pats, symbols) => fVar(l, pats, symbols)
      case Val(l, pats, symbols) => fVal(l, pats, symbols)
      case Method(l, sym, name, argss) => fMethod(l, sym, name, argss)
      case Type(l, sym, name, params, bounds) => fType(l, sym, name, params, bounds)
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
    cata( (l, _pats, _symbols) => { // value
            val root : dot.DotNode = dot.DotNode(l.toString) !! dot.DotAttr.label(decl.toString)
            (root, dot.DotGraph("", List(root), List()))
          }
        , (l, _pats, _symbols) => { // variable
            val root : dot.DotNode = dot.DotNode(l.toString) !! dot.DotAttr.label(decl.toString)
            (root, dot.DotGraph("", List(root), List()))
          }
        , (l, symbol, _name, _argss) => { // method
            val root : dot.DotNode = dot.DotNode(l.toString) !! dot.DotAttr.label(decl.toString)
            (root, dot.DotGraph("", List(root), List()))
          }
        , (l, symbol, _name, _params, _bounds) => { // type
            val root : dot.DotNode = dot.DotNode(l.toString) !! dot.DotAttr.label(decl.toString)
            (root, dot.DotGraph("", List(root), List()))
          }
        , (l, sugared) => { // import
            val root : dot.DotNode = dot.DotNode(l.toString) !! dot.DotAttr.label(decl.toString)
            (root, dot.DotGraph("", List(root), List()))
          }
        , decl
        )

  def isTopLevel(d : Decl) : Boolean = {
    val cTrue : (Any) => Boolean = Function.const(true)
    val cFalse : (Any) => Boolean = Function.const(false)
    kindCata(
        cFalse // val
      , cFalse // var
      , cFalse // method
      , cFalse // type
      , cTrue  // import
      , d
      )
  }

  def isMethod(d : Decl) : Boolean =
    asMethod(d).nonEmpty

  def asMethod(d : Decl) : Option[Method] = {
    val cNone : (Any) => Option[Method] = Function.const(None)
    Decl.kindCata(
        cNone   // value
      , cNone   // variable
      , Some(_) // method
      , cNone   // type
      , cNone   // import
      , d
      )
  }
}
