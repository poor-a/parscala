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
  case class Var(val l : DLabel, pats : List[meta.Pat], symbols : Set[Symbol], sugared : meta.Decl.Var, desugared : List[scalac.ValDef], gettersSetters : List[Tree]) extends Decl {
    def label : DLabel = l

    override def toString : String = sugared.toString
  }

  case class Val(val l : DLabel, pats : List[meta.Pat], symbols : Set[Symbol], sugared : meta.Decl.Val, desugared : List[scalac.ValDef], gettersSetters : List[Tree]) extends Decl {
    def label : DLabel = l

    override def toString : String = sugared.toString
  }

  case class Method(val l : DLabel, symbol : Symbol, name : meta.Term.Name, argss : List[List[meta.Term.Param]], sugared : meta.Decl.Def, desugared : scalac.DefDef) extends Decl {
    def label : DLabel = l

    override def toString : String = symbol.toString
  }

  case class Type(l : DLabel, symbol : Symbol, name : meta.Type.Name, params : List[meta.Type.Param], bounds : meta.Type.Bounds, sugared : meta.Decl.Type, desugared : scalac.TypeDef) extends Decl {
    override def label : DLabel = l

    override def toString : String = sugared.toString
  }

  case class Import(l : DLabel, sugared : meta.Import, desugared : scalac.Import) extends Decl {
    override def label : DLabel = l

    override def toString : String = sugared.toString
  }

  def cata[A]( fVal : (DLabel, List[meta.Pat], Set[Symbol], meta.Decl.Val, List[scalac.ValDef], List[Tree]) => A
             , fVar : (DLabel, List[meta.Pat], Set[Symbol], meta.Decl.Var, List[scalac.ValDef], List[Tree]) => A
             , fMethod : (DLabel, Symbol, meta.Term.Name, List[List[meta.Term.Param]], meta.Decl.Def, scalac.DefDef) => A
             , fType : (DLabel, Symbol, meta.Type.Name, List[meta.Type.Param], meta.Type.Bounds, meta.Decl.Type, scalac.TypeDef) => A
             , fImport : (DLabel, meta.Import, scalac.Import) => A
             , decl : Decl
             ) : A =
    decl match {
      case Var(l, pats, symbols, sugared, desugared, gettersSetters) => fVar(l, pats, symbols, sugared, desugared, gettersSetters)
      case Val(l, pats, symbols, sugared, desugared, gettersSetters) => fVal(l, pats, symbols, sugared, desugared, gettersSetters)
      case Method(l, sym, name, argss, sugared, desugared) => fMethod(l, sym, name, argss, sugared, desugared)
      case Type(l, sym, name, params, bounds, sugared, desugared) => fType(l, sym, name, params, bounds, sugared, desugared)
      case Import(l, sugared, desugared) => fImport(l, sugared, desugared)
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
    cata( (l, _pats, _symbols, sugared, _desugared, _gettersSetters) => { // value
            val root : dot.DotNode = dot.DotNode(l.toString) !! dot.DotAttr.label(sugared.toString)
            (root, dot.DotGraph("", List(root), List()))
          }
        , (l, _pats, _symbols, sugared, _desugared, _gettersSetters) => { // variable
            val root : dot.DotNode = dot.DotNode(l.toString) !! dot.DotAttr.label(sugared.toString)
            (root, dot.DotGraph("", List(root), List()))
          }
        , (l, symbol, _name, _argss, _sugared, _desugared) => { // method
            val root : dot.DotNode = dot.DotNode(l.toString) !! dot.DotAttr.label(symbol.toString)
            (root, dot.DotGraph("", List(root), List()))
          }
        , (l, symbol, _name, _params, _bounds, _sugared, _desugared) => { // type
            val root : dot.DotNode = dot.DotNode(l.toString) !! dot.DotAttr.label(symbol.toString)
            (root, dot.DotGraph("", List(root), List()))
          }
        , (l, sugared, _desugared) => { // import
            val root : dot.DotNode = dot.DotNode(l.toString) !! dot.DotAttr.label(sugared.toString)
            (root, dot.DotGraph("", List(root), List()))
          }
        , decl
        )

  def isMethod(d : Decl) : Boolean =
    asMethod(d).nonEmpty

  def asMethod(d : Decl) : Option[Method] = {
    val c3None : (Any, Any, Any) => Option[Method] = Function.const3(None)
    val c6None : (Any, Any, Any, Any, Any, Any) => Option[Method] = Function.const6(None)
    val c7None : (Any, Any, Any, Any, Any, Any, Any) => Option[Method] = Function.const7(None)
    Decl.cata(
        c6None // value
      , c6None // variable
      , (_, _, _, _, _, _) => Some(d.asInstanceOf[Method]) // method
      , c7None // type
      , c3None // import
      , d
      )
  }
}
