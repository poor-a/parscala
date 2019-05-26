package parscala
package tree

import parscala.dot
import parscala.dot.{DotNode, DotGen}
import scala.meta

import org.typelevel.paiges.Doc

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

  def prettyPrint(decl : Decl) : Doc = {
    def prettyArgss(argss : List[List[meta.Term.Param]]) : Doc =
      Doc.fill(Doc.lineOrEmpty, argss.map(args => Doc.str(args.mkString("(", ",", ")"))))

    cata(
        (_l, pats, symbols) => // val
          Doc.spread(List(Doc.text("val"), Doc.text(pats.mkString("(", ",", ")"))))
      , (_l, pats, symbols) => // var
          Doc.spread(List(Doc.text("var"), Doc.text(pats.mkString("(", ",", ")"))))
      , (_l, _symbols, name, argss) => // method
          Doc.text("def") + Doc.space + Doc.str(name) + Doc.lineOrSpace +
          prettyArgss(argss)
      , (_l, _symbols, name, params, _bounds) => // type
          Doc.text("type") + Doc.space + Doc.str(name) + Doc.space + Doc.str(params.mkString("[", ",", "]"))
      , (_l, imports) => // import
          Doc.text("import") + Doc.space + Doc.str(imports)
      , decl
      )
  }

  /** Converts a declaration into a graph.
   */
  def toDot(d : Decl) : dot.DotGraph = {
    val (nodes, edges) : (List[DotNode], List[dot.DotEdge]) = DotGen.exec(toDotGen(d))
    dot.DotGraph("", nodes, edges)
  }

  def toDotGen(decl : Decl) : dot.DotGen.DotGen[dot.DotNode] =
    cata( 
        (l, pats, _symbols) => // value
          DotGen.node(DotNode.record(l, "Val", pats.mkString(", ")))
        , (l, pats, _symbols) => // variable
            DotGen.node(DotNode.record(l, "Var", pats.mkString(", ")))
        , (l, _symbol, name, argss) => { // method
            val args = argss.map(_.map(_.name).mkString("(", ", ", ")")).mkString("")
            DotGen.node(DotNode.record(l, "Method", s"$name$args"))
          }
        , (l, _symbol, name, _params, _bounds) => // type
            DotGen.node(DotNode.record(l, "Type", name.toString))
        , (l, sugared) => // import
            DotGen.node(DotNode.record(l, "Import", sugared.toString))
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
