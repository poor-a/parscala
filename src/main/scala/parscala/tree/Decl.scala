package parscala
package tree

import parscala.dot
import parscala.dot.{DotNode, DotGen}

import org.typelevel.paiges.Doc

/**
 * Superclass of declarations and definitions.
 */
sealed abstract class Decl[IdentInfo, SemanticInfo] {
  def label : DLabel

  def cata[A]( fVal : (DLabel, List[meta.Mod], List[meta.Pat], List[IdentInfo], meta.Type) => A
             , fVar : (DLabel, List[meta.Mod], List[meta.Pat], List[IdentInfo], meta.Type) => A
             , fMethod : (DLabel, List[IdentInfo], List[meta.Mod], meta.Term.Name, List[meta.Type.Param], List[List[meta.Term.Param]], meta.Type) => A
             , fType : (DLabel, List[IdentInfo], List[meta.Mod], meta.Type.Name, List[meta.Type.Param], meta.Type.Bounds) => A
             , fImport : (DLabel, meta.Import) => A
             ) : A =
    this match {
      case Decl.Val(l, mods, pats, symbols, declType) => fVal(l, mods, pats, symbols, declType)
      case Decl.Var(l, mods, pats, symbols, declType) => fVar(l, mods, pats, symbols, declType)
      case Decl.Method(l, sym, mods, name, typeArgs, argss, declType) => fMethod(l, sym, mods, name, typeArgs, argss, declType)
      case Decl.Type(l, sym, mods, name, params, bounds) => fType(l, sym, mods, name, params, bounds)
      case Decl.Import(l, sugared) => fImport(l, sugared)
    }

  def kindCata[A]( val_ : Decl.Val[IdentInfo, SemanticInfo] => A
                 , var_ : Decl.Var[IdentInfo, SemanticInfo] => A
                 , method_ : Decl.Method[IdentInfo, SemanticInfo] => A
                 , type_ : Decl.Type[IdentInfo, SemanticInfo] => A
                 , import_ : Decl.Import[IdentInfo, SemanticInfo] => A
                 ) : A =
    this match {
      case v : Decl.Val[IdentInfo, SemanticInfo] => val_(v)
      case v : Decl.Var[IdentInfo, SemanticInfo] => var_(v)
      case m : Decl.Method[IdentInfo, SemanticInfo] => method_(m)
      case t : Decl.Type[IdentInfo, SemanticInfo] => type_(t)
      case i : Decl.Import[IdentInfo, SemanticInfo] => import_(i)
    }

  def isTopLevel : Boolean = {
    val cTrue : (Any) => Boolean = Function.const(true)
    val cFalse : (Any) => Boolean = Function.const(false)
    kindCata(
        cFalse // val
      , cFalse // var
      , cFalse // method
      , cFalse // type
      , cTrue  // import
      )
  }

  def isMethod : Boolean = asMethod.nonEmpty

  def asMethod : Option[Decl.Method[IdentInfo, SemanticInfo]] = {
    val cNone : (Any) => Option[Decl.Method[IdentInfo, SemanticInfo]] = Function.const(None)
    kindCata(
        cNone   // value
      , cNone   // variable
      , Some(_) // method
      , cNone   // type
      , cNone   // import
      )
  }
}

object Decl {
  case class Var[IdentInfo, SemanticInfo](l : DLabel, mods : List[meta.Mod], pats : List[meta.Pat], symbols : List[IdentInfo], declType : meta.Type) extends Decl[IdentInfo, SemanticInfo] {
    def label : DLabel = l

    override def toString : String = pats.toString
  }

  case class Val[IdentInfo, SemanticInfo](l : DLabel, mods : List[meta.Mod], pats : List[meta.Pat], symbols : List[IdentInfo], declType : meta.Type) extends Decl[IdentInfo, SemanticInfo] {
    def label : DLabel = l

    override def toString : String = pats.toString
  }

  case class Method[IdentInfo, SemanticInfo](l : DLabel, symbols : List[IdentInfo], mods : List[meta.Mod], name : meta.Term.Name, typeArgs : List[meta.Type.Param], argss : List[List[meta.Term.Param]], declType : meta.Type) extends Decl[IdentInfo, SemanticInfo] {
    def label : DLabel = l

    override def toString : String = {
      val args = argss.map(_.mkString("(", ", ", ")")).mkString("")
      val targs = if (typeArgs.isEmpty) "" else typeArgs.mkString("[", ", ", "]")
      val mods_ = if (mods.length == 0) "" else mods.mkString("", " ", " ")
      s"$mods_$name$targs$args : $declType"
    }
  }

  type TypedMethod = Method[Symbol, List[scalac.Type]]

  case class Type[IdentInfo, SemanticInfo](l : DLabel, symbols : List[IdentInfo], mods : List[meta.Mod], name : meta.Type.Name, params : List[meta.Type.Param], bounds : meta.Type.Bounds) extends Decl[IdentInfo, SemanticInfo] {
    override def label : DLabel = l

    override def toString : String = name.toString
  }

  case class Import[IdentInfo, SemanticInfo](l : DLabel, sugared : meta.Import) extends Decl[IdentInfo, SemanticInfo] {
    override def label : DLabel = l

    override def toString : String = sugared.toString
  }

  type TypedImport = Import[Symbol, List[scalac.Type]]

  def prettyPrint[IdentInfo, SemanticInfo](decl : Decl[IdentInfo, SemanticInfo]) : Doc = {
    import PrettyPrint.{eachFollowedBy, paren}

    def prettyArgss(argss : List[List[meta.Term.Param]]) : Doc =
      Doc.fill(Doc.lineOrEmpty, argss.map(args => paren(Doc.str(args.mkString(", ")))))

    decl.cata(
        (_, mods, pats, _, declType) => // val
          eachFollowedBy(Doc.space, mods) + Doc.spread(List(Doc.text("val"), PrettyPrint.sepBy(Doc.comma + Doc.space, pats), Doc.text(":"), Doc.str(declType)))
      , (_, mods, pats, _, declType) => // var
          eachFollowedBy(Doc.space, mods) + Doc.spread(List(Doc.text("var"), PrettyPrint.sepBy(Doc.comma + Doc.space, pats), Doc.text(":"), Doc.str(declType)))
      , (_, _, mods, name, typeArgs, argss, declType) => // method
          eachFollowedBy(Doc.space, mods) + Doc.text("def") + Doc.space + Doc.str(name) + PrettyPrint.bracketMany1(Doc.comma + Doc.space, typeArgs) +
          prettyArgss(argss) + Doc.space + Doc.text(":") + Doc.space + Doc.str(declType)
      , (_, _, mods, name, params, bounds) => // type
          eachFollowedBy(Doc.space, mods) + Doc.text("type") + Doc.space + Doc.str(name) + PrettyPrint.bracketMany1(Doc.comma + Doc.space, params) + Doc.space + Doc.str(bounds)
      , (_, imports) => // import
          Doc.text("import") + Doc.space + Doc.str(imports)
      )
  }

  /** Converts a declaration into a graph.
   */
  def toDot(d : TypedDecl) : dot.DotGraph = {
    val (nodes, edges) : (List[DotNode], List[dot.DotEdge]) = DotGen.exec(toDotGen(d))
    dot.DotGraph("", nodes, edges)
  }

  def toDotGen(decl : TypedDecl) : dot.DotGen.DotGen[dot.DotNode] =
    decl.cata(
          (l, _, pats, symbols, _) => // val
            DotGen.node(DotNode.record(l, "Val", pats.mkString(", ") + " : " + symbols.map(_.info).mkString(", ")))
        , (l, _, pats, symbols, _) => // var
            DotGen.node(DotNode.record(l, "Var", pats.mkString(", ") + " : " + symbols.map(_.info).mkString(", ")))
        , (l, _, _, _, _, _, _) => // method
            DotGen.node(DotNode.record(l, "Method", decl.toString))
        , (l, _, _, name, _, _) => // type
            DotGen.node(DotNode.record(l, "Type", name.toString))
        , (l, sugared) => // import
            DotGen.node(DotNode.record(l, "Import", sugared.toString))
        )

  def symbols[SemanticInfo](d : Decl[Symbol, SemanticInfo]) : List[Symbol] =
    d.cata(
        (_, _, _, symbols, _) => // val
          symbols
      , (_, _, _, symbols, _) => // var
          symbols
      , (_, symbols, _, _, _, _, _) => // method
          symbols
      , (_, symbols, _, _, _, _) => // type
          symbols
      , (_, _) => // import
          List()
      )
}
