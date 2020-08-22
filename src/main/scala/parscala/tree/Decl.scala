package parscala
package tree

import parscala.dot
import parscala.dot.{DotNode, DotGen}

import org.typelevel.paiges.Doc

/**
 * Superclass of declarations and definitions.
 */
sealed abstract class Decl {
  def label : DLabel
}

object Decl {
  case class Var(l : DLabel, mods : List[meta.Mod], pats : List[meta.Pat], symbols : List[Symbol], declType : meta.Type) extends Decl {
    def label : DLabel = l

    override def toString : String = pats.toString
  }

  case class Val(l : DLabel, mods : List[meta.Mod], pats : List[meta.Pat], symbols : List[Symbol], declType : meta.Type) extends Decl {
    def label : DLabel = l

    override def toString : String = pats.toString
  }

  case class Method(l : DLabel, symbols : List[Symbol], mods : List[meta.Mod], name : meta.Term.Name, typeArgs : List[meta.Type.Param], argss : List[List[meta.Term.Param]], declType : meta.Type) extends Decl {
    def label : DLabel = l

    override def toString : String = {
      val args = argss.map(_.mkString("(", ", ", ")")).mkString("")
      val targs = if (typeArgs.isEmpty) "" else typeArgs.mkString("[", ", ", "]")
      val mods_ = if (mods.length == 0) "" else mods.mkString("", " ", " ")
      s"$mods_$name$targs$args : $declType"
    }
  }

  case class Type(l : DLabel, symbols : List[Symbol], mods : List[meta.Mod], name : meta.Type.Name, params : List[meta.Type.Param], bounds : meta.Type.Bounds) extends Decl {
    override def label : DLabel = l

    override def toString : String = name.toString
  }

  case class Import(l : DLabel, sugared : meta.Import) extends Decl {
    override def label : DLabel = l

    override def toString : String = sugared.toString
  }

  def cata[A]( fVal : (DLabel, List[meta.Mod], List[meta.Pat], List[Symbol], meta.Type) => A
             , fVar : (DLabel, List[meta.Mod], List[meta.Pat], List[Symbol], meta.Type) => A
             , fMethod : (DLabel, List[Symbol], List[meta.Mod], meta.Term.Name, List[meta.Type.Param], List[List[meta.Term.Param]], meta.Type) => A
             , fType : (DLabel, List[Symbol], List[meta.Mod], meta.Type.Name, List[meta.Type.Param], meta.Type.Bounds) => A
             , fImport : (DLabel, meta.Import) => A
             , decl : Decl
             ) : A =
    decl match {
      case Val(l, mods, pats, symbols, declType) => fVal(l, mods, pats, symbols, declType)
      case Var(l, mods, pats, symbols, declType) => fVar(l, mods, pats, symbols, declType)
      case Method(l, sym, mods, name, typeArgs, argss, declType) => fMethod(l, sym, mods, name, typeArgs, argss, declType)
      case Type(l, sym, mods, name, params, bounds) => fType(l, sym, mods, name, params, bounds)
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
    import PrettyPrint.{eachFollowedBy, paren}

    def prettyArgss(argss : List[List[meta.Term.Param]]) : Doc =
      Doc.fill(Doc.lineOrEmpty, argss.map(args => paren(Doc.str(args.mkString(", ")))))

    cata(
        (_l, mods, pats, symbols, declType) => // val
          eachFollowedBy(Doc.space, mods) + Doc.spread(List(Doc.text("val"), PrettyPrint.sepBy(Doc.comma + Doc.space, pats), Doc.text(":"), Doc.str(declType)))
      , (_l, mods, pats, symbols, declType) => // var
          eachFollowedBy(Doc.space, mods) + Doc.spread(List(Doc.text("var"), PrettyPrint.sepBy(Doc.comma + Doc.space, pats), Doc.text(":"), Doc.str(declType)))
      , (_l, _symbols, mods, name, typeArgs, argss, declType) => // method
          eachFollowedBy(Doc.space, mods) + Doc.text("def") + Doc.space + Doc.str(name) + PrettyPrint.bracketMany1(Doc.comma + Doc.space, typeArgs) +
          prettyArgss(argss) + Doc.space + Doc.text(":") + Doc.space + Doc.str(declType)
      , (_l, _symbols, mods, name, params, _bounds) => // type
          eachFollowedBy(Doc.space, mods) + Doc.text("type") + Doc.space + Doc.str(name) + PrettyPrint.bracketMany1(Doc.comma + Doc.space, params)
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
          (l, _mods, pats, symbols, _declType) => // val
            DotGen.node(DotNode.record(l, "Val", pats.mkString(", ") + " : " + symbols.map(_.info).mkString(", ")))
        , (l, _mods, pats, symbols, _declType) => // var
            DotGen.node(DotNode.record(l, "Var", pats.mkString(", ") + " : " + symbols.map(_.info).mkString(", ")))
        , (l, symbols, _mods, name, typeArgs, argss, _declType) => // method
            DotGen.node(DotNode.record(l, "Method", decl.toString))
        , (l, _symbol, _mods, name, _params, _bounds) => // type
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

  def symbols(d : Decl) : List[Symbol] =
    Decl.cata(
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
      , d
      )
}
