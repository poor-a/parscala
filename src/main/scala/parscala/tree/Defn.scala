package parscala
package tree

import parscala.Control.mapM
import parscala.dot.{DotGen, DotNode, DotGraph}

import org.typelevel.paiges.Doc

sealed abstract class Defn[IdentInfo, SemanticInfo] {
  def label : DLabel

  def cata[A]( fVal : (DLabel, List[meta.Mod], List[meta.Pat], List[IdentInfo], Option[meta.Type], Expr[IdentInfo, SemanticInfo]) => A
             , fVar : (DLabel, List[meta.Mod], List[meta.Pat], List[IdentInfo], Option[meta.Type], Option[Expr[IdentInfo, SemanticInfo]]) => A
             , fMethod : (DLabel, List[IdentInfo], List[meta.Mod], meta.Term.Name, List[meta.Type.Param], List[List[meta.Term.Param]], Option[meta.Type], Expr[IdentInfo, SemanticInfo]) => A
             , fType : (DLabel, List[IdentInfo], List[meta.Mod], meta.Type.Name, List[meta.Type.Param], meta.Type) => A
             , fMacro : (DLabel, List[IdentInfo], List[meta.Mod], meta.Term.Name, List[List[meta.Term.Param]], Expr[IdentInfo, SemanticInfo]) => A
             , fSndCtor : (DLabel, List[IdentInfo], List[meta.Mod], meta.Name, List[List[meta.Term.Param]], (ThisApply[IdentInfo, SemanticInfo], List[Statement[IdentInfo, SemanticInfo]])) => A
             , fClass : (DLabel, List[IdentInfo], List[meta.Mod], meta.Type.Name, List[Statement[IdentInfo, SemanticInfo]]) => A
             , fTrait : (DLabel, List[IdentInfo], List[meta.Mod], meta.Type.Name, List[Statement[IdentInfo, SemanticInfo]]) => A
             , fObject : (DLabel, List[IdentInfo], List[meta.Mod], meta.Term.Name, List[Statement[IdentInfo, SemanticInfo]]) => A
             , fPObject : (DLabel, List[IdentInfo], List[meta.Mod], meta.Term.Name, List[Statement[IdentInfo, SemanticInfo]]) => A
             , fPackage : (DLabel, List[IdentInfo], meta.Term.Ref, List[Statement[IdentInfo, SemanticInfo]]) => A
             ) : A =
    this match {
      case Defn.Var(l, mods, pats, symbols, oDeclType, oRhs) => fVar(l, mods, pats, symbols, oDeclType, oRhs)
      case Defn.Val(l, mods, pats, symbols, oDeclType, rhs) => fVal(l, mods, pats, symbols, oDeclType, rhs)
      case Defn.Method(l, sym, mods, name, typeArgs, argss, oDeclType, body) => fMethod(l, sym, mods, name, typeArgs, argss, oDeclType, body)
      case Defn.Type(l, sym, mods, name, params, body) => fType(l, sym, mods, name, params, body)
      case Defn.Macro(l, sym, mods, name, argss, body) => fMacro(l, sym, mods, name, argss, body)
      case Defn.SecondaryCtor(l, sym, mods, name, paramss, body) => fSndCtor(l, sym, mods, name, paramss, body)
      case Defn.Class(l, sym, mods, name, stats) => fClass(l, sym, mods, name, stats)
      case Defn.Trait(l, sym, mods, name, stats) => fTrait(l, sym, mods, name, stats)
      case Defn.Object(l, sym, mods, name, stats) => fObject(l, sym, mods, name, stats)
      case Defn.PackageObject(l, sym, mods, name, stats) => fPObject(l, sym, mods, name, stats)
      case Defn.Package(l, sym, name, stats) => fPackage(l, sym, name, stats)
    }

  def kindCata[A]( val_ : Defn.Val[IdentInfo, SemanticInfo] => A
                 , var_ : Defn.Var[IdentInfo, SemanticInfo] => A
                 , method_ : Defn.Method[IdentInfo, SemanticInfo] => A
                 , type_ : Defn.Type[IdentInfo, SemanticInfo] => A
                 , macro_ : Defn.Macro[IdentInfo, SemanticInfo] => A
                 , sndCtor_ : Defn.SecondaryCtor[IdentInfo, SemanticInfo] => A
                 , class_ : Defn.Class[IdentInfo, SemanticInfo] => A
                 , trait_ : Defn.Trait[IdentInfo, SemanticInfo] => A
                 , object_ : Defn.Object[IdentInfo, SemanticInfo] => A
                 , pkgobject_ : Defn.PackageObject[IdentInfo, SemanticInfo] => A
                 , package_ : Defn.Package[IdentInfo, SemanticInfo] => A
                 ) : A =
    this match {
      case v : Defn.Val[IdentInfo, SemanticInfo] => val_(v)
      case v : Defn.Var[IdentInfo, SemanticInfo] => var_(v)
      case m : Defn.Method[IdentInfo, SemanticInfo] => method_(m)
      case t : Defn.Type[IdentInfo, SemanticInfo] => type_(t)
      case m : Defn.Macro[IdentInfo, SemanticInfo] => macro_(m)
      case s : Defn.SecondaryCtor[IdentInfo, SemanticInfo] => sndCtor_(s)
      case c : Defn.Class[IdentInfo, SemanticInfo] => class_(c)
      case t : Defn.Trait[IdentInfo, SemanticInfo] => trait_(t)
      case o : Defn.Object[IdentInfo, SemanticInfo] => object_(o)
      case p : Defn.PackageObject[IdentInfo, SemanticInfo] => pkgobject_(p)
      case p : Defn.Package[IdentInfo, SemanticInfo] => package_(p)
    }

  def isTopLevel : Boolean = {
    val cTrue : (Any) => Boolean = Function.const(true)
    val cFalse : (Any) => Boolean = Function.const(false)
    kindCata(
        cFalse // val
      , cFalse // var
      , cFalse // method
      , cFalse // type
      , cFalse // macro
      , cFalse // secondary constructor
      , cTrue  // class
      , cTrue  // trait
      , cTrue  // object
      , cTrue  // package object
      , cTrue  // package
      )
  }

  def asMethod : Option[Defn.Method[IdentInfo, SemanticInfo]] =
    this match {
      case m : Defn.Method[IdentInfo, SemanticInfo] => Some(m)
      case _ => None
    }

  def asClass : Option[Defn.Class[IdentInfo, SemanticInfo]] =
    this match {
      case c : Defn.Class[IdentInfo, SemanticInfo] => Some(c)
      case _ => None
    }

  def asObject : Option[Defn.Object[IdentInfo, SemanticInfo]] =
    this match {
      case o : Defn.Object[IdentInfo, SemanticInfo] => Some(o)
      case _ => None
    }
}

object Defn {
  case class Var[IdentInfo, SemanticInfo](l : DLabel, mods : List[meta.Mod], pats : List[meta.Pat], identInfos : List[IdentInfo], oDeclType : Option[meta.Type], oRhs : Option[Expr[IdentInfo, SemanticInfo]]) extends Defn[IdentInfo, SemanticInfo] {
    def label : DLabel = l

    override def toString : String = identInfos.toString
  }

  case class Val[IdentInfo, SemanticInfo](l : DLabel, mods : List[meta.Mod], pats : List[meta.Pat], identInfos : List[IdentInfo], oDeclType : Option[meta.Type], rhs : Expr[IdentInfo, SemanticInfo]) extends Defn[IdentInfo, SemanticInfo] {
    def label : DLabel = l

    override def toString : String = identInfos.toString
  }

  case class Method[IdentInfo, SemanticInfo](l : DLabel, symbols : List[IdentInfo], mods : List[meta.Mod], name : meta.Term.Name, typeArgs : List[meta.Type.Param], paramss : List[List[meta.Term.Param]], declType : Option[meta.Type], body : Expr[IdentInfo, SemanticInfo]) extends Defn[IdentInfo, SemanticInfo] {
    def label : DLabel = l

    override def toString : String = name.toString
  }

  type TypedMethod = Method[Symbol, List[scalac.Type]]

  case class Type[IdentInfo, SemanticInfo](l : DLabel, infos : List[IdentInfo], mods : List[meta.Mod], name : meta.Type.Name, params : List[meta.Type.Param], body : meta.Type) extends Defn[IdentInfo, SemanticInfo] {
    def label : DLabel = l

    override def toString : String = name.toString
  }

  case class Macro[IdentInfo, SemanticInfo](l : DLabel, infos : List[IdentInfo], mods : List[meta.Mod], name : meta.Term.Name, paramss : List[List[meta.Term.Param]], body : Expr[IdentInfo, SemanticInfo]) extends Defn[IdentInfo, SemanticInfo] {
    def label : DLabel = l

    override def toString : String = {
      val params : String = paramss.map(_.mkString("(",", ", ")")).mkString("")
      s"this$params"
    }
  }

  case class SecondaryCtor[IdentInfo, SemanticInfo](l : DLabel, infos : List[IdentInfo], mods : List[meta.Mod], name : meta.Name, paramss : List[List[meta.Term.Param]], body : (ThisApply[IdentInfo, SemanticInfo], List[Statement[IdentInfo, SemanticInfo]])) extends Defn[IdentInfo, SemanticInfo] {
    def label : DLabel = l

    override def toString : String = {
      val params : String = paramss.map(_.mkString("(",", ", ")")).mkString("")
      s"this$params"
    }
  }

  case class Class[IdentInfo, SemanticInfo](l : DLabel, infos : List[IdentInfo], mods : List[meta.Mod], name : meta.Type.Name, stats : List[Statement[IdentInfo, SemanticInfo]]) extends Defn[IdentInfo, SemanticInfo] {
    def label : DLabel = l

    override def toString : String = infos.toString

    def methods : List[Either[Decl.Method[IdentInfo, SemanticInfo], Method[IdentInfo, SemanticInfo]]] = filterMethods(stats)
  }

  type TypedClass = Class[Symbol, List[scalac.Type]]

  case class Trait[IdentInfo, SemanticInfo](l : DLabel, infos : List[IdentInfo], mods : List[meta.Mod], name : meta.Type.Name, stats : List[Statement[IdentInfo, SemanticInfo]]) extends Defn[IdentInfo, SemanticInfo] {
    def label : DLabel = l

    override def toString : String = infos.toString

    def methods : List[Either[Decl.Method[IdentInfo, SemanticInfo], Method[IdentInfo, SemanticInfo]]] = filterMethods(stats)
  }

  case class Object[IdentInfo, SemanticInfo](l : DLabel, infos : List[IdentInfo], mods : List[meta.Mod], name : meta.Term.Name, stats : List[Statement[IdentInfo, SemanticInfo]]) extends Defn[IdentInfo, SemanticInfo] {
    def label : DLabel = l

    override def toString : String = infos.toString

    def methods : List[Either[Decl.Method[IdentInfo, SemanticInfo], Method[IdentInfo, SemanticInfo]]] = filterMethods(stats)
  }

  type TypedObject = Object[Symbol, List[scalac.Type]]

  case class PackageObject[IdentInfo, SemanticInfo](l : DLabel, symbols : List[IdentInfo], mods : List[meta.Mod], name : meta.Term.Name, stats : List[Statement[IdentInfo, SemanticInfo]]) extends Defn[IdentInfo, SemanticInfo] {
    def label : DLabel = l

    override def toString : String = symbols.toString
  }

  type TypedPackageObject = PackageObject[Symbol, List[scalac.Type]]

  case class Package[IdentInfo, SemanticInfo](l : DLabel, symbols : List[IdentInfo], name : meta.Term.Ref, stats : List[Statement[IdentInfo, SemanticInfo]]) extends Defn[IdentInfo, SemanticInfo] {
    def label : DLabel = l

    override def toString : String = symbols.toString

    def classes : List[Class[IdentInfo, SemanticInfo]] = {
      def asClass[IdentInfo, SemanticInfo](defn : Defn[IdentInfo, SemanticInfo]) : Option[Class[IdentInfo, SemanticInfo]] =
        defn match {
          case c : Class[IdentInfo, SemanticInfo] => Some(c)
          case _ => None
        }
      parscala.Control.catSomes(stats.map(stat =>
          for ( defn <- stat.toDefn;
                class_ <- asClass(defn)
              )
          yield class_
        ))
    }
  }

  type TypedPackage = Package[Symbol, List[scalac.Type]]

  def toString(defn : TypedMethod) = {
    val params : String = defn.paramss.map(_.mkString("(",", ", ")")).mkString("")
    val tparams : String = if (defn.typeArgs.isEmpty) "" else defn.typeArgs.mkString("[", ", ", "]")
    val resultTypes : String = (for (s <- defn.symbols; if s.isMethod; m = s.asMethod) yield m.returnType).mkString(" or ")
    s"${defn.name}$tparams$params : $resultTypes"
  }

  def filterMethods[IdentInfo, SemanticInfo](statements : List[Statement[IdentInfo, SemanticInfo]]) : List[Either[Decl.Method[IdentInfo, SemanticInfo], Method[IdentInfo, SemanticInfo]]] = {
    val bitraverse : scalaz.Bitraverse[Either] = scalaz.std.either.eitherInstance
    val optionApplicative : scalaz.Applicative[Option] = scalaz.std.option.optionInstance
    def asMethodDecl[IdentInfo, SemanticInfo](d : Decl[IdentInfo, SemanticInfo]) : Option[Decl.Method[IdentInfo, SemanticInfo]] =
      d match {
        case m : Decl.Method[IdentInfo, SemanticInfo] => Some(m)
        case _ => None
      }

    parscala.Control.catSomes(statements.map{stat =>
      stat.asSymbolTree.flatMap{ symbolTree =>
        bitraverse.bitraverse(symbolTree.unSymbolTree)(asMethodDecl)(_.asMethod)(optionApplicative)
      } 
    })
  }

  def symbols[SemanticInfo](d : Defn[Symbol, SemanticInfo]) : List[Symbol] =
    d.cata(
        (_, _, _, symbols, _, _) => // val
          symbols
      , (_, _, _, symbols, _, _) => // var
          symbols
      , (_, symbols, _, _, _, _, _, _) => // method
          symbols
      , (_, symbols, _, _, _, _) => // type
          symbols
      , (_, symbols, _, _, _, _) => // macro
          symbols
      , (_, symbols, _, _, _, _) => // secondary constructor
          symbols
      , (_, symbols, _, _, _) => // class
          symbols
      , (_, symbols, _, _, _) => // trait
          symbols
      , (_, symbols, _, _, _) => // object
          symbols
      , (_, symbols, _, _, _) => // package object
          symbols
      , (_, symbols, _, _) => // package
          symbols
      )

  def toDot(d : TypedDefn) : DotGraph = {
    val (nodes, edges) = DotGen.exec(toDotGen(d))
    DotGraph("", nodes, edges)
  }

  def prettyPrint[IdentInfo, SemanticInfo](defn : Defn[IdentInfo, SemanticInfo]) : Doc = prettyPrintConfigurable(defn, Statement.prettyPrint[IdentInfo, SemanticInfo], Expr.prettyPrint[IdentInfo, SemanticInfo])

  def prettyPrintLint(defn : TypedDefn) : Doc = prettyPrintConfigurable(defn, Statement.prettyPrintLint, Expr.prettyPrintLint)

  def prettyPrintConfigurable[IdentInfo, SemanticInfo](defn : Defn[IdentInfo, SemanticInfo], prettyStatement : Statement[IdentInfo, SemanticInfo] => Doc, prettyExpression : Expr[IdentInfo, SemanticInfo] => Doc) : Doc = {
    import PrettyPrint.{paren, bracket, lcurly, rcurly, eachFollowedBy, when}

    def prettyArgss(argss : List[List[meta.Term.Param]]) : Doc =
      Doc.fill(Doc.lineOrEmpty, argss.map(args => paren(Doc.str(args.mkString(", ")))))
    def prettyStatements(stmts : List[Statement[IdentInfo, SemanticInfo]]) : Doc =
      PrettyPrint.curly(when(!stmts.isEmpty, Doc.hardLine) + Doc.intercalate(Doc.hardLine + Doc.hardLine, stmts.map(prettyStatement)).indent(2) + when(!stmts.isEmpty, Doc.hardLine))

    def spaceIf(b : Boolean) : Doc = if (b) Doc.space else Doc.empty

    def typ(t : Option[meta.Type]) : Doc =
      t.fold(Doc.empty)(declType => Doc.text(":") + Doc.space + Doc.str(declType) + Doc.space)

    defn.cata(
        (_l, mods, pats, symbols, oDeclType, rhs) => // val
          eachFollowedBy(Doc.space, mods) + Doc.text("val") + Doc.space + PrettyPrint.sepBy(Doc.comma + Doc.space, pats) + Doc.space + typ(oDeclType) + Doc.text("=") + Doc.space + prettyExpression(rhs)
      , (_l, mods, pats, symbols, oDeclType, oRhs) => // var
          eachFollowedBy(Doc.space, mods) + Doc.text("var") + Doc.space + PrettyPrint.sepBy(Doc.comma + Doc.space, pats) + Doc.space + typ(oDeclType) + Doc.text("=") + Doc.space + oRhs.fold(Doc.text("_"))(prettyExpression)
      , (_l, _symbols, mods, name, typeArgs, argss, oDeclType, body) => // method
          eachFollowedBy(Doc.space, mods) + Doc.text("def") + Doc.space + Doc.str(name) + PrettyPrint.bracketMany1(Doc.comma + Doc.space, typeArgs) +
          prettyArgss(argss) + spaceIf(!argss.isEmpty) + typ(oDeclType) + Doc.text("=") + Doc.lineOrSpace +
          prettyExpression(body)
      , (_l, _symbols, mods, name, params, body) => // type
          eachFollowedBy(Doc.space, mods) + Doc.text("type") + Doc.space + Doc.str(name) + PrettyPrint.bracketMany1(Doc.comma + Doc.space, params) + Doc.space + Doc.text("=") + Doc.lineOrSpace + Doc.str(body)
      , (_l, _symbols, mods, name, argss, body) => // macro
           eachFollowedBy(Doc.space, mods) + Doc.spread(List(Doc.text("def"), Doc.str(name), prettyArgss(argss), Doc.text("="))) + Doc.lineOrSpace + prettyExpression(body)
      , (_l, _symbols, mods, name, paramss, body) => { // secondary constructor
          val (initializer, stmts) = body
          eachFollowedBy(Doc.space, mods) + Doc.text("def") + Doc.space + Doc.text("this") + prettyArgss(paramss) + Doc.space + Doc.text("=") + Doc.space + prettyStatements(Statement.fromExpr(initializer) :: stmts)
        }
      , (_l, _symbols, mods, name, statements) => // class
          eachFollowedBy(Doc.space, mods) + Doc.text("class") + Doc.space + Doc.str(name) + Doc.space + prettyStatements(statements)
      , (_l, _symbols, mods, name, statements) => // trait
          eachFollowedBy(Doc.space, mods) + Doc.text("trait") + Doc.space + Doc.str(name) + Doc.space + prettyStatements(statements)
      , (_l, _symbols, mods, name, statements) => // object
          eachFollowedBy(Doc.space, mods) + Doc.text("object") + Doc.space + Doc.str(name) + Doc.space + prettyStatements(statements)
      , (_l, _symbols, mods, name, statements) => // package object
          eachFollowedBy(Doc.space, mods) + Doc.spread(List(Doc.text("package"), Doc.text("object"), Doc.str(name), prettyStatements(statements)))
      , (_l, _symbols, name, statements) => // package
          Doc.text("package") + Doc.space + Doc.str(name) + Doc.space + prettyStatements(statements)
      )
  }

  def toDotGen(defn : TypedDefn) : DotGen.DotGen[DotNode] = {
    def formatStatement(stmt : TypedStatement) : DotGen.DotGen[DotNode] =
      stmt.fold(
          (decl : TypedDecl) => Decl.toDotGen(decl)
        , (defn : TypedDefn) => Defn.toDotGen(defn)
        , (expr : TypedExpr) => Expr.toDotGen(expr)
        )

      defn.cata(
          (l, _mods, pats, symbols, _, rhs) => // val
            for (right <- Expr.toDotGen(rhs);
                 val_ <- DotGen.node(DotNode.record(l, "Val", pats.mkString(", ") + " : " + symbols.map(_.info).mkString(", ")));
                 _ <- DotGen.edge(val_, right, "rhs"))
            yield val_
        , (l, _mods, pats, symbols, _, oRhs) => // var
            for (var_ <- DotGen.node(DotNode.record(l, "Var", pats.mkString(", ") + " : " + symbols.map(_.info).mkString(", ")));
                 optionTraverse : scalaz.Traverse[Option] = scalaz.std.option.optionInstance;
                 _ <- optionTraverse.traverse[DotGen.DotGen, TypedExpr, Unit](oRhs)(rhs =>
                        for (right <- Expr.toDotGen(rhs);
                             _ <- DotGen.edge(var_, right, "rhs"))
                        yield ()
                      )
                )
            yield var_
        , (l, _symbols, _mods, name, typeArgs, argss, _, body) => // method
            for (b <- Expr.toDotGen(body);
                 m <- DotGen.node(DotNode.record(l, "Method", defn.toString));
                 _ <- DotGen.edge(m, b, "body"))
            yield m
        , (l, _symbols, _mods, name, _params, _body) => // type
            for (tpe <- DotGen.node(DotNode.record(l, "Type member", name.toString)))
            yield tpe
        , (l, _symbols, _mods, name, argss, body) => // macro
            for (b <- Expr.toDotGen(body);
                 mcro <- DotGen.node(DotNode.record(l, "Macro", defn.toString));
                 _ <- DotGen.edge(mcro, b, "body"))
             yield mcro
        , (l, _symbols, _mods, name, paramss, body) => { // secondary constructor
            val (initializer, statements) = body
            for (b <- mapM(formatStatement, Statement.fromExpr(initializer) :: statements);
                 ctor <- DotGen.node(DotNode.record(l, "Secondary constructor", defn.toString));
                 _ <- DotGen.enum(ctor, b, Function.const("")))
            yield ctor
          }
        , (l, _symbols, _mods, name, statements) => // class
            for (stmts <- mapM(formatStatement, statements);
                 class_ <- DotGen.node(DotNode.record(l, "Class", name.toString));
                 _ <- DotGen.enum(class_, stmts, Function.const("")))
            yield class_
        , (l, _symbols, _mods, name, statements) => // trait
            for (stmts <- mapM(formatStatement, statements);
                 trait_ <- DotGen.node(DotNode.record(l, "Trait", name.toString));
                 _ <- DotGen.enum(trait_, stmts, Function.const("")))
            yield trait_
        , (l, _symbols, _mods, name, statements) => // object
            for (stmts <- mapM(formatStatement, statements);
                 object_ <- DotGen.node(DotNode.record(l, "Object", name.toString));
                 _ <- DotGen.enum(object_, stmts, Function.const("")))
            yield object_
        , (l, _symbols, _mods, name, statements) => // package object
            for (stmts <- mapM(formatStatement, statements);
                 pobject <- DotGen.node(DotNode.record(l, "Package object", name.toString));
                 _ <- DotGen.enum(pobject, stmts, Function.const("")))
            yield pobject
        , (l, _symbols, name, statements) => // package
            for (stmts <- mapM(formatStatement, statements);
                 package_ <- DotGen.node(DotNode.record(l, "Package", name.toString));
                 _ <- DotGen.enum(package_, stmts, Function.const("")))
            yield package_
        )
  }
}
