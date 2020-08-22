package parscala
package tree

import parscala.Control.mapM
import parscala.dot.{DotGen, DotNode, DotGraph}

import org.typelevel.paiges.Doc

sealed abstract class Defn {
  def label : DLabel
}

object Defn {
  case class Var(l : DLabel, mods : List[meta.Mod], pats : List[meta.Pat], symbols : List[Symbol], oDeclType : Option[meta.Type], oRhs : Option[Expr]) extends Defn {
    def label : DLabel = l

    override def toString : String = symbols.toString
  }

  case class Val(l : DLabel, mods : List[meta.Mod], pats : List[meta.Pat], symbols : List[Symbol], oDeclType : Option[meta.Type], rhs : Expr) extends Defn {
    def label : DLabel = l

    override def toString : String = symbols.toString
  }

  case class Method(l : DLabel, symbols : List[Symbol], mods : List[meta.Mod], name : meta.Term.Name, typeArgs : List[meta.Type.Param], paramss : List[List[meta.Term.Param]], declType : Option[meta.Type], body : Expr) extends Defn {
    def label : DLabel = l

    override def toString : String = {
      val params : String = paramss.map(_.mkString("(",", ", ")")).mkString("")
      val tparams : String = if (typeArgs.isEmpty) "" else typeArgs.mkString("[", ", ", "]")
      val resultTypes : String = (for (s <- symbols; if s.isMethod; m = s.asMethod) yield m.returnType).mkString(" or ")
      s"$name$tparams$params : $resultTypes"
    }
  }

  case class Type(l : DLabel, symbols : List[Symbol], mods : List[meta.Mod], name : meta.Type.Name, params : List[meta.Type.Param], body : meta.Type) extends Defn {
    def label : DLabel = l

    override def toString : String = name.toString
  }

  case class Macro(l : DLabel, symbols : List[Symbol], mods : List[meta.Mod], name : meta.Term.Name, paramss : List[List[meta.Term.Param]], body : Expr) extends Defn {
    def label : DLabel = l

    override def toString : String = {
      val params : String = paramss.map(_.mkString("(",", ", ")")).mkString("")
      s"this$params"
    }
  }

  case class SecondaryCtor(l : DLabel, symbols : List[Symbol], mods : List[meta.Mod], name : meta.Name, paramss : List[List[meta.Term.Param]], body : (ThisApply, List[Statement])) extends Defn {
    def label : DLabel = l

    override def toString : String = {
      val params : String = paramss.map(_.mkString("(",", ", ")")).mkString("")
      s"this$params"
    }
  }

  case class Class(l : DLabel, symbols : List[Symbol], mods : List[meta.Mod], name : meta.Type.Name, stats : List[Statement]) extends Defn {
    def label : DLabel = l

    override def toString : String = symbols.toString

    def methods : List[Either[Decl.Method, Method]] = filterMethods(stats)
  }

  case class Trait(l : DLabel, symbols : List[Symbol], mods : List[meta.Mod], name : meta.Type.Name, stats : List[Statement]) extends Defn {
    def label : DLabel = l

    override def toString : String = symbols.toString

    def methods : List[Either[Decl.Method, Method]] = filterMethods(stats)
  }

  case class Object(l : DLabel, symbols : List[Symbol], mods : List[meta.Mod], name : meta.Term.Name, stats : List[Statement]) extends Defn{
    def label : DLabel = l

    override def toString : String = symbols.toString

    def methods : List[Either[Decl.Method, Method]] = filterMethods(stats)
  }

  case class PackageObject(l : DLabel, symbols : List[Symbol], mods : List[meta.Mod], name : meta.Term.Name, stats : List[Statement]) extends Defn {
    def label : DLabel = l

    override def toString : String = symbols.toString
  }

  case class Package(l : DLabel, symbols : List[Symbol], name : meta.Term.Ref, stats : List[Statement]) extends Defn {
    def label : DLabel = l

    override def toString : String = symbols.toString

    def classes : List[Class] = {
      def asClass(defn : Defn) : Option[Class] =
        defn match {
          case c : Class => Some(c)
          case _         => None
        }
      parscala.Control.catSomes(stats.map(stat =>
          for ( defn <- stat.toDefn;
                class_ <- asClass(defn)
              )
          yield class_
        ))
    }
  }

  def filterMethods(statements : List[Statement]) : List[Either[Decl.Method, Method]] = {
    val bitraverse : scalaz.Bitraverse[Either] = scalaz.std.either.eitherInstance
    val optionApplicative : scalaz.Applicative[Option] = scalaz.std.option.optionInstance
    def asMethodDecl(d : Decl) : Option[Decl.Method] =
      d match {
        case m : Decl.Method => Some(m)
        case _ => None
      }

    parscala.Control.catSomes(statements.map{stat =>
      stat.asSymbolTree.flatMap{ symbolTree =>
        bitraverse.bitraverse(symbolTree.unSymbolTree)(asMethodDecl)(asMethod)(optionApplicative)
      } 
    })
  }

  def cata[A]( fVal : (DLabel, List[meta.Mod], List[meta.Pat], List[Symbol], Option[meta.Type], Expr) => A
             , fVar : (DLabel, List[meta.Mod], List[meta.Pat], List[Symbol], Option[meta.Type], Option[Expr]) => A
             , fMethod : (DLabel, List[Symbol], List[meta.Mod], meta.Term.Name, List[meta.Type.Param], List[List[meta.Term.Param]], Option[meta.Type], Expr) => A
             , fType : (DLabel, List[Symbol], List[meta.Mod], meta.Type.Name, List[meta.Type.Param], meta.Type) => A
             , fMacro : (DLabel, List[Symbol], List[meta.Mod], meta.Term.Name, List[List[meta.Term.Param]], Expr) => A
             , fSndCtor : (DLabel, List[Symbol], List[meta.Mod], meta.Name, List[List[meta.Term.Param]], (ThisApply, List[Statement])) => A
             , fClass : (DLabel, List[Symbol], List[meta.Mod], meta.Type.Name, List[Statement]) => A
             , fTrait : (DLabel, List[Symbol], List[meta.Mod], meta.Type.Name, List[Statement]) => A
             , fObject : (DLabel, List[Symbol], List[meta.Mod], meta.Term.Name, List[Statement]) => A
             , fPObject : (DLabel, List[Symbol], List[meta.Mod], meta.Term.Name, List[Statement]) => A
             , fPackage : (DLabel, List[Symbol], meta.Term.Ref, List[Statement]) => A
             , defn : Defn
             ) : A =
    defn match {
      case Var(l, mods, pats, symbols, oDeclType, oRhs) => fVar(l, mods, pats, symbols, oDeclType, oRhs)
      case Val(l, mods, pats, symbols, oDeclType, rhs) => fVal(l, mods, pats, symbols, oDeclType, rhs)
      case Method(l, sym, mods, name, typeArgs, argss, oDeclType, body) => fMethod(l, sym, mods, name, typeArgs, argss, oDeclType, body)
      case Type(l, sym, mods, name, params, body) => fType(l, sym, mods, name, params, body)
      case Macro(l, sym, mods, name, argss, body) => fMacro(l, sym, mods, name, argss, body)
      case SecondaryCtor(l, sym, mods, name, paramss, body) => fSndCtor(l, sym, mods, name, paramss, body)
      case Class(l, sym, mods, name, stats) => fClass(l, sym, mods, name, stats)
      case Trait(l, sym, mods, name, stats) => fTrait(l, sym, mods, name, stats)
      case Object(l, sym, mods, name, stats) => fObject(l, sym, mods, name, stats)
      case PackageObject(l, sym, mods, name, stats) => fPObject(l, sym, mods, name, stats)
      case Package(l, sym, name, stats) => fPackage(l, sym, name, stats)
    }

  def kindCata[A]( val_ : Val => A
                 , var_ : Var => A
                 , method_ : Method => A
                 , type_ : Type => A
                 , macro_ : Macro => A
                 , sndCtor_ : SecondaryCtor => A
                 , class_ : Class => A
                 , trait_ : Trait => A
                 , object_ : Object => A
                 , pkgobject_ : PackageObject => A
                 , package_ : Package => A
                 , d : Defn
                 ) : A =
    d match {
      case v : Val => val_(v)
      case v : Var => var_(v)
      case m : Method => method_(m)
      case t : Type => type_(t)
      case m : Macro => macro_(m)
      case s : SecondaryCtor => sndCtor_(s)
      case c : Class => class_(c)
      case t : Trait => trait_(t)
      case o : Object => object_(o)
      case p : PackageObject => pkgobject_(p)
      case p : Package => package_(p)
    }

  def isTopLevel(d : Defn) : Boolean = {
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
      , d
      )
  }

  def asMethod(d : Defn) : Option[Method] =
    d match {
      case m : Method => Some(m)
      case _ => None
    }

  def asClass(d : Defn) : Option[Class] =
    d match {
      case c : Class => Some(c)
      case _ => None
    }

  def asObject(d : Defn) : Option[Object] =
    d match {
      case o : Object => Some(o)
      case _ => None
    }

  def symbols(d : Defn) : List[Symbol] =
    cata(
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
      , d
      )

  def toDot(d : Defn) : DotGraph = {
    val (nodes, edges) = DotGen.exec(toDotGen(d))
    DotGraph("", nodes, edges)
  }

  def prettyPrint(defn : Defn) : Doc = prettyPrintConfigurable(defn, Statement.prettyPrint, Expr.prettyPrint)

  def prettyPrintLint(defn : Defn) : Doc = prettyPrintConfigurable(defn, Statement.prettyPrintLint, Expr.prettyPrintLint)

  def prettyPrintConfigurable(defn : Defn, prettyStatement : Statement => Doc, prettyExpression : Expr => Doc) : Doc = {
    import PrettyPrint.{paren, bracket, lcurly, rcurly, eachFollowedBy, when}

    def prettyArgss(argss : List[List[meta.Term.Param]]) : Doc =
      Doc.fill(Doc.lineOrEmpty, argss.map(args => paren(Doc.str(args.mkString(", ")))))
    def prettyStatements(stmts : List[Statement]) : Doc =
      PrettyPrint.curly(when(!stmts.isEmpty, Doc.hardLine) + Doc.intercalate(Doc.hardLine + Doc.hardLine, stmts.map(prettyStatement)).indent(2) + when(!stmts.isEmpty, Doc.hardLine))

    def spaceIf(b : Boolean) : Doc = if (b) Doc.space else Doc.empty

    def typ(t : Option[meta.Type]) : Doc =
      t.fold(Doc.empty)(declType => Doc.text(":") + Doc.space + Doc.str(declType) + Doc.space)

    cata(
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
      , defn
      )
  }

  def toDotGen(defn : Defn) : DotGen.DotGen[DotNode] = {
    def formatStatement(stmt : Statement) : DotGen.DotGen[DotNode] =
      stmt.fold(
          (decl : Decl) => Decl.toDotGen(decl)
        , (defn : Defn) => Defn.toDotGen(defn)
        , (expr : Expr) => Expr.toDotGen(expr)
        )

      cata(
          (l, _mods, pats, symbols, _, rhs) => // val
            for (right <- Expr.toDotGen(rhs);
                 val_ <- DotGen.node(DotNode.record(l, "Val", pats.mkString(", ") + " : " + symbols.map(_.info).mkString(", ")));
                 _ <- DotGen.edge(val_, right, "rhs"))
            yield val_
        , (l, _mods, pats, symbols, _, oRhs) => // var
            for (var_ <- DotGen.node(DotNode.record(l, "Var", pats.mkString(", ") + " : " + symbols.map(_.info).mkString(", ")));
                 optionTraverse : scalaz.Traverse[Option] = scalaz.std.option.optionInstance;
                 _ <- optionTraverse.traverse[DotGen.DotGen, Expr, Unit](oRhs)(rhs =>
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
        , defn
        )
  }
}
