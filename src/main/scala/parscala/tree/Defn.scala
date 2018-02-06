package parscala
package tree

import parscala.dot

sealed abstract class Defn {
  def label : DLabel
}

object Defn {
  case class Var(val l : DLabel, pats : List[meta.Pat], symbols : Set[Symbol], oRhs : Option[Expr], sugared : meta.Defn.Var, desugared : List[scalac.ValDef], gettersSetters : List[scalac.DefDef]) extends Defn {
    def label : DLabel = l

    override def toString : String = sugared.toString
  }

  case class Val(val l : DLabel, pats : List[meta.Pat], symbols : Set[Symbol], rhs : Expr, sugared : meta.Defn.Val, desugared : List[scalac.ValDef], gettersSetters : List[scalac.DefDef]) extends Defn {
    def label : DLabel = l

    override def toString : String = sugared.toString
  }

  case class Method(val l : DLabel, symbol : Symbol, name : meta.Term.Name, argss : List[List[meta.Term.Param]], body : Expr, sugared : meta.Defn.Def, desugared : scalac.DefDef) extends Defn {
    def label : DLabel = l

    override def toString : String = symbol.toString
  }

  case class Class(val l : DLabel, symbol : Symbol, name : meta.Type.Name, stats : List[Statement], sugared : meta.Defn.Class, desugared : scalac.ClassDef) extends Defn {
    def label : DLabel = l

    override def toString : String = symbol.toString

    def methods : List[Either[Decl.Method, Method]] = filterMethods(stats)
  }

  case class Trait(val l : DLabel, symbol : Symbol, name : meta.Type.Name, stats : List[Statement], sugared : meta.Defn.Trait, desugared : scalac.ClassDef) extends Defn {
    def label : DLabel = l

    override def toString : String = symbol.toString

    def methods : List[Either[Decl.Method, Method]] = filterMethods(stats)
  }

  case class Object(val l : DLabel, symbol : Symbol, name : meta.Term.Name, stats : List[Statement], sugared : meta.Defn.Object, desugared : scalac.ModuleDef) extends Defn{
    def label : DLabel = l

    override def toString : String = symbol.toString
  }

  case class PackageObject(val l : DLabel, symbol : Symbol, name : meta.Term.Name, stats : List[Statement], sugared : meta.Pkg.Object, desugared : scalac.ModuleDef) extends Defn {
    def label : DLabel = l

    override def toString : String = symbol.toString
  }

  case class Package(val l : DLabel, symbol : Symbol, name : String, stats : List[Statement], sugared : meta.Pkg, desugared : scalac.PackageDef) extends Defn {
    def label : DLabel = l

    override def toString : String = symbol.toString

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

  def cata[A]( fVal : (DLabel, List[meta.Pat], Set[Symbol], Expr, meta.Defn.Val, List[scalac.ValDef], List[scalac.DefDef]) => A
             , fVar : (DLabel, List[meta.Pat], Set[Symbol], Option[Expr], meta.Defn.Var, List[scalac.ValDef], List[scalac.DefDef]) => A
             , fMethod : (DLabel, Symbol, meta.Term.Name, List[List[meta.Term.Param]], Expr, meta.Defn.Def, scalac.DefDef) => A
             , fClass : (DLabel, Symbol, meta.Type.Name, List[Statement], meta.Defn.Class, scalac.ClassDef) => A
             , fTrait : (DLabel, Symbol, meta.Type.Name, List[Statement], meta.Defn.Trait, scalac.ClassDef) => A
             , fObject : (DLabel, Symbol, meta.Term.Name, List[Statement], meta.Defn.Object, scalac.ModuleDef) => A
             , fPObject : (DLabel, Symbol, meta.Term.Name, List[Statement], meta.Pkg.Object, scalac.ModuleDef) => A
             , fPackage : (DLabel, Symbol, String, List[Statement], meta.Pkg, scalac.PackageDef) => A
             , defn : Defn
             ) : A =
    defn match {
      case Var(l, pats, symbols, oRhs, sugared, desugared, gettersSetters) => fVar(l, pats, symbols, oRhs, sugared, desugared, gettersSetters)
      case Val(l, pats, symbols, rhs, sugared, desugared, gettersSetters) => fVal(l, pats, symbols, rhs, sugared, desugared, gettersSetters)
      case Method(l, sym, name, argss, body, sugared, desugared) => fMethod(l, sym, name, argss, body, sugared, desugared)
      case Class(l, sym, name, stats, sugared, desugared) => fClass(l, sym, name, stats, sugared, desugared)
      case Trait(l, sym, name, stats, sugared, desugared) => fTrait(l, sym, name, stats, sugared, desugared)
      case Object(l, sym, name, stats, sugared, desugared) => fObject(l, sym, name, stats, sugared, desugared)
      case PackageObject(l, sym, name, stats, sugared, desugared) => fPObject(l, sym, name, stats, sugared, desugared)
      case Package(l, sym, name, stats, sugared, desugared) => fPackage(l, sym, name, stats, sugared, desugared)
    }

  def kindCata[A]( val_ : Val => A
                 , var_ : Var => A
                 , method_ : Method => A
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
      case c : Class => class_(c)
      case t : Trait => trait_(t)
      case o : Object => object_(o)
      case p : PackageObject => pkgobject_(p)
      case p : Package => package_(p)
    }

  def asMethod(d : Defn) : Option[Method] =
    d match {
      case m : Method => Some(m)
      case _ => None
    }

  def toDot(d : Defn) : dot.DotGraph =
    dot.DotGraph("", List(), List())
}
