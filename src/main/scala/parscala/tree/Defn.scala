package parscala
package tree

import parscala.dot

sealed abstract class Defn {
  def label : DLabel
}

object Defn {
  case class Var(val l : DLabel, pats : List[meta.Pat], symbols : Set[Symbol], rhs : Node, sugared : meta.Decl.Var) extends Defn {
    def label : DLabel = l

    override def toString : String = sugared.toString
  }

  case class Val(val l : DLabel, pats : List[meta.Pat], symbols : Set[Symbol], rhs : Node, sugared : meta.Decl.Val) extends Defn {
    def label : DLabel = l

    override def toString : String = sugared.toString
  }

  case class Method(val l : DLabel, symbol : Symbol, name : meta.Term.Name, argss : List[List[meta.Term.Param]], body : Node, sugared : meta.Decl.Def) extends Defn {
    def label : DLabel = l

    override def toString : String = symbol.toString
  }

  case class Class(val l : DLabel, symbol : Symbol, name : String, stats : List[Statement], sugared : meta.Defn.Class) extends Defn {
    def label : DLabel = l

    override def toString : String = symbol.toString

    def methods : List[Either[Decl.Method, Method]] = {
      val bitraverse : scalaz.Bitraverse[Either] = scalaz.std.either.eitherInstance
      val optionApplicative : scalaz.Applicative[Option] = scalaz.std.option.optionInstance
      def asMethodDecl(d : Decl) : Option[Decl.Method] =
        d match {
          case m : Decl.Method => Some(m)
          case _ => None
        }

      parscala.Control.catSomes(stats.map{stat =>
        stat.asSymbolTree.flatMap{ symbolTree =>
          bitraverse.bitraverse(symbolTree.unSymbolTree)(asMethodDecl)(asMethod)(optionApplicative)
        } 
      })
    }
  }

  case class Object(val l : DLabel, symbol : Symbol, name : String, stats : List[Statement], sugared : meta.Defn.Object) extends Defn{
    def label : DLabel = l

    override def toString : String = symbol.toString
  }

  case class PackageObject(val l : DLabel, symbol : Symbol, name : String, stats : List[Statement], sugared : meta.Defn.Object) extends Defn {
    def label : DLabel = l

    override def toString : String = symbol.toString
  }

  case class Package(val l : DLabel, symbol : Symbol, name : String, stats : List[Statement], sugared : meta.Pkg) extends Defn {
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

  def cata[A]( fVal : (DLabel, List[meta.Pat], Set[Symbol], Node, meta.Decl.Val) => A
             , fVar : (DLabel, List[meta.Pat], Set[Symbol], Node, meta.Decl.Var) => A
             , fMethod : (DLabel, Symbol, meta.Term.Name, List[List[meta.Term.Param]], Node, meta.Decl.Def) => A
             , fClass : (DLabel, Symbol, String, List[Statement], meta.Defn.Class) => A
             , fObject : (DLabel, Symbol, String, List[Statement], meta.Defn.Object) => A
             , fPObject : (DLabel, Symbol, String, List[Statement], meta.Defn.Object) => A
             , fPackage : (DLabel, Symbol, String, List[Statement], meta.Pkg) => A
             , defn : Defn
             ) : A =
    defn match {
      case Var(l, pats, symbols, rhs, sugared) => fVar(l, pats, symbols, rhs, sugared)
      case Val(l, pats, symbols, rhs, sugared) => fVal(l, pats, symbols, rhs, sugared)
      case Method(l, sym, name, argss, body, sugared) => fMethod(l, sym, name, argss, body, sugared)
      case Class(l, sym, name, stats, sugared) => fClass(l, sym, name, stats, sugared)
      case Object(l, sym, name, stats, sugared) => fObject(l, sym, name, stats, sugared)
      case PackageObject(l, sym, name, stats, sugared) => fPObject(l, sym, name, stats, sugared)
      case Package(l, sym, name, stats, sugared) => fPackage(l, sym, name, stats, sugared)
    }

  def kindCata[A]( val_ : Val => A
                 , var_ : Var => A
                 , method_ : Method => A
                 , class_ : Class => A
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
