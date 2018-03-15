package parscala
package tree

import parscala.dot

sealed abstract class Defn {
  def label : DLabel
}

object Defn {
  case class Var(val l : DLabel, pats : List[meta.Pat], symbols : List[Symbol], oRhs : Option[Expr]) extends Defn {
    def label : DLabel = l

    override def toString : String = symbols.toString
  }

  case class Val(val l : DLabel, pats : List[meta.Pat], symbols : List[Symbol], rhs : Expr) extends Defn {
    def label : DLabel = l

    override def toString : String = symbols.toString
  }

  case class Method(val l : DLabel, symbols : List[Symbol], name : meta.Term.Name, argss : List[List[meta.Term.Param]], body : Expr) extends Defn {
    def label : DLabel = l

    override def toString : String = symbols.map(_.fullName).toString
  }

  case class Class(val l : DLabel, symbols : List[Symbol], name : meta.Type.Name, stats : List[Statement]) extends Defn {
    def label : DLabel = l

    override def toString : String = symbols.toString

    def methods : List[Either[Decl.Method, Method]] = filterMethods(stats)
  }

  case class Trait(val l : DLabel, symbols : List[Symbol], name : meta.Type.Name, stats : List[Statement]) extends Defn {
    def label : DLabel = l

    override def toString : String = symbols.toString

    def methods : List[Either[Decl.Method, Method]] = filterMethods(stats)
  }

  case class Object(val l : DLabel, symbols : List[Symbol], name : meta.Term.Name, stats : List[Statement]) extends Defn{
    def label : DLabel = l

    override def toString : String = symbols.toString
  }

  case class PackageObject(val l : DLabel, symbols : List[Symbol], name : meta.Term.Name, stats : List[Statement]) extends Defn {
    def label : DLabel = l

    override def toString : String = symbols.toString
  }

  case class Package(val l : DLabel, symbols : List[Symbol], name : meta.Term.Ref, stats : List[Statement]) extends Defn {
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

  def cata[A]( fVal : (DLabel, List[meta.Pat], List[Symbol], Expr) => A
             , fVar : (DLabel, List[meta.Pat], List[Symbol], Option[Expr]) => A
             , fMethod : (DLabel, List[Symbol], meta.Term.Name, List[List[meta.Term.Param]], Expr) => A
             , fClass : (DLabel, List[Symbol], meta.Type.Name, List[Statement]) => A
             , fTrait : (DLabel, List[Symbol], meta.Type.Name, List[Statement]) => A
             , fObject : (DLabel, List[Symbol], meta.Term.Name, List[Statement]) => A
             , fPObject : (DLabel, List[Symbol], meta.Term.Name, List[Statement]) => A
             , fPackage : (DLabel, List[Symbol], meta.Term.Ref, List[Statement]) => A
             , defn : Defn
             ) : A =
    defn match {
      case Var(l, pats, symbols, rhs) => fVar(l, pats, symbols, rhs)
      case Val(l, pats, symbols, rhs) => fVal(l, pats, symbols, rhs)
      case Method(l, sym, name, argss, body) => fMethod(l, sym, name, argss, body)
      case Class(l, sym, name, stats) => fClass(l, sym, name, stats)
      case Trait(l, sym, name, stats) => fTrait(l, sym, name, stats)
      case Object(l, sym, name, stats) => fObject(l, sym, name, stats)
      case PackageObject(l, sym, name, stats) => fPObject(l, sym, name, stats)
      case Package(l, sym, name, stats) => fPackage(l, sym, name, stats)
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

  def isTopLevel(d : Defn) : Boolean = {
    val cTrue : (Any) => Boolean = Function.const(true)
    val cFalse : (Any) => Boolean = Function.const(false)
    kindCata(
        cFalse // val
      , cFalse // var
      , cFalse // method
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

  def toDot(d : Defn) : dot.DotGraph =
    dot.DotGraph("", List(), List())
}
