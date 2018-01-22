package parscala
package tree

sealed abstract class Defn extends Statement with SymbolTree

object Defn {
case class Var(val l : DLabel, pats : List[meta.Pat], symbols : Set[Symbol], rhs : Node, sugared : meta.Decl.Var) extends Decl {
  def label : DLabel = l

  override def toString : String = sugared.toString
}

case class Val(val l : DLabel, pats : List[meta.Pat], symbols : Set[Symbol], rhs : Node, sugared : meta.Decl.Val) extends Decl {
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

  def methods : List[Either Decl.Method Method] = 
    parscala.Control.catSomes(stats.map(asMethod))
}

case class Object(val l : DLabel, symbol : Symbol, name : String, stats : List[Statement], sugared : meta.Defn.Object) extends Decl {
  def label : DLabel = l

  override def toString : String = symbol.toString
}

case class PackageObject(val l : DLabel, symbol : Symbol, name : String, stats : List[Statement], sugared : meta.Defn.Object) extends Decl {
  def label : DLabel = l

  override def toString : String = symbol.toString
}

case class Package(val l : DLabel, symbol : Symbol, name : String, stats : List[Statement], sugared : meta.Pkg) extends Decl {
  def label : DLabel = l

  override def toString : String = symbol.toString

  def classes : List[Class] =
    parscala.Control.catSomes(stats.map(asClass))
}

  def cata[A]( fVal : (DLabel, List[meta.Pat], Set[Symbol], Node, meta.Decl.Val) => A
             , fVar : (DLabel, List[meta.Pat], Set[Symbol], Node, meta.Decl.Var) => A
             , fMethod : (DLabel, Symbol, meta.Term.Name, List[List[meta.Term.Param]], meta.Decl.Def) => A
             , fClass : (DLabel, Symbol, String, List[Statement], meta.Defn.Class) => A
             , fObject : (DLabel, Symbol, String, List[Statement], meta.Defn.Object) => A
             , fPObject : (DLabel, Symbol, String, List[Statement], meta.Defn.Object) => A
             , fPackage : (DLabel, Symbol, String, List[Statement], meta.Pkg) => A
             , defn : Defn
             ) : A =
    defn match {
      case Var(l, pats, symbols, rhs, desugared) => fVar(l, pats, symbols, rhs, desugared)
      case Val(l, pats, symbols, rhs, desugared) => fVal(l, pats, symbols, rhs, desugared)
      case Method(l, sym, name, argss, body, desugared) => fMethod(l, sym, name, argss, body, desugared)
      case Class(l, sym, name, stats, desugared) => fClass(l, sym, name, stats, desugared)
      case Object(l, sym, name, stats, desugared) => fObject(l, sym, name, stats, desugared)
      case PackageObject(l, sym, name, stats, desugared) => fPObject(l, sym, name, stats, desugared)
      case Package(l, sym, name, stats, desugared) => fPackage(l, sym, name, stats, desugared)
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
      case o : Object => class_(o)
      case p : PackageObject => object_(p)
      case p : Package => package_(p)
    }
}
