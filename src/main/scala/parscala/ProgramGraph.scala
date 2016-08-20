package parscala

import tree._
import callgraph.{CallGraph,CallGraphBuilder}

class ProgramGraph(val packages : Set[Package]) {
  def <+> (g : ProgramGraph) : ProgramGraph = {
    val common  = packages intersect g.packages
    val updated = common map (p => p <+> g.packages.find(_ == p).get)
    new ProgramGraph(updated ++ packages ++ g.packages)
  }

  def methods : Set[Method] =
    (packages.toIterator flatMap (_.classes) flatMap (_.methods)).toSet

  def callGraph : (CallGraph, ProgramGraph) = {
    val cs : Iterator[CallGraph] = methods.toIterator map (CallGraphBuilder.getCalls(_))
    val g : CallGraph = cs.foldLeft(CallGraphBuilder.empty)(_ ++ _)
    val g2 : CallGraph = methods.foldLeft(g){(acc,m) => acc.resolve(m)}
    val (ms, pg) = missingMethods(g2.unknown)
    (ms.foldLeft(g2){(acc,m) => acc.resolve(m)}, pg)
  }

  private def topLevelClass(ms : Set[Method]) : Set[Class] = {
    val groups : Map[Symbol, Set[Method]] = ms groupBy (_.symbol.owner)
    groups.foldLeft(Set.empty[Class]){(acc,g) => acc + Class(g._1, g._2, Set.empty)}
  }

  def missingMethods(s : Set[Symbol]) : (Set[Method],ProgramGraph) = {
    val syms : Iterator[Symbol] = s.toIterator filter (_.isMethod)
    val methods : Set[Method] = (syms map (Method(_, None))).toSet
    val classes : Set[Class] = topLevelClass(methods)
    val cGroups : Map[Symbol, Set[Class]] = classes filter (_.symbol.owner.isPackage) groupBy (_.symbol.owner)
    val packages : Set[Package] = cGroups.foldLeft(Set.empty[Package]){(acc,g) => acc + Package(g._1, g._2)}
    (methods, new ProgramGraph(packages))
  }
}

object ProgramGraph {
  import compiler.Quasiquote

  def fields(c : Tree) : Set[Field] = {
    def f(acc : List[Field], ast : Tree) : List[Field] = {
      ast match {
        case q"$mods val $name: $tpt = $expr" =>
          Field(ast) :: acc
        case q"$mods val $pat = $expr" =>
          Field(ast) :: acc
        case q"$mods var $tname: $tpt = $expr" =>
          Field(ast) :: acc
        case q"$mods var $pat = $expr" =>
          Field(ast) :: acc
        case _ => 
          acc
      }
    }

    val defs : List[Tree] = c match {
      case q"$_ class $_ extends ..$earlydefs { ..$defs }" => defs
      case q"$_ class $_ extends $earlydefs with ..$parents { ..$defs }" => defs
      case q"$_ object $_ extends ..$earlydefs { ..$defs }" => defs
      case q"$_ object $_ extends $earlydefs with ..$parents { ..$defs }" => defs
      case q"package object $_ extends ..$earlydefs { ..$defs }" => defs
      case q"package object $_ extends $earlydefs with ..$parents { ..$defs }" => defs
      case _ =>
        List.empty
    }

    val fs : List[Field] = defs.foldLeft(List.empty[Field])(f)
    fs.toSet
  }

  def methods(c : Tree) : Set[Method] = {
    def f(acc : List[Method], ast : Tree) : List[Method] = {
      ast match {
        case q"$_ def $_ (...$_) : $_ = $_" => 
          Method(ast.symbol, Some(ast)) :: acc
        case _ =>
          acc
      }
    }

    val defs : List[Tree] = c match {
      case q"$_ class $_ extends ..$earlydefs { ..$defs }" => defs
      case q"$_ class $_ extends $earlydefs with ..$parents { ..$defs }" => defs
      case q"$_ object $_ extends ..$earlydefs { ..$defs }" => defs
      case q"$_ object $_ extends $earlydefs with ..$parents { ..$defs }" => defs
      case q"package object $_ extends ..$earlydefs { ..$defs }" => defs
      case q"package object $_ extends $earlydefs with ..$parents { ..$defs }" => defs
      case _ =>
        List.empty
    }
    val ms : List[Method] = defs.foldLeft(List.empty[Method])(f)
    ms.toSet
  }

  def classes(p : Tree) : Set[Class] = {
    def f (acc : List[Class], ast : Tree) : List[Class] = {
      ast match {
        case q"$_ class $_ extends $_" =>
          Class(ast.symbol, Set.empty, Set.empty) :: acc
        case q"$_ class $_ extends $_ { ..$_ }" => 
          Class(ast.symbol, methods(ast), fields(ast)) :: acc
        case q"$_ class $_ extends $_ with $_ { ..$_ }" =>
          Class(ast.symbol, methods(ast), fields(ast)) :: acc
        case q"$_ object $_ extends $_" => 
          Class(ast.symbol, Set.empty, Set.empty) :: acc
        case q"$_ object $_ extends $_ { ..$_ }" =>
          Class(ast.symbol, methods(ast), fields(ast)) :: acc
        case q"$_ object $_ extends $_ with $_ { ..$_ }" =>
          Class(ast.symbol, methods(ast), fields(ast)) :: acc
        case q"package object $_ extends $_" =>
          Class(ast.symbol, Set.empty, Set.empty) :: acc
        case q"package object $_ extends $_ { ..$_ }" =>
          Class(ast.symbol, methods(ast), fields(ast)) :: acc
        case q"package object $_ extends $_ with $_ { ..$_ }" =>
          Class(ast.symbol, methods(ast), fields(ast)) :: acc        
        case _ =>
          acc
      }
    }

    val q"package $_ { ..$topStmts }" = p
    val cs : List[Class] = topStmts.foldLeft(List.empty[Class])(f)
    cs.toSet
  }

  def packages(unit : CompilationUnit) : Set[Package] = {
    val topLevelPackage = unit.body
    Set(Package(topLevelPackage.symbol, classes(topLevelPackage)))
  }

  def apply(unit : CompilationUnit) : ProgramGraph = {
    new ProgramGraph(packages(unit))
  }

  def empty : ProgramGraph = new ProgramGraph(Set.empty[Package])
}
