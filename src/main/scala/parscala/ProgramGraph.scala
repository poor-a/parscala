package parscala

import parscala.Control.{forM, mapM_}
import parscala.dot.{DotNode, DotEdge, DotGen, DotGraph}
//import callgraph.{CallGraph,CallGraphBuilder}

import scalaz.Either3

case class ProgramGraph (
    val declarations : tree.TypedDeclMap
  , val definitions : tree.TypedDefnMap
  , val expressions : tree.TypedExprMap
  , val symbolTable : tree.SymbolTable
  , val foreignSymbols : Map[DLabel, Symbol]
  , val topLevels : List[Either[tree.TypedDecl, tree.TypedDefn]]
  , val callTargets : Map[SLabel, List[Either[DLabel, SLabel]]]
  ) {
  def lookupDeclDefn(l : DLabel) : Option[Either3[tree.TypedDecl, tree.TypedDefn, Symbol]] =
    declarations.get(l).map(Either3.left3[tree.TypedDecl, tree.TypedDefn, Symbol](_)) orElse
    definitions.get(l).map(Either3.middle3[tree.TypedDecl, tree.TypedDefn, Symbol](_)) orElse
    foreignSymbols.get(l).map(Either3.right3[tree.TypedDecl, tree.TypedDefn, Symbol](_))

  def classes : List[tree.Defn.TypedClass] = parscala.Control.catSomes(
      topLevels map (_.fold(
          (decl : tree.TypedDecl) => None
        , (defn : tree.TypedDefn) => defn.asClass
        )
      )
    )

  def objects : List[tree.Defn.TypedObject] = parscala.Control.catSomes(
      topLevels map (_.fold(
          (decl : tree.TypedDecl) => None
        , (defn : tree.TypedDefn) => defn.asObject
        )
      )
    )

  def toDot : dot.DotGraph = {
    val (nodes, edges) : (List[DotNode], List[DotEdge]) = DotGen.exec(
      for (nodes <- forM(topLevels)(
              (declOrDefn : Either[tree.TypedDecl, tree.TypedDefn]) =>
                declOrDefn.fold(
                    (decl : tree.TypedDecl) => tree.Decl.toDotGen(decl)
                  , (defn : tree.TypedDefn) => tree.Defn.toDotGen(defn)
                  )
            );
          root <- DotGen.node(DotNode.record("root", "Root", "")(scalaz.std.string.stringInstance));
          _ <- mapM_((n : DotNode) => DotGen.edge(root, n, ""), nodes))
      yield ()
    )

    DotGraph("program_graph", nodes, edges)
  }
}

/*
class ProgramGraph_(val packages : Set[Package_]) {
  def <+> (g : ProgramGraph_) : ProgramGraph_ = {
    val common  = packages intersect g.packages
    val updated = common map (p => p <+> g.packages.find(_ == p).get)
    new ProgramGraph_(updated ++ packages ++ g.packages)
  }

  def methods : Set[Method_] =
    (packages.toIterator flatMap (_.classes) flatMap (_.methods)).toSet

  def findMethod(s : Symbol) : Option[Method_] = {
    methods find (_.symbol == s)
  }

  def callGraph : (CallGraph, ProgramGraph_) = {
    val cs : Iterator[CallGraph] = methods.toIterator map (CallGraphBuilder.getCalls(_))
    val g : CallGraph = cs.foldLeft(CallGraphBuilder.empty)(_ ++ _)
    val g2 : CallGraph = methods.foldLeft(g){(acc,m) => acc.resolve(m)}
    val (ms, pg) = missingMethods(g2.unknown)
    (ms.foldLeft(g2){(acc,m) => acc.resolve(m)}, pg)
  }

  private def topLevelClass(ms : Set[Method_]) : Set[Class_] = {
    val groups : Map[Symbol, Set[Method_]] = ms groupBy (_.symbol.owner)
    groups.foldLeft(Set.empty[Class_]){(acc,g) => acc + Class_(g._1, g._2, Set.empty)}
  }

  def missingMethods(s : Set[Symbol]) : (Set[Method_],ProgramGraph_) = {
    val syms : Iterator[Symbol] = s.toIterator filter (_.isMethod)
    val methods : Set[Method_] = (syms map (Method_(_, None))).toSet
    val classes : Set[Class_] = topLevelClass(methods)
    val cGroups : Map[Symbol, Set[Class_]] = classes filter (_.symbol.owner.isPackage) groupBy (_.symbol.owner)
    val packages : Set[Package_] = cGroups.foldLeft(Set.empty[Package_]){(acc,g) => acc + Package_(g._1, g._2)}
    (methods, new ProgramGraph_(packages))
  }
}

object ProgramGraph_ {
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

    // ok
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

  def methods(c : Tree) : Set[Method_] = {
    def f(acc : List[Method_], ast : Tree) : List[Method_] = {
      ast match {
        case q"$_ def $_ (...$_) : $_ = $_" => 
          Method_(ast.symbol, Some(ast)) :: acc
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
    val ms : List[Method_] = defs.foldLeft(List.empty[Method_])(f)
    ms.toSet
  }

  def classes(p : Tree) : Set[Class_] = {
    def f (acc : List[Class_], ast : Tree) : List[Class_] = {
      ast match {
        case q"$_ class $_ extends $_" =>
          Class_(ast.symbol, Set.empty, Set.empty) :: acc
        case q"$_ class $_ extends $_ { ..$_ }" => 
          Class_(ast.symbol, methods(ast), fields(ast)) :: acc
        case q"$_ class $_ extends $_ with $_ { ..$_ }" =>
          Class_(ast.symbol, methods(ast), fields(ast)) :: acc
        case q"$_ object $_ extends $_" => 
          Class_(ast.symbol, Set.empty, Set.empty) :: acc
        case q"$_ object $_ extends $_ { ..$_ }" =>
          Class_(ast.symbol, methods(ast), fields(ast)) :: acc
        case q"$_ object $_ extends $_ with $_ { ..$_ }" =>
          Class_(ast.symbol, methods(ast), fields(ast)) :: acc
        case q"package object $_ extends $_" =>
          Class_(ast.symbol, Set.empty, Set.empty) :: acc
        case q"package object $_ extends $_ { ..$_ }" =>
          Class_(ast.symbol, methods(ast), fields(ast)) :: acc
        case q"package object $_ extends $_ with $_ { ..$_ }" =>
          Class_(ast.symbol, methods(ast), fields(ast)) :: acc        
        case _ =>
          acc
      }
    }

    val q"package $_ { ..$topStmts }" = p
    val cs : List[Class_] = topStmts.foldLeft(List.empty[Class_])(f)
    cs.toSet
  }

  def packages(unit : CompilationUnit) : Set[Package_] = {
    val topLevelPackage = unit.body
    Set(Package_(topLevelPackage.symbol, classes(topLevelPackage)))
  }

  def apply(unit : CompilationUnit) : ProgramGraph_ = {
    new ProgramGraph_(packages(unit))
  }

  def empty : ProgramGraph_ = new ProgramGraph_(Set.empty[Package_])
}
*/
