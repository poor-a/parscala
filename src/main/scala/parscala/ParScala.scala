package parscala

import parscala.{tree => tr}

import java.nio
import scala.meta

import scalaz.{Monad, \/-, -\/}

object ParScala {
  def analyse(pathes : List[nio.file.Path], classPath : Option[String]) : Either[String, (ProgramGraph, Option[String])] = {
    scalaz.std.option.cata(classPath)(
        cp => scalac.currentSettings.classpath.value = cp
      , ()
      )

    val desugaredAsts : List[Tree] = typeCheck(pathes.map(_.toString))
    val sugaredSources : List[meta.Parsed[meta.Source]] = pathes.map(parseMeta)
    val trees : List[(Tree, meta.Source)] = desugaredAsts.zip(sugaredSources).flatMap { case (desugared, parseResult) =>
      parseResult.fold(
          _ => List()
        , source =>
            List((desugared, source))
        )
    }
    val m : Monad[tr.Expr.NodeGen] = tr.Expr.nodeGenMonadInstance
    val genDecls : tr.Expr.NodeGen[Unit] =
      m.void(m.sequence(trees.toList.map{ case (desugared, sugared) => tr.Expr.resugar(sugared, desugared) } )
                        (Scalaz.listTraverseInst)
            )
    tr.Expr.runNodeGen(genDecls) match {
      case \/-((log, (st, _))) =>
        val pgraph : ProgramGraph = new ProgramGraph(st.decls, st.defns, st.exprs, st.symbolTable, foreignSymbols(st), st.topLevels, st.callTargets)
        if (!log.isEmpty) {
          val l : String = log.mkString("\n")
          println(s"During the AST building, we noticed the following:\n$l")
          Right((pgraph, Some(l)))
        } else {
          Right((pgraph, None))
        }
      case -\/(err) =>
        Left(err)
    }
  }

  def parseMeta(path : nio.file.Path) : meta.Parsed[meta.Source] =
    // parsing again does not work with Input.File, as if scalameta caches input length
    meta.parsers.Parse.parseSource(meta.inputs.Input.Stream(java.nio.file.Files.newInputStream(path), java.nio.charset.Charset.forName("UTF-8")), meta.dialects.Scala212)

  def typeCheck(pathes : List[String]) : List[Tree] = {
    val run = new scalac.Run()
    run.compile(pathes)
    run.units.map(_.body).toList
  }

  private def foreignSymbols(st : tr.Expr.St) : Map[DLabel, Symbol] = {
    val known : Set[DLabel] = st.decls.keySet union st.defns.keySet
    st.symbolTable.toIterator.filterNot{case (sym @ _, l) => known.contains(l)}.map(_.swap).toMap
  }

  def astOfExprWithSource(expr : String) : Option[(Tree, SourceFile)] = {
    val freshGen = scalac.currentFreshNameCreator
    val packageName : TermName = scalac.freshTermName("p")(freshGen)
    val objectName : TermName = scalac.freshTermName("o")(freshGen)
    val funName : TermName = scalac.freshTermName("f")(freshGen)
    val code : String = "package %s { object %s { def %s : Any = { %s } } }".format( packageName
                                                                                   , objectName
                                                                                   , funName
                                                                                   , expr
                                                                                   )
    val source : SourceFile = scalac.newSourceFile(code)
    val r : scalac.Run = new scalac.Run // todo: reset reporter
    r.compileSources(List(source))
    val units : Iterator[CompilationUnit] = r.units
    if (units.nonEmpty) {
      // import scalac.Quasiquote
      // val q"package $_ { object $_ { def $_(...$_) : $_ = $body } }" = units.next().body // Generates many unused pattern var warnings
      val scalac.PackageDef(_, List(scalac.ModuleDef(_, _, scalac.Template(_, _, List(scalac.DefDef(_, _, _, _, _, body)))))) = units.next().body
      Some((body, source))
    } else {
      None
    }
  }

  def astOfExpr : String => Option[Tree] = (astOfExprWithSource _) andThen (_.map(_._1))

  def astOfClassWithSource(cls : String) : Option[(Tree, SourceFile)] = {
    val freshGen = scalac.currentFreshNameCreator
    val packageName : TermName = scalac.freshTermName("p")(freshGen)
    val code : String = "package %s { %s }".format(packageName, cls)
    val source : SourceFile = scalac.newSourceFile(code)
    val r : scalac.Run = new scalac.Run
    r.compileSources(List(source))
    val units : Iterator[CompilationUnit] = r.units
    if (units.nonEmpty) {
      // import scalac.Quasiquote
      // val q"package $_ { $clsAst }" = units.next().body // Generates many unused pattern var warnings
      val scalac.PackageDef(_, List(clsAst)) = units.next().body
      Some((clsAst, source))
    } else {
      None
    }
  }

  def astOfClass : String => Option[Tree] = (astOfClassWithSource _) andThen (_.map(_._1))
}
