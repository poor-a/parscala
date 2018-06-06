package parscala

import parscala.{tree => tr}

import java.nio
import org.langmeta.inputs
import scala.meta

import scalaz.{Monad, \/-, -\/}

object ParScala {
  def analyse(pathes : List[nio.file.Path], classPath : Option[String]) : (ProgramGraph, Option[String]) = {
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
        if (!log.isEmpty) {
          val l : String = log.mkString("\n")
          println(s"During the AST building, we noticed the following:\n$l")
        }
        (new ProgramGraph(st.decls, st.defns, st.exprs, st.symbols, st.topLevels, st.callTargets), None)
      case -\/(err) =>
        (new ProgramGraph(Map(), Map(), Map(), Map(), List(), Map()), Some(err))
    }
  }

  def parseMeta(path : nio.file.Path) : meta.Parsed[meta.Source] =
    // parsing again does not work with Input.File, as if scalameta caches input length
    meta.parsers.Parse.parseSource(inputs.Input.Stream(java.nio.file.Files.newInputStream(path), java.nio.charset.Charset.forName("UTF-8")), meta.dialects.Scala212)

  def typeCheck(pathes : List[String]) : List[Tree] = {
    val run = new scalac.Run()
    run.compile(pathes)
    run.units.map(_.body).toList
  }


  def astOfExprWithSource(expr : String) : Option[(Tree, SourceFile)] = {
    import scalac.Quasiquote

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
      val q"package $_ { object $_ { def $_(...$_) : $_ = $body } }" = units.next().body
      Some((body, source))
    } else {
      None
    }
  }

  def astOfExpr : String => Option[Tree] = (astOfExprWithSource _) andThen (_.map(_._1))

  def astOfClassWithSource(cls : String) : Option[(Tree, SourceFile)] = {
    import scalac.Quasiquote

    val freshGen = scalac.currentFreshNameCreator
    val packageName : TermName = scalac.freshTermName("p")(freshGen)
    val code : String = "package %s { %s }".format(packageName, cls)
    val source : SourceFile = scalac.newSourceFile(code)
    val r : scalac.Run = new scalac.Run
    r.compileSources(List(source))
    val units : Iterator[CompilationUnit] = r.units
    if (units.nonEmpty) {
      val q"package $_ { $clsAst }" = units.next().body
      Some((clsAst, source))
    } else {
      None
    }
  }

  def astOfClass : String => Option[Tree] = (astOfClassWithSource _) andThen (_.map(_._1))
}
