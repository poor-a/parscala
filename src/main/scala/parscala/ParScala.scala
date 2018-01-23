package parscala

import parscala.{tree => tr}

import java.nio
import org.langmeta.inputs
import scala.meta

import scalaz.{Monad, \/-, -\/}

object ParScala {
  def analyse(pathes : List[nio.file.Path], classPath : Option[String]) : Either[String, ProgramGraph] = {
    scalaz.std.option.cata(classPath)(
        cp => compiler.currentSettings.classpath.value = cp
      , ()
      )
    val run = new compiler.Run()
    run.compile(pathes.map(_.toString))

    val desugaredAsts : Iterator[Tree] = run.units.map(_.body)
    val sugaredSources : Iterator[meta.Parsed[meta.Source]] = pathes.toIterator.map(parseMeta)
    val trees : Iterator[(Tree, meta.Source)] = desugaredAsts.zip(sugaredSources).flatMap { case (desugared, parseResult) =>
      parseResult.fold(
          _ => List()
        , source =>
            List((desugared, source))
        )
    }
    val m : Monad[tr.Node.NodeGen] = tr.Node.nodeGenMonadInstance
    val genDecls : tr.Node.NodeGen[Unit] = 
      m.void(m.sequence(trees.toList.map{ case (desugared, sugared) => tr.Node.resugar(sugared, desugared) } )
                        (Scalaz.listTraverseInst)
            )
    tr.Node.runNodeGen(genDecls) match {
      case \/-((st, _)) =>
        Right(new ProgramGraph(st.decls, st.exprs, st.symbols, st.packages))
      case -\/(err) =>
        Left(err)
    }
  }

  def parseMeta(path : nio.file.Path) : meta.Parsed[meta.Source] =
    meta.parsers.Parse.parseSource(inputs.Input.File(path), meta.dialects.Scala212)

  def astOfExprWithSource(expr : String) : Option[(Tree, SourceFile)] = {
    import compiler.Quasiquote

    val freshGen = compiler.currentFreshNameCreator
    val packageName : TermName = compiler.freshTermName("p")(freshGen)
    val objectName : TermName = compiler.freshTermName("o")(freshGen)
    val funName : TermName = compiler.freshTermName("f")(freshGen)
    val code : String = "package %s { object %s { def %s : Any = { %s } } }".format( packageName
                                                                                   , objectName
                                                                                   , funName
                                                                                   , expr
                                                                                   )
    val source : SourceFile = compiler.newSourceFile(code)
    val r : compiler.Run = new compiler.Run // todo: reset reporter
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
    import compiler.Quasiquote

    val freshGen = compiler.currentFreshNameCreator
    val packageName : TermName = compiler.freshTermName("p")(freshGen)
    val code : String = "package %s { %s }".format(packageName, cls)
    val source : SourceFile = compiler.newSourceFile(code)
    val r : compiler.Run = new compiler.Run
    r.compileSources(List(source))
    val units : Iterator[CompilationUnit] = r.units
    if (units.nonEmpty) {
      val body : Tree = units.next().body
      print("body is a " + body.getClass)
      val q"package $_ { $clsAst }" = body
      Some((clsAst, source))
    } else {
      None
    }
  }

  def astOfClass : String => Option[Tree] = (astOfClassWithSource _) andThen (_.map(_._1))
}
