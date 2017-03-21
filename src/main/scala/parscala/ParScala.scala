package parscala

import scala.reflect.macros.whitebox.Context
import scala.language.experimental.macros

object ParScala {
  def analyse(pathes : List[String], classPath : Option[String]) : ProgramGraph = {
    scalaz.std.option.cata(classPath)(
        cp => compiler.currentSettings.classpath.value = cp
      , ()
      )
    val run = new compiler.Run()
    run.compile(pathes)
    run.units.map{u : CompilationUnit => ProgramGraph(u)}.foldLeft(ProgramGraph.empty)((x : ProgramGraph, y : ProgramGraph) => x <+> y)
  }

  def astOfExpr(expr : String) : Tree = macro astMacro

  def astMacro(c : Context)(expr : c.Expr[String]) : Tree = {
    import parscala.compiler.Quasiquote
//    val s : String = c.eval(expr)
//    val tr : c.Tree = c.parse("def f = 2")
//    val t : c.Tree = c.typecheck(q"package p { object o { def f = $tr } }")
//    c.typecheck(tr)
//    tr
    q"asd"
  }
}
