package parscala

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
}
