package parscala

object ParScala {
  def analyse(pathes : List[String]) : ProgramGraph = {
    val run = new compiler.Run()
    run.compile(pathes)
    run.units.map{u : CompilationUnit => ProgramGraph(u)}.foldLeft(ProgramGraph.empty)((x : ProgramGraph, y : ProgramGraph) => x <+> y)
  }
}
