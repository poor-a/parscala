package parscala

object ParScala {
  def analyse(pathes : List[String]) : Iterator[CompilationUnit] = {
    val run = new compiler.Run()
    run.compile(pathes)
    run.units
  }
}
