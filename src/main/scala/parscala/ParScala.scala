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

  def astOfExpr(expr : String) : Tree = {
    import compiler.Quasiquote

    val freshGen = compiler.currentFreshNameCreator
    val packageName : TermName = compiler.freshTermName("p")(freshGen)
    val objectName : TermName = compiler.freshTermName("o")(freshGen)
    val funName : TermName = compiler.freshTermName("f")(freshGen)
    val source : String = "package %s { object %s { def %s = { %s } } }".format( packageName
                                                                               , objectName
                                                                               , funName
                                                                               , expr
                                                                               )
    val dummyUnit : CompilationUnit = compiler.newCompilationUnit(source)
    val r : compiler.Run = new compiler.Run
    r.compileUnits(List(dummyUnit), r.parserPhase)
    val q"package $_ { object $_ { def $_(...$_) = $body } }" = dummyUnit.body
    body
  }
}
