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
    val r : compiler.Run = new compiler.Run
    r.compileSources(List(source))
    val units : Iterator[CompilationUnit] = r.units
    if (units.nonEmpty) {
	val q"package $_ { object $_ { def $_(...$_) : $_ = $body } }" = units.next().body
	Some((body, source))
    } else
	None
  }

  def astOfExpr : String => Option[Tree] = (astOfExprWithSource _) andThen (_.map(_._1))

  def astOfClass(cls : String) : Tree = {
    import compiler.Quasiquote

    val freshGen = compiler.currentFreshNameCreator
    val packageName : TermName = compiler.freshTermName("p")(freshGen)
    val source : String = "package %s { %s }".format( packageName
                                                    , cls
                                                    )
    val dummyUnit : CompilationUnit = compiler.newCompilationUnit(source)
    val r : compiler.Run = new compiler.Run
    r.compileUnits(List(dummyUnit), r.parserPhase)
    val q"package $_ { $clsAst }" = dummyUnit.body
    clsAst
  }
}
