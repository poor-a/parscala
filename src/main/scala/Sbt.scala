import java.nio.file.{Path, Files}

object Sbt {
  def isSbtProject(dir : Path) : Boolean =
    Files.isDirectory(dir) && Files.list(dir).anyMatch(p => p.getFileName().toString().endsWith(".sbt"))

  def loadProject(dir : Path) : (List[Path], List[Path]) = {
    val sbtLaunchConfig : java.net.URL = ClassLoader.getSystemClassLoader().getResource("sbt/sbt.boot.properties")
    if (sbtLaunchConfig != null) {
      val projectDir : java.io.File = dir.toFile()
      val app : xsbti.AppProvider = xsbt.boot.Launcher.getAppProvider(projectDir, sbtLaunchConfig)
      val config : xsbt.boot.AppConfiguration = new xsbt.boot.AppConfiguration(Array[String](), projectDir, app)
      val st : sbt.State = sbt.StandardMain.initialState(
        config,
        Seq(sbt.BuiltinCommands.defaults, sbt.BasicCommands.early),
        Seq(sbt.BasicCommandStrings.runEarly(sbt.internal.CommandStrings.DefaultsCommand),
            sbt.BasicCommandStrings.runEarly(sbt.internal.CommandStrings.InitCommand),
            sbt.internal.CommandStrings.LoadProject)
      )
      val loaded : sbt.State = parscala.Control.until[sbt.State](_.remainingCommands.isEmpty, sbt.MainLoop.next, st)
      val project : sbt.Extracted = sbt.Project.extract(loaded)
      val sources : Option[sbt.Result[Seq[java.io.File]]] =
        sbt.EvaluateTask(
          project.structure
        , sbt.Keys.sources.in(sbt.ConfigKey.configurationToKey(sbt.librarymanagement.syntax.Compile)).scopedKey
        , loaded
        , project.currentRef
        ).map(_._2)
      lazy val classpath : Option[sbt.Result[sbt.Def.Classpath]] =
        sbt.EvaluateTask(
          project.structure
        , sbt.Keys.dependencyClasspath.in(sbt.ConfigKey.configurationToKey(sbt.librarymanagement.syntax.Compile)).scopedKey
        , loaded
        , project.currentRef
        ).map(_._2)
      val optionInstance : scalaz.Applicative[Option] = scalaz.std.option.optionInstance
      optionInstance.tuple2(sources, classpath) match {
        case Some((sbt.Value(src), sbt.Value(cp))) => (src.map(_.toPath).toList, cp.map(_.data.toPath).toList)
        case _ => (List(), List())
      }
    } else {
      (List(), List())
    }
  }
}
