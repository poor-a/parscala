import scala.tools.nsc.Global

import scala.tools.nsc.{Settings, Global}

package object parscala {
  private val settings : Settings = new Settings()
  settings.stopAfter.value = List("typer")
  settings.embeddedDefaults[ParScala.type]
  val compiler : Global = new Global(settings)

  type Tree = compiler.Tree
  type Name = compiler.Name
  type CompilationUnit = compiler.CompilationUnit
}
