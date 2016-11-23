import scala.tools.nsc.{Settings, Global}

package object parscala {
  private val settings : Settings = new Settings
  settings.embeddedDefaults[ParScala.type]
  settings.stopAfter.value = List("typer")
  val compiler : Global = new Global(settings)

  type Tree = compiler.Tree
  type Name = compiler.Name
  type TermName = compiler.TermName
  type Symbol = compiler.Symbol
  type Modifiers = compiler.Modifiers
  type CompilationUnit = compiler.CompilationUnit

  type BLabelGen = Stream[BLabel]
  type SLabelGen = Stream[SLabel]
}
