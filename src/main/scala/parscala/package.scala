import scala.tools.nsc.{Settings, Global}

/**
 * Core types and values for ParScala.
 */
package object parscala {
  private val settings : Settings = new Settings
  settings.embeddedDefaults[ParScala.type]
  settings.stopAfter.value = List("typer")

  /**
   * The Scala compiler.
   */
  val compiler : Global = new Global(settings)

  type Tree = compiler.Tree
  type Constant = compiler.Constant
  type Name = compiler.Name
  type TermName = compiler.TermName
  type TypeName = compiler.TypeName
  type Symbol = compiler.Symbol
  type Modifiers = compiler.Modifiers
  type CompilationUnit = compiler.CompilationUnit
  type SourceFile = scala.reflect.internal.util.SourceFile

  type BLabelGen = Stream[BLabel]
  type SLabelGen = Stream[SLabel]
  type PLabelGen = Stream[PLabel]
  type DLabelGen = Stream[DLabel]
  type ExprMap = Map[SLabel, tree.Node]
  type DefnMap = Map[DLabel, tree.Defn]
  type DeclMap = Map[DLabel, tree.Decl]
  type SymMap[A] = Map[Symbol, A]
  type SymbolTable = SymMap[DLabel]
}
