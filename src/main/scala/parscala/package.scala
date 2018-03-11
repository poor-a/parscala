import scala.tools.nsc.{Settings, Global}

/**
 * Core types and values for ParScala.
 */
package object parscala {
  private val settings : Settings = new Settings
  settings.embeddedDefaults[ParScala.type]
  settings.Yrangepos.value = true
  settings.stopAfter.value = List("typer")

  /**
   * The Scala compiler.
   */
  val scalac : Global = new Global(settings)

  type Tree = scalac.Tree
  type Constant = scalac.Constant
  type Name = scalac.Name
  type TermName = scalac.TermName
  type TypeName = scalac.TypeName
  type Symbol = scalac.Symbol
  type Modifiers = scalac.Modifiers
  type CompilationUnit = scalac.CompilationUnit
  type SourceFile = scala.reflect.internal.util.SourceFile

  type BLabelGen = Stream[BLabel]
  type SLabelGen = Stream[SLabel]
  type PLabelGen = Stream[PLabel]
  type DLabelGen = Stream[DLabel]
  type ExprMap = Map[SLabel, tree.Expr]
  type DefnMap = Map[DLabel, tree.Defn]
  type DeclMap = Map[DLabel, tree.Decl]
  type SymMap[A] = Map[Symbol, A]
  type SymbolTable = SymMap[DLabel]

  type MLabel = Either[DLabel, SLabel]
}
