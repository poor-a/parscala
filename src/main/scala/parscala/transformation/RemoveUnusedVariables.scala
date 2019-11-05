package parscala
package transformation

import parscala.analysis.UnusedVariablesAnalysis
import parscala.Control.catSomes
import parscala.tree.{Expr, Defn, Block, Statement}

object RemoveUnusedVariables {
  // TODO: review ProgramGraph bookkeeping -- symbolTable, declarations
  def in(m : Defn.Method, pg : ProgramGraph) : ProgramGraph = {
    val unused : Set[DLabel] = UnusedVariablesAnalysis.analyse(m)
    val transformed : Defn.Method = m.copy(body = removeVarDefnDecl(unused, m.body))
    maintainAst(transformed, new ProgramGraph(pg.declarations, unused.foldLeft(pg.definitions.updated(m.label, transformed))((acc, label) => acc - label), pg.expressions, pg.symbolTable, pg.foreignSymbols, pg.topLevels, pg.callTargets))
  }

  private def maintainAst(d : Defn, pg : ProgramGraph) : ProgramGraph = {
    def replace[A](l : List[A], labelOf : A => Option[DLabel], lift : Defn => A, to : Defn) : List[A] =
      l match {
        case List() => List()
        case x :: xs =>
          if (labelOf(x) == Some(to.label))
            lift(to) :: xs
          else
            x :: replace(xs, labelOf, lift, to)
      }

    Defn.symbols(d) match {
      case s :: _ =>
        pg.symbolTable.get(s.owner).flatMap(pg.definitions.get(_)) match {
          case Some(parent) =>
            val updated : Option[Defn] = Defn.cata(
                (_, _, _, _, _) => // val
                  None
              , (_, _, _, _, _) => // var
                  None
              , (_, _, _, _, _, _) => // method
                  None
              , (_, _, _, _, _) => // type
                  None
              , (_, _, _, _, _) => // macro
                  None
              , (_, _, _, _, _) => // secondary constructor
                  None
              , (l, symbols, name, statements) => { // class
                  Some(Defn.Class(l,
                                  symbols, 
                                  name, 
                                  replace[Statement](statements, stmt => stmt.fold(decl => Some(decl.label), defn => Some(defn.label), expr => None), Statement.fromDefn, d)))
                }
              , (l, symbols, name, statements) => // trait
                  Some(Defn.Trait(l,
                                  symbols,
                                  name,
                                  replace[Statement](statements, stmt => stmt.fold(decl => Some(decl.label), defn => Some(defn.label), expr => None), Statement.fromDefn, d)))
              , (l, symbols, name, statements) => // object
                  Some(Defn.Object(l,
                                   symbols,
                                   name,
                                   replace[Statement](statements, stmt => stmt.fold(decl => Some(decl.label), defn => Some(defn.label), expr => None), Statement.fromDefn, d)))
              , (l, symbols, name, statements) => // package object
                  Some(Defn.PackageObject(l,
                                          symbols,
                                          name,
                                          replace[Statement](statements, stmt => stmt.fold(decl => Some(decl.label), defn => Some(defn.label), expr => None), Statement.fromDefn, d)))
              , (l, symbols, name, statements) => // package
                  Some(Defn.Package(l,
                                    symbols,
                                    name,
                                    replace[Statement](statements, stmt => stmt.fold(decl => Some(decl.label), defn => Some(defn.label), expr => None), Statement.fromDefn, d)))
              , parent
              )
            updated match {
              case Some(defn) =>
                maintainAst(defn,
                            pg.copy(definitions = pg.definitions.updated(defn.label, defn),
                                    topLevels = if (Defn.isTopLevel(defn))
                                        replace[Either[tree.Decl, tree.Defn]](pg.topLevels, d => Some(d.fold(_.label, _.label)), Right(_), defn)
                                      else
                                        pg.topLevels
                                   )
                           )
              case None => pg
            }
          case None => pg
        }
      case List() => pg
    }
  }

  private def removeVarDefnDecl(vars : Set[DLabel], e : Expr) : Expr = {
    Expr.cata(
          (_, _, _) => // literal
            e
        , (_, _, _, _) => // identifier reference
            e
        , (_, _, _, _) => // assignment
            e
        , (_, _, _, _) => // application
            e
        , (_, _, _, _, _) => // infix application
            e
        , (_, _, _, _) => // unary application
            e
        , (_, _, _, _) => // new
            e
        , (_, _, _, _) => // selection
            e
        , (_, _, _) => // this
            e
        , (_, _, _, _) => // super
            e
        , (_, _, _) => // tuple
            e
        , (_, _, _, _) => // if-then
            e
        , (_, _, _, _, _) => // if-then-else
            e
        , (_, _, _, _) => // while loop
            e
        , (_, _, _, _) => // for loop
            e
        , (_, _, _, _) => // for-yield loop
            e
        , (_, _) => // return
            e
        , (_, _, _) => // return with expr
            e
        , (_, _, _) => // throw
            e
        , (l, stmts, typ) => { // block
            val filtered = catSomes(stmts.map(_.fold(
                decl => if (!(vars contains decl.label)) Some(Statement.fromDecl(decl)) else None
              , defn => if (!(vars contains defn.label)) Some(Statement.fromDefn(defn)) else None
              , e => Some(Statement.fromExpr(removeVarDefnDecl(vars, e)))
              )))
            Block(l, filtered, typ)
          }
        , (_, _, _) => // other expression
            e
        , e
        )
  }
}