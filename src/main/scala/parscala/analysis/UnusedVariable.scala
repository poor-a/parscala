package parscala
package analysis

import parscala.tree.{Defn, TypedExpr, TypedDecl, TypedDefn}

object UnusedVariablesAnalysis {
  type AnalysisValue = (Map[Symbol, DLabel], Set[Symbol], Set[Symbol]) // TODO better name
  private val nothing : AnalysisValue = (Map(), Set(), Set())

  def analyse(m : Defn.TypedMethod) : Set[DLabel] = {
    def mappend(x1 : AnalysisValue, x2 : AnalysisValue) : AnalysisValue =
      (x1, x2) match {
        case ((symbols1, defined1, used1), (symbols2, defined2, used2)) => (symbols1 ++ symbols2, defined1 union defined2, used1 union used2)
      }

    def mconcat(l : List[AnalysisValue]) : AnalysisValue =
      l.foldLeft(nothing)(mappend)

    def traverse(e : TypedExpr) : AnalysisValue = {
      e.cata(
          (_, _, _) => // literal
            nothing
        , (_, _, used, _) => // identifier reference
            (Map(), Set(), Set(used))
        , (_, _, rhs, _) => // assignment
            traverse(rhs)
        , (_, m, args, _) => // application
            mconcat((m :: args).map(traverse))
        , (_, lhs, _, args, _) => // infix application
            mconcat((lhs :: args).map(traverse))
        , (_, _, arg, _) => // unary application
            traverse(arg)
        , (_, _, argss, _) => // new
            mconcat(for (args <- argss; arg <- args) yield traverse(arg))
        , (_, obj, _, _, _) => // selection
            traverse(obj)
        , (_, argss) => // this(...) application
            mconcat(for (args <- argss; arg <- args) yield traverse(arg))
        , (_, _, _) => // this
            nothing
        , (_, _, _, _) => // super
            nothing
        , (_, comps, _) => // tuple
            mconcat(comps.map(traverse))
        , (_, pred, thenE, _) => // if-then
            mappend(traverse(pred), traverse(thenE))
        , (_, pred, thenE, elseE, _) => // if-then-else
            mappend(traverse(pred), mappend(traverse(thenE), traverse(elseE)))
        , (_, pred, body, _) => // while loop
            mappend(traverse(pred), traverse(body))
        , (_, _, body, _) => // for loop
            traverse(body) // TODO: traverse enums
        , (_, _, body, _) => // for-yield loop
            traverse(body)
        , (_, _) => // return
            nothing
        , (_, expr, _) => // return with expr
            traverse(expr)
        , (_, expr, _) => // throw
            traverse(expr)
        , (_, stmts, _) => // block
            mconcat(stmts.map(_.fold(collectVarDecls, collectVarDefns, traverse)))
        , (_, _, _) => // other expression
            nothing
        )
    }

    val (symbols, defined, used) = traverse(m.body)
    val unused : Set[Symbol] = defined diff used
    unused.foldLeft(Set[DLabel]())((acc, sym) =>
      symbols.get(sym) match {
        case Some(l) => acc + l
        case _ => acc
      }
    )
  }

  private def collectVarDecls(d : TypedDecl) : AnalysisValue =
    d.cata(
        (l, _, _, symbols, _) => // val
          (symbols.map((_, l)).toMap, symbols.toSet, Set())
      , (l, _, _, symbols, _) => // var
          (symbols.map((_, l)).toMap, symbols.toSet, Set())
      , (_, _, _, _, _, _, _) => // method
          nothing
      , (_, _, _, _, _, _) => // type
          nothing
      , (_, _) => // import
          nothing
      )

  private def collectVarDefns(d : TypedDefn) : AnalysisValue =
    d.cata(
        (l, _, _, symbols, _, _) => // val
          (symbols.map((_, l)).toMap, symbols.toSet, Set())
      , (l, _, _, symbols, _, _) => // var
          (symbols.map((_, l)).toMap, symbols.toSet, Set())
      , (_, _, _, _, _, _, _, _) => // method
          nothing
      , (_, _, _, _, _, _) => // type
          nothing
      , (_, _, _, _, _, _) => // macro
          nothing
      , (_, _, _, _, _, _) => // secondary constructor
          nothing
      , (_, _, _, _, _) => // class
          nothing
      , (_, _, _, _, _) => // trait
          nothing
      , (_, _, _, _, _) => // object
          nothing
      , (_, _, _, _, _) => // package object
          nothing
      , (_, _, _, _) => // package
          nothing
      )
}
