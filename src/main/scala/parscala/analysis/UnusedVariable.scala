package parscala
package analysis

import parscala.tree.{Expr, Defn, Decl, Statement}

object UnusedVariablesAnalysis {
  private val nothing : (Map[Symbol, DLabel], Set[Symbol], Set[Symbol]) = (Map(), Set(), Set())

  def analyse(m : Defn.Method) : Set[DLabel] = {
    def mappend(x1 : (Map[Symbol, DLabel], Set[Symbol], Set[Symbol]), x2 : (Map[Symbol, DLabel], Set[Symbol], Set[Symbol])) : (Map[Symbol, DLabel], Set[Symbol], Set[Symbol]) =
      (x1, x2) match {
        case ((symbols1, defined1, used1), (symbols2, defined2, used2)) => (symbols1 ++ symbols2, defined1 union defined2, used1 union used2)
      }

    def mconcat(l : List[(Map[Symbol, DLabel], Set[Symbol], Set[Symbol])]) : (Map[Symbol, DLabel], Set[Symbol], Set[Symbol]) =
      l.foldLeft(nothing)(mappend)

    def traverse(e : Expr) : (Map[Symbol, DLabel], Set[Symbol], Set[Symbol]) = {
      Expr.cata(
          (_, _, _) => // literal
            nothing
        , (_, _, used, _) => // identifier reference
            (Map(), Set(), used.toSet)
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
        , (_, obj, _, _) => // selection
            traverse(obj)
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
        , e
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

  private def collectVarDecls(d : Decl) : (Map[Symbol, DLabel], Set[Symbol], Set[Symbol]) =
    Decl.cata(
        (l, _, symbols) => // val
          (symbols.map((_, l)).toMap, symbols.toSet, Set())
      , (l, _, symbols) => // var
          (symbols.map((_, l)).toMap, symbols.toSet, Set())
      , (_, _, _, _) => // method 
          nothing
      , (_, _, _, _, _) => // type
          nothing
      , (_, _) => // import
          nothing
      , d
      )

  private def collectVarDefns(d : Defn) : (Map[Symbol, DLabel], Set[Symbol], Set[Symbol]) =
    Defn.cata(
        (l, _, symbols, _, _) => // val
          (symbols.map((_, l)).toMap, symbols.toSet, Set())
      , (l, _, symbols, _, _) => // var
          (symbols.map((_, l)).toMap, symbols.toSet, Set())
      , (_, _, _, _, _, _) => // method
          nothing
      , (_, _, _, _, _) => // type
          nothing
      , (_, _, _, _, _) => // macro
          nothing
      , (_, _, _, _, _) => // secondary constructor
          nothing
      , (_, _, _, _) => // class
          nothing
      , (_, _, _, _) => // trait
          nothing
      , (_, _, _, _) => // object
          nothing
      , (_, _, _, _) => // package object
          nothing
      , (_, _, _, _) => // package
          nothing
      , d
      )
}
