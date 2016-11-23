package parscala
package tree

/**
 * Useful method over syntax trees.
 */
object Control {
  /**
   * Catamorphism over expression syntax trees.
   * 
   * Pattern matches on t and passes the arguments of the
   * data constructor to one of the function arguments.
   * 
   * @tparam A the common result type of the function arguments
   * @param tuple handles the components of a tuple
   * @param newE handles the early definitions, parents and list of body statements of a 'new' expression
   * @param sel handles a selection of a field or method of an object
   * @param app handles the applied method and its actual parameter lists
   * @param ass handles the left and right side of an assignment
   * @param idDef handles the modifiers, identifier and right side of an identifier definition (val or var)
   * @return the common type A
   */
  def exprCata[A](tuple : List[Tree] => A,
                  newE : (List[Tree], List[Tree], List[Tree]) => A,
                  sel : (Tree, TermName) => A,
                  app : (Tree, List[List[Tree]]) => A,
                  ass : (Tree, Tree) => A,
                  idDef : (Modifiers, TermName, Tree) => A,
                  other : Tree => A,
                  t : Tree) : A = {
    import compiler.Quasiquote

    t match {
      case q"(..$components)" if components.size >= 2 => tuple(components)
      case q"new { ..$earlydefns } with ..$parents { $self => ..$stats }" => newE(earlydefns, parents, stats)
      case q"$expr.$tname" => sel(expr, tname)
      case q"$expr(...$args)" if t.isInstanceOf[compiler.Apply] => app(expr, args)
      case q"$lexpr = $rexpr" => ass(lexpr, rexpr)
      case q"$mods val $name = $expr" => idDef(mods, name, expr)
      case q"$mods var $name = $expr" => idDef(mods, name, expr)
      case _ => other(t)
    }
  }
}
