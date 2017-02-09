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
   * @param returnUnit handles return statements without expressions
   * @param returnExpr handles return statements with expressions
   * @return the common type A
   */
  def exprCata[A](lit: Tree => A,
                  id : (Symbol) => A,
                  tuple : List[Tree] => A,
                  newE : (List[Tree], List[Tree], List[Tree]) => A,
                  thisE : TypeName => A,
                  sel : (Tree, TermName) => A,
                  app : (Tree, List[List[Tree]]) => A,
                  ifE : (Tree, Tree) => A,
                  ifElse : (Tree, Tree, Tree) => A,
                  whileE : (Tree, Tree) => A,
                  forE : (List[Tree], Tree) => A,
                  forYieldE : (List[Tree], Tree) => A,
                  ass : (Tree, Tree) => A,
                  idDef : (Modifiers, Symbol, Tree) => A,
                  returnUnit : () => A,
                  returnExpr : (Tree) => A,
                  block : (List[Tree]) => A,
                  other : Tree => A,
                  t : Tree) : A = {
    import compiler.Quasiquote

    t match {
      case _ if t.isInstanceOf[compiler.Literal] => lit(t)
      case _ if t.isInstanceOf[compiler.Ident] => id(t.symbol)
      case q"(..$components)" if components.size >= 2 => tuple(components)
      case q"new { ..$earlydefns } with ..$parents { $self => ..$stats }" => newE(earlydefns, parents, stats)
      case q"$expr.this" => thisE(expr)
      case q"$expr.$tname" => sel(expr, tname)
      case q"$expr(...$args)" if t.isInstanceOf[compiler.Apply] => app(expr, args)
      case q"if ($pred) $thenE" => ifE(pred, thenE)
      case q"if ($pred) $thenE else $elseE" => ifElse(pred, thenE, elseE)
      case q"while ($pred) $body" => whileE(pred, body)
      case q"for (..$enums) $body" => forE(enums, body)
      case q"for (..$enums) yield $body" => forYieldE(enums, body)
      case q"$lexpr = $rexpr" => ass(lexpr, rexpr)
      case q"$mods val $id = $expr" => idDef(mods, t.symbol, expr)
      case q"$mods var $id = $expr" => idDef(mods, t.symbol, expr)
      case q"return" => returnUnit()
      case q"return $expr" => returnExpr(expr)
//      case fq"$pat <- $expr" => gen(pat, expr)
//      case fq"if $pred" => guard(pred)
//      case fq"$pat = $expr" => forValDef(pat, expr)
      case _ if t.isInstanceOf[compiler.Block] =>
        val q"{ ..$stmts }" = t
        block(stmts)
      case _ => other(t)
    }
  }
}
