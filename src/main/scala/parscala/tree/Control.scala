package parscala
package tree

import compiler.Quasiquote

/**
 * Useful method over syntax trees.
 */
object Control {
  /**
   * Catamorphism over expression syntax trees.
   * 
   * Pattern matches on `t` and passes the arguments of a
   * data constructor to corresponding function.
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
  def exprCata[A](lit : (Lit, Tree) => A,
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
                  t : Tree) : A =
    t match {
      case q"${n : Int}" => lit(IntLit(n), t)
      case q"${s : String}" => lit(StringLit(s), t)
      case _ if t.isInstanceOf[compiler.Ident] => id(t.symbol)
      case q"(..$components)" if components.size >= 2 => tuple(components)
      case q"new { ..$earlydefns } with ..$parents { $_ => ..$stats }" => newE(earlydefns, parents, stats)
      case q"$expr.this" => thisE(expr)
      case q"$expr.$tname" => sel(expr, tname)
      case q"$expr(...$args)" if t.isInstanceOf[compiler.Apply] => app(expr, args)
      case q"if ($pred) $thenE" => ifE(pred, thenE)
      case q"if ($pred) $thenE else $elseE" => ifElse(pred, thenE, elseE)
      case q"while ($pred) $body" => whileE(pred, body)
      case q"for (..$enums) $body" => forE(enums, body)
      case q"for (..$enums) yield $body" => forYieldE(enums, body)
      case q"$lexpr = $rexpr" => ass(lexpr, rexpr)
      case q"$mods val $_ = $expr" => idDef(mods, t.symbol, expr)
      case q"$mods var $_ = $expr" => idDef(mods, t.symbol, expr)
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

  /**
   * Catamorphism over declaration syntax trees.
   * 
   * Pattern matches on `t` and passes the arguments of a
   * data constructor to corresponding function.
   * 
   * @tparam A the common result type of the function arguments
   * @param varD handles the name and right hand side (if it is an initialization) of a variable declaration
   * @param valD handles the name and right hand side (if it is an initialization) of a value declaration
   * @param methodD handles method declaration with its argument lists and optional body
   *   (it is `Some(body)` if it is a definition, `None` it it is an abstract method)
   * @param classD handles a class definition with its members
   * @param objectD handles an object definition with its members
   * @param packageOD handles a pacakage object definition with its members
   * @param packageD handles a package definition with its members
   * @return the common type A
   */
  def declCata[A](varD : (String, Symbol, Option[Tree]) => A
                 ,valD : (String, Symbol, Option[Tree]) => A
                 ,methodD : (String, Symbol, List[List[Tree]], Option[Tree]) => A
                 ,classD : (String, Symbol, List[Tree]) => A
                 ,objectD : (String, Symbol, List[Tree]) => A
                 ,packageOD : (String, Symbol, List[Tree]) => A
                 ,packageD : (String, Symbol, List[Tree]) => A
                 ,t : Tree
                 ) : A =
    t match {
      case q"$_ var $name: $_ = $expr" => varD(name.toString, t.symbol, Some(expr))
      case q"$_ var $name = $expr" => varD(name.toString, t.symbol, Some(expr))
      case q"$_ var $name: $_" => varD(name.toString, t.symbol, None)
      case q"$_ val $name: $_ = $expr" => valD(name.toString, t.symbol, Some(expr))
      case q"$_ val $name = $expr" => valD(name.toString, t.symbol, Some(expr))
      case q"$_ val $name: $_" => valD(name.toString, t.symbol, None)
      case q"$_ def $name (...$argss) : $_ = $body" => methodD(name.toString, t.symbol, argss, Some(body))
      case q"$_ def $name (...$argss) : $_" => methodD(name.toString, t.symbol, argss, None)
      case q"$_ class $name extends ..$_ { ..$defs }" => classD(name.toString, t.symbol, defs)
      case q"$_ class $name extends $_ with ..$_ { ..$defs }" => classD(name.toString, t.symbol, defs)
      case q"$_ object $name extends ..$_ { ..$defs }" => objectD(name.toString, t.symbol, defs)
      case q"$_ object $name extends $_ with ..$_ { ..$defs }" => objectD(name.toString, t.symbol, defs)
      case q"package object $name extends ..$_ { ..$defs }" => packageOD(name.toString, t.symbol, defs)
      case q"package object $name extends $_ with ..$_ { ..$defs }" => packageOD(name.toString, t.symbol, defs)
      case q"package $name { ..$topStmts }" => packageD(name.toString, t.symbol, topStmts)
    }

  /**
   * Catamorphism over pattern syntax trees.
   * 
   * Pattern matches on `t` and passes the arguments of a
   * data constructor to corresponding function.
   * 
   * @tparam A the common result type of the function arguments
   * @param litP handles the a literal pattern (e.g. `1` or `'a'`)
   * @param idP handles a binding pattern (e.g. `x`)
   * @param underP handles the underscore pattern `_` 
   * @param otherP handles yet unsupported patterns
   * @return the common type `A˙
   */
  def patCata[A]( litP : (Constant) => A
                , idP : (Name, Tree) => A
                , underP : () => A
                , otherP : (Tree) => A
                , t : Tree
                ) : A =
    t match {
      case pq"${lit : compiler.Literal}" => litP(lit.value)
      case pq"$name @ $pat" => idP(name, pat)
      case pq"_" => underP()
      case _ => otherP(t)
    }

  /**
   * Catamorphism over literals.
   * 
   * Pattern matches on `lit` and passes the arguments of a
   * data constructor to corresponding function.
   *
   * Note that this is a total function despite there is no 'other'
   * case, since all the literal types in the
   * [[http://scala-lang.org/files/archive/spec/2.12/01-lexical-syntax.html language specification]]
   * are enumerated.
   * 
   * @tparam A the common result type of the argument functions
   * @param intLit handles an integer literal
   * @param boolLit handles a boolean literal
   * @param charLit handles a character literal
   * @param stringLit handles a string literal
   * @param floatLit handles a float literal
   * @param doubleLit handles a double literal
   * @param symbolLit handles a symbol literal
   * @return the common type `A˙
   */
  def litCata[A]( intLit : Int => A
                , boolLit : Boolean => A
                , charLit : Char => A
                , stringLit : String => A
                , floatLit : Float => A
                , doubleLit : Double => A
                , symbolLit : Symbol => A
                , otherLit : Any => A
                , lit : Constant
                ) : A =
    lit match {
      case compiler.Constant(n : Int) => intLit(n)
      case compiler.Constant(b : Boolean) => boolLit(b)
      case compiler.Constant(c : Char) => charLit(c)
      case compiler.Constant(s : String) => stringLit(s)
      case compiler.Constant(f : Float) => floatLit(f)
      case compiler.Constant(d : Double) => doubleLit(d)
      case compiler.Constant(s : Symbol) => symbolLit(s)
      case compiler.Constant(x) => otherLit(x)
    }
}
