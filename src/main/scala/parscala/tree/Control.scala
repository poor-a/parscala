package parscala
package tree

import scala.meta
import scalac.Quasiquote

/**
 * Useful method over syntax trees.
 */
object Control {
  /**
   * Catamorphism over expression syntax trees of scala.meta.
   */
  def exprCataMeta[A]( lit : meta.Lit => A
                     , id : String => A
                     , tuple : List[meta.Term] => A
                     , newE : (meta.Type, meta.Name, List[List[meta.Term]]) => A
                     , thisE : meta.Name => A
                     , sel : (meta.Term, meta.Name) => A
                     , app : (meta.Term, List[meta.Term]) => A
                     , appInfix : (meta.Term, meta.Name, List[meta.Type], List[meta.Term]) => A
                     , ifE : (meta.Term, meta.Term) => A
                     , ifElse : (meta.Term, meta.Term, meta.Term) => A
                     , whileE : (meta.Term, meta.Term) => A
                     , forE : (List[meta.Enumerator], meta.Term) => A
                     , forYieldE : (List[meta.Enumerator], meta.Term) => A
                     , asgn : (meta.Term, meta.Term) => A
                     , returnUnit : () => A
                     , returnExpr : meta.Term => A
                     , block : List[meta.Stat] => A
                     , other : meta.Term => A
                     , t : meta.Term
                     ) : A =
    t match {
      case l : meta.Lit => lit(l)
      case meta.Term.Name(s) => id(s)
      case meta.Term.Tuple(comps) => tuple(comps)
      case meta.Term.New(meta.Init(typ, name, argss)) => newE(typ, name, argss)
      case meta.Term.This(qual) => thisE(qual)
      case meta.Term.Select(qual, name) => sel(qual, name)
      case meta.Term.Apply(f, args) => app(f, args)
      case meta.Term.ApplyInfix(lhs, f, typArgs, rhs) => appInfix(lhs, f, typArgs, rhs)
      case meta.Term.If(pred, thenPart, elsePart @ meta.Lit.Unit()) if elsePart.pos.start >= elsePart.pos.end => ifE(pred, thenPart)
      case meta.Term.If(pred, thenPart, elsePart) => ifElse(pred, thenPart, elsePart)
      case meta.Term.While(pred, body) => whileE(pred, body)
      case meta.Term.For(enums, body) => forE(enums, body)
      case meta.Term.ForYield(enums, body) => forYieldE(enums, body)
      case meta.Term.Assign(lhs, rhs) => asgn(lhs, rhs)
      case meta.Term.Return(expr) if expr.pos.start >= expr.pos.end => returnUnit()
      case meta.Term.Return(expr) => returnExpr(expr)
      case meta.Term.Block(stats) => block(stats)
      case _ => other(t)
    }

  /**
   * Catamorphism over expression syntax trees of the Scala compiler.
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
      // TODO make literal handling total
      case q"${n : Int}" => lit(IntLit(n), t)
      case q"${s : String}" => lit(StringLit(s), t)
      case _ if t.isInstanceOf[scalac.Ident] => id(t.symbol)
      case q"(..$components)" if components.size >= 2 => tuple(components)
      case q"new { ..$earlydefns } with ..$parents { $_ => ..$stats }" => newE(earlydefns, parents, stats)
      case q"$expr.this" => thisE(expr)
      case q"$expr.$tname" => sel(expr, tname)
      case q"$expr(...$args)" if t.isInstanceOf[scalac.Apply] => app(expr, args)
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
      case _ if t.isInstanceOf[scalac.Block] =>
        val q"{ ..$stmts }" = t
        block(stmts)
      case _ => other(t)
  }

  /**
   * Catamorphism over scala.meta declaration syntax trees.
   */
  def declCataMeta[A]( val_ : (List[meta.Mod], List[meta.Pat]) => meta.Decl.Val => A
                     , var_ : (List[meta.Mod], List[meta.Pat]) => meta.Decl.Var => A
                     , def_ : (List[meta.Mod], meta.Term.Name, List[meta.Type.Param], List[List[meta.Term.Param]]) => meta.Decl.Def => A
                     , type_ : (List[meta.Mod], meta.Type.Name, List[meta.Type.Param], meta.Type.Bounds) => meta.Decl.Type => A
                     , decl : meta.Decl
                     ) : A =
    decl match {
      case d @ meta.Decl.Val(mods, pats, decltype @ _) => val_(mods, pats)(d)
      case d @ meta.Decl.Var(mods, pats, decltype @ _) => var_(mods, pats)(d)
      case d @ meta.Decl.Def(mods, name, tparams, paramss, decltype @ _) => def_(mods, name, tparams, paramss)(d)
      case d @ meta.Decl.Type(mods, name, tparams, bounds) => type_(mods, name, tparams, bounds)(d)
    }

  /**
   * Catamorphism over Scala compiler declaration syntax trees.
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
      case q"$_ def $name (...$argss) : $_" => methodD(name.toString, t.symbol, argss, None)
      case q"$_ def $name (...$argss) : $_ = $body" => methodD(name.toString, t.symbol, argss, Some(body))
   // full syntax: 
   // case q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }"
      case q"$_ class $name[..$_] $_(...$_) extends { ..$_ } with ..$_ { $_ => ..$defs }" => classD(name.toString, t.symbol, defs)
   // full syntax: 
   // case q"$mods object $tname extends { ..$earlydefns } with ..$parents { $self => ..$stats }"
      case q"$_ object $name extends { ..$_ } with ..$_ { $_ => ..$defs }" => objectD(name.toString, t.symbol, defs)
   // full syntax: 
   // case q"package object $tname extends { ..$earlydefns } with ..$parents { $self => ..$stats }"
      case q"package object $name extends { ..$_ } with ..$_ { $_ => ..$defs }" => packageOD(name.toString, t.symbol, defs)
      case q"package $name { ..$topStmts }" => packageD(name.toString, t.symbol, topStmts)
    }

  def patCataMeta[A]( var_ : meta.Term.Name => A
                    , wildcard : () => A
                    , seqWildcard : () => A
                    , bind : (meta.Pat, meta.Pat) => A
                    , alternative : (meta.Pat, meta.Pat) => A
                    , tuple : List[meta.Pat] => A
                    , extract : (meta.Term, List[meta.Pat]) => A
                    , extractInfix : (meta.Pat, meta.Term.Name, List[meta.Pat]) => A
                    , interpolate : (meta.Term.Name, List[meta.Lit], List[meta.Pat]) => A
                    , xml : (List[meta.Lit], List[meta.Pat]) => A
                    , typed : (meta.Pat, meta.Type) => A
                    , p : meta.Pat
                    ) : A =
    p match {
      case meta.Pat.Var(name) => var_(name)
      case meta.Pat.Wildcard() => wildcard()
      case meta.Pat.SeqWildcard() => seqWildcard()
      case meta.Pat.Bind(lhs, rhs) => bind(lhs, rhs)
      case meta.Pat.Alternative(lhs, rhs) => alternative(lhs, rhs)
      case meta.Pat.Tuple(args) => tuple(args)
      case meta.Pat.Extract(fun, args) => extract(fun, args)
      case meta.Pat.ExtractInfix(lhs, op, rhs) => extractInfix(lhs, op, rhs)
      case meta.Pat.Interpolate(prefix, parts, args) => interpolate(prefix, parts, args)
      case meta.Pat.Xml(parts, args) => xml(parts, args)
      case meta.Pat.Typed(lhs, rhs) => typed(lhs, rhs)
    }

  /**
   * Catamorphism over Scala compiler pattern syntax trees.
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
      case pq"${lit : scalac.Literal}" => litP(lit.value)
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
      case scalac.Constant(n : Int) => intLit(n)
      case scalac.Constant(b : Boolean) => boolLit(b)
      case scalac.Constant(c : Char) => charLit(c)
      case scalac.Constant(s : String) => stringLit(s)
      case scalac.Constant(f : Float) => floatLit(f)
      case scalac.Constant(d : Double) => doubleLit(d)
      case scalac.Constant(s : Symbol) => symbolLit(s)
      case scalac.Constant(x) => otherLit(x)
    }

  def metaStatKindCata[A]( term_ : meta.Term => A
                         , decl_ : meta.Decl => A
                         , defn_ : meta.Defn => A
                         , secondary_ : meta.Ctor.Secondary => A
                         , pobject_ : meta.Pkg.Object => A
                         , pkg_ : meta.Pkg => A
                         , import_ : meta.Import => A
                         , stat : meta.Stat
                         ) : A =
    stat match {
      case term : meta.Term => term_(term)
      case decl : meta.Decl => decl_(decl)
      case defn : meta.Defn => defn_(defn)
      case snd : meta.Ctor.Secondary => secondary_(snd)
	  case pobj : meta.Pkg.Object => pobject_(pobj)
	  case pkg : meta.Pkg => pkg_(pkg)
      case imprt : meta.Import => import_(imprt)
    }

  def isPackageMeta(decl : Decl) : Boolean =
    decl match {
      case meta.Pkg(_, _) => true
      case _ => false
    }

  def patNames(pat : meta.Pat) : List[meta.Name] =
    patCataMeta( name => List(name) // var
               , () => List()       // wildcard
               , () => List()       // sequence wildcard
               , (lhs, rhs) => patNames(lhs) ++ patNames(rhs) // bind
               , (lhs, rhs) => patNames(lhs) ++ patNames(rhs) // alternative
               , args => args.flatMap(patNames) // tuple
               , (_, args) => args.flatMap(patNames) // extract
               , (lhs, _, rhs) => patNames(lhs) ++ rhs.flatMap(patNames) // extract infix
               , (_, _, args) => args.flatMap(patNames) // interpolate
               , (_, args) => args.flatMap(patNames) // xml
               , (lhs, _) => patNames(lhs) // typed
               , pat
               )
}
