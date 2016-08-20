package parscala
package callgraph

import parscala.tree._

class CallGraph(val calls : List[Edge], val unresolved : List[(Symbol, Method => Edge)]) {
  val unknown : Set[Symbol] = (unresolved.toIterator map (_._1)).toSet

  def resolve(m : Method) : CallGraph = {
    if (unknown contains m.symbol) {
      val (resolvable, unres) = unresolved partition (x => x._1 == m.symbol)
      val resolved : List[Edge] = resolvable map {x => x._2(m)}
      new CallGraph(resolved ++ calls, unres)
    } 
    else
      this
  }

  val isClosed : Boolean = unresolved.isEmpty

  def ++(other : CallGraph) : CallGraph = {
    new CallGraph(calls ++ other.calls, unresolved ++ other.unresolved)
  }
}

object CallGraphBuilder {
  import compiler.Quasiquote

  type Unresolved = (Symbol, Method => Edge)

  private def fold[A](ast : Tree, acc : A)(f : (A, Tree) => A) : A = {
    ast match {
      case _ if ast.isInstanceOf[compiler.Block] => {
        val q"{..$stmts}" = ast
        stmts.foldLeft(acc)((acc, ast) => fold(ast, acc)(f))
      }
      case q"if ($p) $a else $b" => {
        val acc1 = fold(p, acc)(f)
        val acc2 = fold(a, acc1)(f)
        fold(b, acc2)(f)
      }
      case q"while ($p) $body" => {
        val acc1 = fold(p, acc)(f)
        fold(body, acc1)(f)
      }
      case q"$_ val $_: $_ = $expr" =>
        fold(expr, acc)(f)
      case q"$_ var $_: $_ = $expr" =>
        fold(expr, acc)(f)
      case _ => 
        f(acc, ast)
    }
  }

 def getCalls(method : Method) : CallGraph = {
    def addSym(acc : (List[Edge], List[Unresolved]), s : Symbol) : (List[Edge], List[Unresolved]) = {
      val (res, unres) = acc
      if (s == method.symbol && !(res exists (_.callee == s)))
        (Edge(method, method) :: res, unres)
      else
        if (!(unres exists (_._1 == s)))
          (res, (s, Edge(method, _ : Method)) :: unres)
        else
          acc
    }
  
    def f (acc : (List[Edge], List[Unresolved]), t : Tree) : (List[Edge], List[Unresolved]) = {
      if (t.isInstanceOf[compiler.Apply]) {
        t match {
          case q"$g(...$args)" => addSym(acc, g.symbol)
          case _ => acc
        }
      } else
          acc
    }
    
    method.body match {
      case Some(ast) => {
        val (calls, unresolved) = fold(ast, (List[Edge](), List[Unresolved]()))(f)
        new CallGraph(calls, unresolved)
      }
      case None =>
        empty
    }

  }

  def empty : CallGraph = 
    new CallGraph(List.empty, List.empty)
}
