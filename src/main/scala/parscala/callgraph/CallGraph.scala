package parscala
package callgraph

import parscala.{tree => tr}

class CallGraph(val methods : Set[tr.Method], val calls : Set[Edge]) {
  def ++(other : CallGraph) : CallGraph = {
    new CallGraph(methods ++ other.methods, calls ++ other.calls)
  }

  def addMethod(m : tr.Method) : CallGraph =
    new CallGraph(methods + m, calls)

  def call(m : tr.Method) : Set[tr.Method] =
    for (e <- calls; if (e.caller == m)) yield e.callee

  def calledBy(m : tr.Method) : Set[tr.Method] = 
    for (e <- calls; if (e.callee == m)) yield e.caller
}

object CallGraphBuilder {
  def fullCallGraph(pgraph : ProgramGraph) : CallGraph = {
    println("pgraph: " + pgraph.declarations)
    pgraph.declarations.foldLeft(empty)((acc, x) => {
      val (label @ _, decl) : (DLabel, tr.Decl) = x
      scalaz.std.option.cata(tr.Decl.asMethod(decl))(
          method => acc ++ calls(method, pgraph)
        , acc
        )
    })
  }

  def calls(method : tr.Method, pgraph : ProgramGraph) : CallGraph = {
    def collectCalls(ast : tr.Node) : CallGraph = {
      val const2 : (Any, Any) => CallGraph = Function.const2(empty)
      val const3 : (Any, Any, Any) => CallGraph = Function.const3(empty)
      val const4 : (Any, Any, Any, Any) => CallGraph = Function.const4(empty)
      tr.Node.nodeCata(
          const3 // literal
        , const3 // identifier
        , (_, _, rhs, _) => // pattern matching definition
            collectCalls(rhs)
        , (_, lhs, rhs, _) => // assignment todo: add an update or setter edge if lhs is not a mutable variable
            collectCalls(lhs) ++ collectCalls(rhs)
        , (_, fun, argss, funRef, _) => { // application
            val callsInFun : CallGraph = collectCalls(fun)
            val callsInArgss : CallGraph = argss.flatten.foldLeft(empty)(
                (acc, arg) => acc ++ collectCalls(arg)
              )
            val callsInSubtrees : CallGraph = callsInArgss ++ callsInFun
            scalaz.std.option.cata(pgraph.declarations.get(funRef))(
                decl =>
                  scalaz.std.option.cata(tr.Decl.asMethod(decl))(
                      callee => callsInSubtrees ++ new CallGraph(Set(callee), Set(Edge(method, callee)))
                    , callsInSubtrees
                    )
              , callsInSubtrees
              )
          }
        , (_, _, argss, _) => // new todo: add edge to constructor
            argss.flatten.foldLeft(empty)(
                (acc, arg) => acc ++ collectCalls(arg)
              )
        , (_, obj, _, _) => // selection
            collectCalls(obj)
        , const3 // this
        , (_, components, _) => // tuple
            components.foldLeft(empty)(
                (acc, component) => acc ++ collectCalls(component)
              )
        , (_, pred, thenBranch, _) => // if
            collectCalls(pred) ++
            collectCalls(thenBranch)
        , (_, pred, thenBranch, elseBranch, _) => // if-then-else
            collectCalls(pred) ++
            collectCalls(thenBranch) ++
            collectCalls(elseBranch)
        , (_, pred, body, _) => // while loop
            collectCalls(pred) ++
            collectCalls(body)
        , (_, enumerators, outputExpr, _) => // for loop
            enumerators.foldLeft(empty)(
                (acc, enum) => acc ++ collectCalls(enum)
              ) ++ collectCalls(outputExpr)
        , (_, enumerators, outputExpr, _) => // for-yield loop
            enumerators.foldLeft(empty)(
                (acc, enum) => acc ++ collectCalls(enum)
              ) ++ collectCalls(outputExpr)
        , const2 // return statement
        , (_, value, _) => // return value statement
            collectCalls(value)
        , (_, exception, _) => // throw
            collectCalls(exception)
        , (_, statements, _) => // block
            statements.foldLeft(empty)(
                (acc, stmt) => acc ++ collectCalls(stmt)
              )
        , const4 // lambda
        , const2 // other expression
        , ast
        )
    }
  
    method.body match {
      case Some(ast) =>
        collectCalls(ast).addMethod(method)
      case None =>
        empty
    }
  }

  def empty : CallGraph = 
    new CallGraph(Set[tr.Method](), Set[Edge]())
}
