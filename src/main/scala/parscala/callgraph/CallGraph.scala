package parscala
package callgraph

import parscala.{tree => tr}

class CallGraph(val methods : Set[Either[tr.Decl.Method, tr.Defn.Method]], val calls : Set[Edge]) {
  def ++(other : CallGraph) : CallGraph = {
    new CallGraph(methods ++ other.methods, calls ++ other.calls)
  }

  def addMethod(m : tr.Defn.Method) : CallGraph =
    new CallGraph(methods + Right(m), calls)

  def addMethod(m : tr.Decl.Method) : CallGraph =
    new CallGraph(methods + Left(m), calls)

  def call(m : tr.Defn.Method) : Set[Either[tr.Decl.Method, tr.Defn.Method]] =
    for (e <- calls; if (e.caller == m)) yield e.callee

  private def calledBy(m : Either[tr.Decl.Method, tr.Defn.Method]) : Set[tr.Defn.Method] =
    for (e <- calls; if (e.callee == m)) yield e.caller

  def calledBy(m : tr.Defn.Method) : Set[tr.Defn.Method] =
    calledBy(Right(m))

  def calledBy(m : tr.Decl.Method) : Set[tr.Defn.Method] =
    calledBy(Left(m))
}

object CallGraphBuilder {
  def fullCallGraph(pgraph : ProgramGraph) : CallGraph = {
    pgraph.definitions.foldLeft(empty){ case (acc, (label @ _, defn : tr.Defn)) =>
      scalaz.std.option.cata(tr.Defn.asMethod(defn))(
          method => acc ++ calls(method, pgraph)
        , acc
        )
    }
  }

  def calls(method : tr.Defn.Method, pgraph : ProgramGraph) : CallGraph = {
    def collectCalls(ast : tr.Expr) : CallGraph = {
      val const2 : (Any, Any) => CallGraph = Function.const2(empty)
      val const3 : (Any, Any, Any) => CallGraph = Function.const3(empty)
      val const4 : (Any, Any, Any, Any) => CallGraph = Function.const4(empty)
      tr.Expr.cata(
          const3 // literal
        , const3 // identifier
        , (_, lhs, rhs, _) => // assignment todo: add an update or setter edge if lhs is not a mutable variable
            collectCalls(lhs) ++ collectCalls(rhs)
        , (_, fun, argss, _) => { // application
            val callsInFun : CallGraph = collectCalls(fun)
            val callsInArgss : CallGraph = argss.flatten.foldLeft(empty)(
                (acc, arg) => acc ++ collectCalls(arg)
              )
            val callsInSubtrees : CallGraph = callsInArgss ++ callsInFun
            val calleeIsDecl : Option[CallGraph] = 
              for (decl <- pgraph.declarations.get(funRef);
                   callee <- tr.Decl.asMethod(decl).map(Left(_)))
              yield new CallGraph(Set(callee), Set(Edge(method, callee)))
            lazy val calleeIsDefn : Option[CallGraph] = 
              for (defn <- pgraph.definitions.get(funRef);
                   callee <- tr.Defn.asMethod(defn).map(Right(_)))
              yield new CallGraph(Set(callee), Set(Edge(method, callee)))
            
            scalaz.std.option.cata(calleeIsDecl orElse calleeIsDefn)(
                calleeAndEdge => callsInSubtrees ++ calleeAndEdge
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
  
    collectCalls(method.body).addMethod(method)
  }

  def empty : CallGraph = 
    new CallGraph(Set(), Set())
}
