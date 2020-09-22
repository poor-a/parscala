package parscala
package callgraph

import parscala.{tree => tr}

import scalaz.Either3

class CallGraph(val methods : Set[Either[tr.Decl.Method, tr.Defn.Method]], val calls : Set[Edge]) {
  def ++(other : CallGraph) : CallGraph = {
    new CallGraph(methods ++ other.methods, calls ++ other.calls)
  }

  def addMethod(m : tr.Defn.Method) : CallGraph =
    new CallGraph(methods + Right(m), calls)

  def addMethod(m : tr.Decl.Method) : CallGraph =
    new CallGraph(methods + Left(m), calls)

  type Callee = Either3[tree.Decl.Method, tree.Defn.Method, tree.Expr]

  def call(m : tr.Defn.Method) : Set[Callee] =
    for (e <- calls; if (e.caller == m)) yield e.callee

  private def calledBy(m : Callee) : Set[tr.Defn.Method] =
    for (e <- calls; if (e.callee == m)) yield e.caller

  def calledBy(m : tr.Defn.Method) : Set[tr.Defn.Method] =
    calledBy(Either3.middle3(m) : Callee)

  def calledBy(m : tr.Decl.Method) : Set[tr.Defn.Method] =
    calledBy(Either3.left3(m) : Callee)
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
      val const5 : (Any, Any, Any, Any, Any) => CallGraph = Function.const5(empty)
      tr.Expr.cata(
          const3 // literal
        , const4 // identifier
        , (_, lhs, rhs, _) => // assignment todo: add an update or setter edge if lhs is not a mutable variable
            collectCalls(lhs) ++ collectCalls(rhs)
        , (l, fun, args, _) => { // application
            val callsInFun : CallGraph = collectCalls(fun)
            val callsInArgs : CallGraph = args.foldLeft(empty)(
                (acc, arg) => acc ++ collectCalls(arg)
              )
            val callsInSubtrees : CallGraph = callsInArgs ++ callsInFun
            pgraph.callTargets.get(l) match {
              case None => callsInSubtrees
              case Some(funRefs) => {
                val edges : Set[Edge] = 
                  funRefs.foldLeft(Set[Edge]()){
                    (acc : Set[Edge], funRef : MLabel) => acc ++ funRef.fold(
                        (dl : DLabel) => {
                          val calleeIsDecl : Option[Set[Edge]] =
                            for (decl <- pgraph.declarations.get(dl);
                                 callee <- tr.Decl.asMethod(decl).map[CallGraph#Callee](Either3.left3(_)))
                            yield Set(Edge(method, callee))
                          lazy val calleeIsDefn : Option[Set[Edge]] =
                            for (defn <- pgraph.definitions.get(dl);
                                 callee <- tr.Defn.asMethod(defn).map[CallGraph#Callee](Either3.middle3(_)))
                            yield Set(Edge(method, callee))
                          (calleeIsDecl orElse calleeIsDefn).getOrElse(Set[Edge]())
                        }
                      , (sl : SLabel) => {
                          val mEdge : Option[Set[Edge]] =
                            for (expr <- pgraph.expressions.get(sl))
                            yield Set(Edge(method, Either3.right3(expr)))
                          mEdge.getOrElse(Set[Edge]())
                        }
                  )
                }
                callsInSubtrees ++ new CallGraph(Set(Right(method)), edges)
              }
            }
          }
        , const5 // infix application
        , const4 // unary application
        , (_, _, argss, _) => // new todo: add edge to constructor
            argss.flatten.foldLeft(empty)(
                (acc, arg) => acc ++ collectCalls(arg)
              )
        , (_, obj, _, _, _) => // selection
            collectCalls(obj)
        , (_, argss) => // this(...) application TODO: edge to called constructor
            argss.flatten.foldLeft(empty)(
              (acc, arg) => acc ++ collectCalls(arg)
            )
        , const3 // this
        , const4 // super
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
//            enumerators.foldLeft(empty)(
//                (acc, enum) => acc ++ collectCalls(enum)
//              ) ++ collectCalls(outputExpr)
            collectCalls(outputExpr)
        , (_, enumerators, outputExpr, _) => // for-yield loop
//            enumerators.foldLeft(empty)(
//                (acc, enum) => acc ++ collectCalls(enum)
//              ) ++ collectCalls(outputExpr)
            collectCalls(outputExpr)
        , const2 // return statement
        , (_, value, _) => // return value statement
            collectCalls(value)
        , (_, exception, _) => // throw
            collectCalls(exception)
        , (_, statements, _) => // block
            statements.foldLeft(empty)(
                (acc, stmt) => 
                  stmt.fold(_ => acc // declaration
                           ,_ => acc // definition
                           ,expr => acc ++ collectCalls(expr) // expression
                           )
              )
//        , const4 // lambda
        , const3 // other expression
        , ast
        )
    }
  
    collectCalls(method.body).addMethod(method)
  }

  def empty : CallGraph = 
    new CallGraph(Set(), Set())
}
