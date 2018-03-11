package parscala
package callgraph

import parscala.tree

import scalaz.Either3

case class Edge(caller : tree.Defn.Method, callee : Either3[tree.Decl.Method, tree.Defn.Method, tree.Expr])
