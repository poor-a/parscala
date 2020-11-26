package parscala
package callgraph

import parscala.tree

import scalaz.Either3

case class Edge(caller : tree.Defn.Method[Symbol, List[scalac.Type]], callee : CallGraph#Callee)
