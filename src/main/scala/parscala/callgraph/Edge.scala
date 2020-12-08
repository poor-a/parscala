package parscala
package callgraph

import parscala.tree

case class Edge(caller : tree.Defn.Method[Symbol, List[scalac.Type]], callee : CallGraph#Callee)
