package parscala
package callgraph

import parscala.tree

case class Edge(caller : tree.Defn.Method, callee : Either[tree.Decl.Method, tree.Defn.Method])
