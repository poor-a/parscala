package parscala
package callgraph

import parscala.tree.Method

case class Edge(caller : Method, callee : Method)
