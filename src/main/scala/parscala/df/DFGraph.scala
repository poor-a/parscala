package parscala
package df

import parscala.controlflow._
import tree.{Expr, ExprTree}
import parscala.{tree => tr}

import scala.collection.immutable.Traversable

/** 
 * Dataflow edge label. It is the superclass of specific dataflow edge
 * labels. It is used to label edges in a dataflow graph.
 *
 * @see [[DFGraph]]
 */
sealed abstract class DEdgeLabel

/**
 * Flow label. Indicates that the value of an expression provides value
 * for (or flows into) an other.
 */
case class F() extends DEdgeLabel {
  override def toString : String = "flows"
}

/**
 * Dependency label. Indicates dependency of unspecified kind.
 */
case class D() extends DEdgeLabel {
  override def toString : String = "depends"
}

/**
 * Constructor label. Indicates that an expression is used to
 * construct some datatype (a tuple, list, etc.).
 *
 * @param n Index of an expression in the constructed datatype (a
 * tuple, list, etc.).
 */
case class C(n : Int) extends DEdgeLabel {
  override def toString : String = s"constructs($n)"
}

/**
 * Selector label. Indicates that the value of an expression is derived
 * from some datatype (a tuple, list, etc.).
 *
 * @param n Index of an expression in the constructed datatype (a
 * tuple, list, etc.).
 */
case class S(n : Int) extends DEdgeLabel {
  override def toString : String = s"selects($n)"
}

/**
 * Auxiliary object for dataflow graphs.
 */

object DFGraph {
  /**
   * Directed, labeled dataflow edge. Direction corresponds flow of
   * value, that is `((source, destination), label)`.
   */
  type Edge = ((SLabel, SLabel), DEdgeLabel)

 /**
  * Constructs a dataflow graph from a `ExprTree`
  * (extended abstract syntax tree) and use-definition information.
  *
  * @see [[UseDefinition]]
  * @see [[tree.ExprTree]]
  */
  def apply(tree : ExprTree, ud : UseDefinition) : DFGraph =
    new DFGraph(traverse(tree.root, ud).toList)

 /**
  * Constructs a dataflow graph from an abstract syntax tree
  * and use-definition information.
  *
  * @see [[UseDefinition]]
  * @see [[tree.ExprTree]]
  */
  def apply(ast : Expr, ud : UseDefinition) : DFGraph =
    new DFGraph(traverse(ast, ud).toList)

  private def traverse(root : Expr, ud : UseDefinition) : Set[Edge] = {
    def const2[A](x : A) : (Any, Any) => A = (_, _) => x
    def const3[A](x : A) : (Any, Any, Any) => A = (_, _, _) => x
    def const4[A](x : A) : (Any, Any, Any, Any) => A = (_, _, _, _) => x
    tree.Expr.cata(
        const3(Set())  // literal
      , (label, _, _symbols, _) => { // identifier
          ud(label).map { case (sym @ _, assignment) => ((assignment -> label), F()) }}
      , (label, _, rhs, _) => // assignment
          traverse(rhs, ud) + ((rhs.label -> label, F()))
      , (label, fun, args, _) => { // application
          val edges : Set[Edge] = 
            args.map(arg => traverse(arg, ud) + ((arg.label -> label, D())) )
            .foldLeft(Set.empty[Edge])(_ union (_)) + ((fun.label -> label, D())) union traverse(fun, ud)
          fun match {
            case tr.Select(_, expr, _, _) => edges + ((expr.label -> label, D()))
            case _                        => edges
          }
        }
      , (_label, _lhs, _op, _args, _) => Set[Edge]() // infix application TODO
      , (_label, _op, _arg, _) => Set[Edge]() // unary application TODO
      , (label, _, argss, _) => // new
          argss.flatMap(args => args.map( arg => traverse(arg, ud) + ((arg.label -> label, D())) )).foldLeft(Set.empty[Edge])(_ union (_))
      , (label, obj, _, _) => // select
          traverse(obj, ud) + ((obj.label -> label, D()))
      , const3(Set.empty[Edge]) // this
      , const4(Set.empty[Edge]) // super
      , (label, components, _) => // tuple
          (components zip (1 to components.length)).foldLeft(Set.empty[Edge]){ case (acc, (c, n)) => acc union traverse(c, ud) + ((c.label -> label, C(n)))}
      , (_, cond, tBranch, _) => // if-then
          traverse(cond, ud) union traverse(tBranch, ud)
      , (_, cond, tBranch, fBranch, _) => // if-then-else
          traverse(cond, ud) union traverse(tBranch, ud) union traverse(fBranch, ud)
      , (_, cond, body, _) => // while loop
          traverse(cond, ud) union traverse(body, ud)
      , (_, enumerators, body, _) => traverse(body, ud) // for loop TODO
//          enumerators.foldLeft(Set.empty[Edge])( (acc, enum) => traverse(enum, ud) ) union traverse(body, ud)
      , (_, enumerators, body, _) => traverse(body, ud) // for-yield loop TODO
//          enumerators.foldLeft(Set.empty[Edge])( (acc, enum) => traverse(enum, ud) ) union traverse(body, ud)
      , const2(Set.empty[Edge]) // return statement
      , (label, expr, _) => // return expr statement
          traverse(expr, ud) + ((expr.label -> label, F()))
      , (label, expr, _) => // throw statement
          traverse(expr, ud)
      , (label, statements, _) => { // block expression
          val edges : Set[Edge] = statements.foldLeft(Set.empty[Edge]){ (acc, stmt) => 
            stmt.fold(_ => acc
                     ,_ => acc
                     ,expr => acc union traverse(expr, ud)
                     )
          }
          if (statements.isEmpty) edges
          else statements.last.label.map((l : SLabel) => edges + ((l -> label, F()))).getOrElse(edges)
        }
//      , (label, _, body, _) => { // lambda function
//          traverse(body, ud)
//        }
      , (label, _, _) => // other expression
          Set.empty[Edge]
      , root
    )
  }
}

/**
 * The dataflow graph. Stores the information on value flow
 * in a program.
 *
 * A graph is represented as a collection of dataflow edges.
 * Edges are directed, and labeled with the kind of flow.
 *
 * @see [[DEdgeLabel]]
 */
class DFGraph(graph : List[DFGraph.Edge]) {

  /**
   * Adds a single edge to this dataflow graph.
   */
  def +(e : DFGraph.Edge) : DFGraph =
    new DFGraph(e :: graph)

  /**
   * Adds a collection of edges to this dataflow graph.
   */
  def ++:(es : Traversable[((SLabel, SLabel), DEdgeLabel)]) : DFGraph =
    new DFGraph(es ++: graph)

  /**
   * Converts this dataflow graph to a sequence of dot edges for
   * visualization.
   *
   * @example asd
   *  a2
   */
  def toDotEdges : Traversable[dot.DotEdge] = 
    graph.map{ case ((s, t), label) =>
                 (dot.DotEdge(dot.DotNode(s), dot.DotNode(t))
                  !! dot.DotAttr.color(dot.Color.Red)
                  !! dot.DotAttr.fontColor(dot.Color.Red)
                  !! dot.DotAttr.label(label.toString))
             }
}
