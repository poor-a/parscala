package parscala
package df

import parscala.{controlflow => cf}
import parscala.{tree => tr}
import parscala.dot

import scala.collection.immutable.Traversable

object UseDefinition {

  /** Point of assignment in the program. */
  type Assignment = Either[DLabel, SLabel]

  /**
   * Associates use of a variable with the assignments reaching that
   * use. A program point that is not a variable reference is associated
   * with an empty set.
   */ 
  private type UD = Map[SLabel, Set[Assignment]]

  def fromCFGraph(cfg : cf.CFGraph) : UseDefinition = fromReachingDefinition(ReachingDefinition(cfg))

  def fromReachingDefinition(rd : ReachingDefinition) : UseDefinition = {
    def useDefinitions(l : SLabel, reachingDefs : Set[(Symbol, Assignment)]) : Set[Assignment] =
      rd.cfg(l) match {
        case None =>
          Set.empty
        case Some(node) =>
          val const2 : (Any, Any) => Set[Assignment] = (_, _) => Set.empty
          val const3 : (Any, Any, Any) => Set[Assignment] = (_, _, _) => Set.empty
          val const4 : (Any, Any, Any, Any) => Set[Assignment] = (_, _, _, _) => Set.empty
          val const5 : (Any, Any, Any, Any, Any) => Set[Assignment] = (_, _, _, _, _) => Set.empty
          tr.Expr.cata(
              const3 // literal
            , (_, _, symbols, _) => // identifier
                reachingDefs filter { case (sym, assignment @ _) => symbols contains sym } map (_._2)
            , const4 // assignment
            , const4 // application
            , const5 // infix application
            , const4 // unary application
            , const4 // new
            , const5 // selection
            , const2 // this(...) application
            , const3 // this
            , const4 // super
            , const3 // tuple
            , const4 // if-then
            , const5 // if-then-else
            , const4 // while loop
            , const4 // for loop
            , const4 // for-yield loop
            , const2 // return
            , const3 // return with expr
            , const3 // throw
            , const3 // block
//            , const4 // lambda function
            , const3 // other expression
            , node)
    }
    new UseDefinition(rd.rd map{ case (k, v) => (k, useDefinitions(k, v)) })
  }
}

/**
 * Use-definition stores which assignments to a variable may reach a
 * use of that variable.
 *
 * See Flemming Nielson, Hanne Riis Nielson, Chris Hankin:
 * 'Principles of Program Analysis', Section 2.1
 */
class UseDefinition private (useDefinitions : UseDefinition.UD) {
  def toDotEdges : Traversable[dot.DotEdge] =
    useDefinitions.to[Stream].flatMap{ case (variable, definitions) =>
      definitions.to[Stream].map{ assignment =>
        dot.DotEdge(dot.DotNode(assignment.toString), dot.DotNode(variable.toString)) !! List(dot.DotAttr.label("flow"), dot.DotAttr.color(dot.Color.Purple), dot.DotAttr.fontColor(dot.Color.Purple))
      }
  }

  /**
   * When l is a variable reference, returns the set of assignments
   * of the variable that reaches the reference. Otherwise, returns
   * an empty set.
   */
  def apply(l : SLabel) : Set[UseDefinition.Assignment] = useDefinitions.get(l).getOrElse(Set.empty)
}
