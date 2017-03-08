package parscala
package df

import parscala.{controlflow => cf}
import parscala.{tree => tr}
import parscala.dot

import scala.collection.immutable.Traversable

object UseDefinition {

  /** Variables associated with point of assignment in the program. */
  type Assignment = (Symbol, SLabel)

  /**
   * Associates use of variables with the assignments reaching that
   * use. When the a program point is not a variable reference, it is
   * associated with an empty set.
   */ 
  private type UD = Map[SLabel, Set[Assignment]]

  def fromCFGraph(cfg : cf.CFGraph) : UseDefinition = fromReachingDefinition(ReachingDefinition(cfg))

  def fromReachingDefinition(rd : ReachingDefinition) : UseDefinition = {
    def useDefinitions(l : SLabel, reachingDefs : Set[Assignment]) : Set[Assignment] =
      rd.cfg(l) match {
        case None => 
          Set.empty
        case Some(node) =>
          val const2 : (Any, Any) => Set[Assignment] = (_, _) => Set.empty
          val const3 : (Any, Any, Any) => Set[Assignment] = (_, _, _) => Set.empty
          val const4 : (Any, Any, Any, Any) => Set[Assignment] = (_, _, _, _) => Set.empty
          val const5 : (Any, Any, Any, Any, Any) => Set[Assignment] = (_, _, _, _, _) => Set.empty
          tr.Node.nodeCata(
              const3 // literal
            , (sl, symbol, _) => // identifier
                reachingDefs filter { case (sym, assignment) => symbol == sym }
            , const4 // pattern definition
            , const4 // assignment
            , const4 // application
            , const4 // new
            , const4 // selection
            , const3 // this
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
            , const2 // expression
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
  def toDot : Traversable[dot.DotEdge] =
    useDefinitions.to[Stream].flatMap{ case (variable, definitions) =>
      definitions.to[Stream].map{ case (_, assignment) => 
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
