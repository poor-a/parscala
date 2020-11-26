package parscala
package df

import parscala.{controlflow => cf}
import parscala.{tree => tr}

import scalaz.std.option

object ReachingDefinition {

  /** Reaching definition: variables associated with point of
   *  assignment in the program.
   */
  type RD = Set[(Symbol, Either[DLabel, SLabel])]

  def apply(cfg : cf.CFGraph) : ReachingDefinition = {
    // Assignments reaching the entry of each expression
    type RDMap = Map[SLabel, RD]

    def transfer(l : SLabel, rd : RD) : RD =
      cfg(l) match {
        case None => 
          rd
        case Some(node) =>
          val const2 : (Any, Any) => RD = (_, _) => rd
          val const3 : (Any, Any, Any) => RD = (_, _, _) => rd
          val const4 : (Any, Any, Any, Any) => RD = (_, _, _, _) => rd
          val const5 : (Any, Any, Any, Any, Any) => RD = (_, _, _, _, _) => rd
          node.cata(
              const3 // literal
            , const4 // identifier
/*            , (sl, pat, _, _) => // pattern definition
                rd ++ (tr.Pat.identifiers(pat).map(symbol => (symbol, sl)).toSet)
*/
            , (sl, lhs, _, _) => // assignment
                lhs.cata(
                    const3 // literal
                  , (_, _, symbol, _) => // identifier
                      rd.filter { case (s : Symbol, _ ) => symbol != s } + ((symbol, Right(sl)))
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
//                  , const4 // lambda expression
                  , const3 // other expression
                  )
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
//            , const4 // lambda expression
            , const3 // other expression
            )
    }

    def updateExprRd(expr : SLabel, rd : RD, precRD : RD, analysis : RDMap) : (RD, RDMap) = {
        val before : RD = rd ++ precRD
        val after : RD = transfer(expr, before)
        (after, analysis.updated(expr, before))
    }

    def updateNodeRD(acc : (RD, RDMap), n : cf.Node[_,_]) : (RD, RDMap) = {
      val (precRD, analysis) = acc
      val const : Any => (RD, RDMap) = Function.const(acc)
      val const2 : (Any, Any) => (RD, RDMap) = (_, _) => acc
      val const3 : (Any, Any, Any) => (RD, RDMap) = (_, _, _) => acc

      cf.Node.cata(
          const  // label
        , const3 // pattern
        , expr =>   // expression
            option.cata(analysis.get(expr))(
                rd => if (!(precRD subsetOf rd))
                        updateExprRd(expr, rd, precRD, analysis)
                      else
                        (rd, analysis)
              , acc
              )
        , (expr, _, _) => // application
            option.cata(analysis.get(expr))(
                rd => if (!(precRD subsetOf rd))
                        updateExprRd(expr, rd, precRD, analysis)
                      else
                        (rd, analysis)
              , acc
              )
        , (_, _, call) => { // return
            val cf.Call(expr, _, _) = call
            option.cata(analysis.get(expr))(
                rd => (rd, analysis)
              , acc
              )
          }
        , const3 // cond
        , const2 // branch
        , const  // jump
        , const  // done
        , n
      )
    }

    def initNodeRD(acc : (RD, RDMap), n : cf.Node[_,_]) : (RD, RDMap) = {
      val (precRD, analysis) = acc
      val const : Any => (RD, RDMap) = Function.const(acc)
      val const2 : (Any, Any) => (RD, RDMap) = (_, _) => acc
      val const3 : (Any, Any, Any) => (RD, RDMap) = (_, _, _) => acc

      cf.Node.cata(
          const  // label
        , const3 // pattern
        , expr =>   // expression
            option.cata(analysis.get(expr))(
                rd => updateExprRd(expr, rd, precRD, analysis)
              , acc
              )
        , (expr, _, _) => // application
            option.cata(analysis.get(expr))(
                rd => updateExprRd(expr, rd, precRD, analysis)
              , acc
              )
        , (_, _, call) => { // return
            val cf.Call(expr, _, _) = call
            option.cata(analysis.get(expr))(
                rd => (rd, analysis)
              , acc
              )
          }
        , const3 // cond
        , const2 // branch
        , const  // jump
        , const  // done
        , n
      )
    }

    def updateBlockRD(updateNode : ((RD, RDMap), cf.Node[_,_]) => (RD, RDMap))(b : cf.Block[cf.Node, cf.C, cf.C], analysis : RDMap) : RDMap = {
      val firstRD = for (first <- cf.Block.sLabels(b).headOption;
                         rd <- analysis.get(first))
                    yield rd
      option.cata(firstRD)(
          rd => b.fold[(RD, RDMap)](updateNode, updateNode, updateNode, (rd, analysis))._2
        , analysis
        )
    }

    def step(x : (List[cfg.BEdge], RDMap)) : (List[cfg.BEdge], RDMap) = {
      val (w, analysis) = x
      val (source, target, _) :: es = w
      val st = for (s <- cfg.get(source);
                    t <- cfg.get(target);
                    last <- cf.Block.sLabels(s).lastOption;
                    first <- cf.Block.sLabels(t).headOption;
                    lRD <- analysis.get(last);
                    fRD <- analysis.get(first))
               yield (t, first, last, lRD, fRD)
      st match {
        case Some((targetBlock, first, last, beforeLast, beforeFirst)) => 
          val afterLast = transfer(last, beforeLast)
          if (!(afterLast subsetOf beforeFirst)) {
            val beforeFirstUpdated : RD = beforeFirst ++ afterLast
            val analUpdated : RDMap = updateBlockRD(updateNodeRD)(targetBlock, analysis.updated(first, beforeFirstUpdated))
            val edgesToUpdate : List[cfg.BEdge] = targetBlock.successors map { case (l, tag) => (target, l, tag) }
            (edgesToUpdate ++ es, analUpdated)
          } 
          else
            (es, analysis)
        case None => 
          (es, analysis)
      }
    }

    def empty(x : (List[cfg.BEdge], RDMap)) : Boolean = x._1.isEmpty

    val workingList : List[cfg.BEdge] = cfg.flow
    val sLabels : List[SLabel] = cfg.sLabels
    val initRD : RD = Set.empty
    val analysisEmpty : RDMap = sLabels.zip(List.fill(sLabels.size)(initRD)).toMap
    val analysisInit : RDMap = cfg.traverse(updateBlockRD(initNodeRD), analysisEmpty)

    val (_, analysis) = parscala.Control.until(empty, step, (workingList, analysisInit))
    new ReachingDefinition(analysis, cfg)
  }
}

/**
 * Reaching definition stores that which assignments may reach
 * a given point of the program.
 *
 * See Flemming Nielson, Hanne Riis Nielson, Chris Hankin:
 * 'Principles of Program Analysis' section 2.1
 */
class ReachingDefinition private (val rd : Map[SLabel, ReachingDefinition.RD], val cfg : cf.CFGraph) {
  def get(l : SLabel) : ReachingDefinition.RD = rd.get(l).getOrElse(Set.empty)

  def toDotEdges : List[dot.DotEdge] =
    rd.foldLeft(List.empty[dot.DotEdge]) { (acc, kv) =>
      val (expression, reachingAssignments) = kv
      reachingAssignments.foldLeft(acc) { (acc2, reachingAssignment) => 
        val (_, assignment) = reachingAssignment
        val edge : dot.DotEdge = dot.DotEdge(dot.DotNode(assignment.toString), dot.DotNode(expression.toString)) !! dot.DotAttr.label("reach") !! dot.DotAttr.color(dot.Color.Red) !! dot.DotAttr.fontColor(dot.Color.Red)
        edge :: acc2
      }
    }
}
