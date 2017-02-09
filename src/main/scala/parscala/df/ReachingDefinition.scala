package parscala
package df

import parscala.{controlflow => cf}
import parscala.{tree => tr}
import parscala.dot

import scalaz.std.option

object ReachingDefinition {
  def apply(cfg : cf.CFGraph) : ReachingDefinition = {
    // Reaching definition: variables associated with point of assignment in the program
    type RD = Set[(Symbol, SLabel)]

    // Reaching definitionas that reaches the entry of each expression
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
          tr.Node.nodeCata(
              const2 // literal
            , const3 // identifier
            , (sl, symbol, _, _) => // valdef
                rd + ((symbol, sl))
            , (sl, lhs, _, _) => // assignment
                tr.Node.nodeCata(
                    const2 // literal
                  , (_, symbol, _) => // identifier
                      rd.filter {x : (Symbol, SLabel) => x._1 != symbol }.+((symbol, sl))
                  , const4 // valdef
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
                  , const2 // expr
                  , lhs)
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

    def updateNodeRD(n : cf.Node[_,_], acc : (RD, RDMap)) : (RD, RDMap) = {
      val (precRD, analysis) = acc
      val const0 : () => (RD, RDMap) = () => acc
      val const : Any => (RD, RDMap) = Function.const(acc)
      val const2 : (Any, Any) => (RD, RDMap) = (_, _) => acc
      val const3 : (Any, Any, Any) => (RD, RDMap) = (_, _, _) => acc

      cf.Control.nodeCata(
          const  // label
        , const3 // pattern
        , l =>   // expression
            option.cata(analysis.get(l))(
                rd =>
                  if (!(precRD subsetOf rd)) {
                    val before : RD = rd ++ precRD
                    val after : RD = transfer(l, before)
                    (after, analysis.updated(l, before))
                  }
                  else
                    (rd, analysis)
              , acc
              )
        , const3 // cond
        , const2 // branch
        , const  // jump
        , const0 // done
        , n
      )
    }

    def initNodeRD(n : cf.Node[_,_], acc : (RD, RDMap)) : (RD, RDMap) = {
      val (precRD, analysis) = acc
      val const0 : () => (RD, RDMap) = () => acc
      val const : Any => (RD, RDMap) = Function.const(acc)
      val const2 : (Any, Any) => (RD, RDMap) = (_, _) => acc
      val const3 : (Any, Any, Any) => (RD, RDMap) = (_, _, _) => acc

      cf.Control.nodeCata(
          const  // label
        , const3 // pattern
        , l =>   // expression
            option.cata(analysis.get(l))(
                rd => {
                    val before : RD = rd ++ precRD
                    val after : RD = transfer(l, before)
                    (after, analysis.updated(l, before))
                }
              , acc
              )
        , const3 // cond
        , const2 // branch
        , const  // jump
        , const0 // done
        , n
      )
    }

    def updateBlockRD(updateNode : (cf.Node[_,_], (RD, RDMap)) => (RD, RDMap))(b : cf.Block[cf.Node, cf.C, cf.C], analysis : RDMap) : RDMap = {
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

  def toDot(rd : ReachingDefinition) : dot.DotGraph = {
    val edges : List[dot.DotEdge] = rd.rd.foldLeft(List.empty[dot.DotEdge]) { (acc, kv) => {
        val (expression, reachingAssignments) = kv
        reachingAssignments.foldLeft(acc) { (acc2, reachingAssignment) => {
            val (_, assignment) = reachingAssignment
            val edge : dot.DotEdge = dot.DotEdge(dot.DotNode(assignment.toString), dot.DotNode(expression.toString)) !! dot.DotAttr.label("reach")
            edge :: acc2
          }
        }
      }
    }
  dot.DotGraph("Reaching definitions", List.empty, edges)
  }
}

/**
 * Reaching definition stores that which assignments may reach
 * a given point of the program.
 *
 * See Flemming Nielson, Hanne Riis Nielson, Chris Hankin:
 * 'Principles of Program Analysis' section 2.1
 */
class ReachingDefinition(val rd : Map[SLabel, Set[(Symbol, SLabel)]], val cfg : cf.CFGraph) {
  def get(l : SLabel) : Option[Set[(Symbol, SLabel)]] = rd.get(l)
}
