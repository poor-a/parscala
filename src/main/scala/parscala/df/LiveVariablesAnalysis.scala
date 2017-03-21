package parscala
package df

import parscala.{controlflow => cf}
import parscala.{tree => tr}
import parscala.dot

import scalaz.std.option

object LiveVariablesAnalysis {
  // Live variable: a variable used at a later point in the program
  type LV = Symbol

  // Live variables at program points
  type LVMap = Map[SLabel, Set[LV]]

  def fromCFGraph(cfg : cf.CFGraph) : LiveVariablesAnalysis = {
    def transfer(l : SLabel, live : Set[LV]) : Set[LV] =
      cfg(l) match {
        case None => 
          live
        case Some(node) =>
          val const2 : (Any, Any) => Set[LV] = (_, _) => live
          val const3 : (Any, Any, Any) => Set[LV] = (_, _, _) => live
          val const4 : (Any, Any, Any, Any) => Set[LV] = (_, _, _, _) => live
          val const5 : (Any, Any, Any, Any, Any) => Set[LV] = (_, _, _, _, _) => live
          tr.Node.nodeCata(
              const3 // literal
            , const3 // identifier
            , const4 // pattern definition
            , (sl, lhs, _, _) => // assignment
                tr.Node.nodeCata(
                    const3 // literal
                  , (_, symbol, _) => // identifier
                      live - symbol
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

    def updateNodeLV(n : cf.Node[_,_], acc : (Set[LV], LVMap)) : (Set[LV], LVMap) = {
      val (succEntryLV, analysis) = acc
      val const0 : () => (Set[LV], LVMap) = () => acc
      val const : Any => (Set[LV], LVMap) = Function.const(acc)
      val const2 : (Any, Any) => (Set[LV], LVMap) = (_, _) => acc
      val const3 : (Any, Any, Any) => (Set[LV], LVMap) = (_, _, _) => acc

      cf.Control.nodeCata(
          const  // label
        , const3 // pattern
        , l =>   // expression
            option.cata(analysis.get(l))(
                exitLV =>
                  if (!(succEntryLV subsetOf exitLV)) {
                    val exitLVUpdated : Set[LV] = exitLV ++ succEntryLV
                    val entryLVUpdated : Set[LV] = transfer(l, exitLVUpdated)
                    (entryLVUpdated, analysis.updated(l, exitLVUpdated))
                  }
                  else
                    (exitLV, analysis)
              , acc
              )
        , const3 // cond
        , const2 // branch
        , const  // jump
        , const0 // done
        , n
      )
    }

    def updateBlockLV(updateNode : (cf.Node[_,_], (Set[LV], LVMap)) => (Set[LV], LVMap))(b : cf.Block[cf.Node, cf.C, cf.C], analysis : LVMap) : LVMap = {
      val firstLV : Option[Set[LV]] = for (
          first <- cf.Block.sLabels(b).headOption;
          rd <- analysis.get(first))
        yield rd
      option.cata(firstLV)(
          live => b.foldRight[(Set[LV], LVMap)](updateNode, updateNode, updateNode, (live, analysis))._2
        , analysis
        )
    }

    def step(x : (List[cfg.BEdge], LVMap)) : (List[cfg.BEdge], LVMap) = {
      val (w, analysis) = x
      val (source, target, _) :: es = w
      val st = for (s <- cfg.get(source);
                    t <- cfg.get(target);
                    tLast <- cf.Block.sLabels(t).lastOption;
                    sFirst <- cf.Block.sLabels(s).headOption;
                    tLastExitLV <- analysis.get(tLast);
                    sFirstExitLV <- analysis.get(sFirst))
               yield (t, tLast, sFirst, tLastExitLV, sFirstExitLV)
      st match {
        case Some((targetBlock, tLast, sFirst, tExitLV, sExitLV)) =>
          val sEntryLV : Set[LV] = transfer(sFirst, sExitLV)
          if (!(sEntryLV subsetOf tExitLV)) {
            val tExitLVUpdated : Set[LV] = sEntryLV ++ tExitLV
            val analUpdated : LVMap = updateBlockLV(updateNodeLV)(targetBlock, analysis.updated(tLast, tExitLVUpdated))
            val edgesToUpdate : List[cfg.BEdge] = targetBlock.successors map { case (l, tag) => (target, l, tag) }
            (edgesToUpdate ++ es, analUpdated)
          } 
          else
            (es, analysis)
        case None => 
          (es, analysis)
      }
    }

    def empty(x : (List[cfg.BEdge], LVMap)) : Boolean = x._1.isEmpty

    val workingList : List[cfg.BEdge] = cfg.reverse.flow
    val sLabels : List[SLabel] = cfg.sLabels
    val initRD : Set[LV] = Set.empty
    val analysisInit : LVMap = sLabels.zip(List.fill(sLabels.size)(initRD)).toMap

    val (_, analysis) = parscala.Control.until(empty, step, (workingList, analysisInit))
    new LiveVariablesAnalysis(analysis, cfg)
  }
}

/**
 * Stores that which variables are live at a given point of the
 * program. A variable is live at a program point if it is used at one
 * point later in the program.
 *
 * See Flemming Nielson, Hanne Riis Nielson, Chris Hankin:
 * 'Principles of Program Analysis' section 2.1
 */
class LiveVariablesAnalysis private (val lv : LiveVariablesAnalysis.LVMap, cfg : cf.CFGraph) {
  def get : SLabel => Option[Set[LiveVariablesAnalysis.LV]] = lv.get
}
