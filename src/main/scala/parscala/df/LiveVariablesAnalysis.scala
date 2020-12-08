package parscala
package df

import parscala.{controlflow => cf}

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
          node.cata(
              const3 // literal
            , (_, _, symbol, _) => // identifier
                live + symbol
//            , (_, pat, _, _) => { // pattern definition
//                live -- tr.Pat.identifiers(pat)
//            }
            , (_, lhs, _, _) => // assignment
                lhs.cata(
                    const3 // literal
                  , (_, _, symbol, _) => // identifier
                      live - symbol
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

    def updateIfObsolete(l : SLabel, exitLV : Set[LV], succEntryLV : Set[LV], analysis : LVMap) : (Set[LV],LVMap) = 
      if (!(succEntryLV subsetOf exitLV)) {
        val exitLVUpdated : Set[LV] = exitLV ++ succEntryLV
        val entryLVUpdated : Set[LV] = transfer(l, exitLVUpdated)
        (entryLVUpdated, analysis.updated(l, exitLVUpdated))
      } else
        (exitLV, analysis)

    def updateNodeLV(n : cf.Node[_,_], acc : (Set[LV], LVMap)) : (Set[LV], LVMap) = {
      val const : Any => (Set[LV], LVMap) = Function.const(acc)
      val const2 : (Any, Any) => (Set[LV], LVMap) = Function.const2(acc)
      val const3 : (Any, Any, Any) => (Set[LV], LVMap) = Function.const3(acc)

      val (succEntryLV, analysis) = acc
      cf.Node.cata(
          const  // label
        , const3 // pattern
        , expr =>  // expression
            option.cata(analysis.get(expr))(
                exitLV =>
                  updateIfObsolete(expr, exitLV, succEntryLV, analysis)
              , {
                  val entryLV : Set[LV] = transfer(expr, succEntryLV)
                  (entryLV, analysis.updated(expr, succEntryLV))
                }
              )
        , const // definition
        , (expr, _, _) => // application
            option.cata(analysis.get(expr))(
                exitLV =>
                  updateIfObsolete(expr, exitLV, succEntryLV, analysis)
              , {
                  val entryLV : Set[LV] = transfer(expr, succEntryLV)
                  (entryLV, analysis.updated(expr, succEntryLV))
                }
              )
        , const3 // return
        , const3 // cond
        , const2 // branch
        , const  // jump
        , const  // done
        , n
        )
    }

    def updateBlockLV(updateNode : (cf.Node[_,_], (Set[LV], LVMap)) => (Set[LV], LVMap))(b : cf.Block[cf.Node, cf.C, cf.C], succEntryLV : Set[LV], analysis : LVMap) : LVMap =
      b.foldRight[(Set[LV], LVMap)](updateNode, updateNode, updateNode, (succEntryLV, analysis))._2

    def step(x : (List[cfg.BEdge], LVMap)) : (List[cfg.BEdge], LVMap) = {
      val ((source, target, _) :: es, analysis) = x
      val st = for (s <- cfg.get(source);
                    t <- cfg.get(target);
                    tLast <- cf.Block.sLabels(t).lastOption;
                    sFirst <- cf.Block.sLabels(s).headOption;
                    tLastExitLV <- analysis.get(tLast);
                    sFirstExitLV <- analysis.get(sFirst)
                   )
               yield (t, sFirst, tLastExitLV, sFirstExitLV)
      st match {
        case Some((targetBlock, sFirst, tExitLV, sExitLV)) =>
          val sEntryLV : Set[LV] = transfer(sFirst, sExitLV)
          if (!(sEntryLV subsetOf tExitLV)) {
            val analUpdated : LVMap = updateBlockLV(updateNodeLV)(targetBlock, sEntryLV, analysis)
            val edgesToUpdate : List[cfg.BEdge] = cfg.precedessors(target) map { case (l, tag) => (target, l, tag) }
            (edgesToUpdate ++ es, analUpdated)
          } else
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
class LiveVariablesAnalysis private (val lv : LiveVariablesAnalysis.LVMap, val cfg : cf.CFGraph) {
  def get : SLabel => Option[Set[LiveVariablesAnalysis.LV]] = lv.get
}
