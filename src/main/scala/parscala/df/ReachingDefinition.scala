package parscala
package df

import parscala.controlflow.CFGraph

object ReachingDefinition {
  def apply(cfg : CFGraph) : ReachingDefinition = {
    type RD = Set[(Symbol, SLabel)]
    type Store = Map[SLabel, RD]
/*
    def transfer(l : SLabel, rd : RD) : RD = {
      val const : Any => RD = Function.const(rd)
      tree.Control.exprCata(const, //tuple
                            const, //newE
                            const, //sel
                            const, //app
                            (lhs, _) => // id
                              tree.Control.exprCata(const, //tuple
                                                    const, //newE
                                                    const, //sel
                                                    const, //app
                                                    
lhs)
                              rd filter (_._1 != symbol) + ((symbol, l)),
                            (_, symbol, _) =>
                              rd filter (_._1 != symbol) + ((symbol, l)),
                            const, //other
                            cfg(l))
    }
*/
    def step(x : (List[cfg.BEdge], Store)) : (List[cfg.BEdge], Store) = {
      val (w, analysis) = x
      x
    }

    def empty(x : (List[cfg.BEdge], Store)) : Boolean = x._1.isEmpty

    val w : List[cfg.BEdge] = cfg.flow
    val extremal : List[(Symbol, SLabel)] = List.empty
    val initRD = Set.empty[(Symbol, SLabel)]
    val sLabels : List[SLabel] = cfg.sLabels
    val analysisInit : Store = sLabels.zip(List.fill(sLabels.size)(initRD)).toMap
    val (_, analysis) = Control.until(empty, step, (w, analysisInit))
    new ReachingDefinition(analysis)
  }

}

/**
 * Reaching definition stores that which assignments may reach
 * a given point of the program.
 *
 * See Flemming Nielson, Hanne Riis Nielson, Chris Hankin:
 * 'Principles of Program Analysis' section 1.2
 */
class ReachingDefinition(val rd : Map[SLabel, Set[(Symbol, SLabel)]])
