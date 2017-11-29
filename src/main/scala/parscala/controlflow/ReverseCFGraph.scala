package parscala
package controlflow

class ReverseCFGraph(val g : CFGraph, val edges : List[(BLabel,BLabel,EdgeLabel.TagType)]) {
  private val start : BLabel = g.done.entryLabel

  def dominators() : Map[BLabel, Set[BLabel]] = {
    val labels : Set[BLabel] = g.graph.keySet
    val domInit : Map[BLabel, Set[BLabel]] = labels.toIterator.map{(_,labels)}.toMap.updated(start, Set(start))
    val noStart : Set[BLabel] = labels - start

    def approximate(acc : (Map[BLabel, Set[BLabel]], Boolean)) : (Map[BLabel, Set[BLabel]], Boolean) = {
      noStart.foldLeft((acc._1, false)){(ac, l) => 
        val (dom, changed) = ac
        val d : Set[BLabel] = pred(l).foldLeft(labels){(a, pred) => a & dom(pred)} + l
        if (d != dom(l))
          (dom.updated(l, d), true)
        else
          (dom, changed)
      }
    }
    def noChange[T](x : (T, Boolean)) : Boolean = !x._2

    parscala.Control.until(noChange, approximate, (domInit, true))._1
  }

  def immediateDominators(domin : Map[BLabel, Set[BLabel]]) : DomTree = {
    val initIdom : Map[BLabel, Set[BLabel]] = (domin.toIterator map {x => (x._1, x._2 - x._1)}).toMap
    val noStart : Set[BLabel] = g.graph.keySet - start
    val idom : Map[BLabel, Set[BLabel]] = noStart.foldLeft(initIdom){(idom, l) =>
      idom(l).foldLeft(idom){(idomm, dom) =>
        val others : Set[BLabel] = idomm(l) - dom
        others.foldLeft(idomm){(idommm, otherDom) =>
          if (idommm(dom)(otherDom))
            idommm.updated(l, idommm(l) - otherDom)
          else
            idommm
        }
      }
    }
    
    new DomTree(idom mapValues (_.headOption))
  }

  def pred(l : BLabel) : List[BLabel] =
    for ((s, t, _) <- edges; if (l == t)) yield s

  type CEdge = (BLabel, BLabel, EdgeLabel.TagType)

  def controlDependency() : List[CEdge] = {
    type Path = List[BLabel]

    def walkUp(from : BLabel, to : BLabel, t : DomTree) : Path = {
      def reachedOrTop(x : (Path, BLabel)) : Boolean =
        t.parent(x._2).isEmpty || x._2 == to

      def step(x : (Path, BLabel)) : (Path, BLabel) = {
        val (p, l) = x
        (l :: p, t.parent(l).get)
      }

      parscala.Control.until(reachedOrTop, step, (List.empty, from))._1
    }

    def candidates(b : Block[Node,C,C], xs : List[CEdge]) : List[CEdge] = {
      val bl : BLabel = b.entryLabel
      b.successors match {
        case List((x, xTag), (y, yTag)) => (bl, x, xTag) :: (bl, y, yTag) :: xs
        case _ => xs
      }
    }
    
    val idom : DomTree = immediateDominators(dominators)
    val es : List[CEdge] = g.traverse(candidates, List.empty)
    es flatMap { case (a,b,tag) => 
      val to : BLabel = idom.parent(a) getOrElse a
      walkUp(b, to, idom).map{l => (a, l, tag)}
    }
  }

  def flow : List[g.BEdge] = edges
}
