package parscala.dot

import parscala.Control.foldM_

import scalaz.{State, MonadState, IndexedStateT}

object DotGen {
    type St = (List[DotNode], List[DotEdge])
    type DotGen[A] = State[St, A]

    val dotGenState : MonadState[DotGen, St] = IndexedStateT.stateMonad

    def run[A](gen : DotGen[A]) : (St, A) = gen.run((List(), List()))

    def exec(gen : DotGen[_]) : St = gen.exec((List(), List()))

    def node(node : DotNode) : DotGen[DotNode] = 
      for (_ <- dotGenState.modify{ case (ns, es) => (node :: ns, es) })
      yield node

    def edge(source : DotNode, target : DotNode, label : String) : DotGen[Unit] =
      dotGenState.modify{ case (ns, es) => (ns, (DotEdge(source, target) !! DotAttr.label(label)) :: es) }

    def graph(g : DotGraph) : DotGen[Unit] =
      dotGenState.modify{ case (ns, es) => (g.blocks ++: ns, g.edges ++: es) }

    def enum(parent : DotNode, children : List[DotNode], eLabelTemplate : String => String) : DotGen[Unit] = 
      foldM_[DotGen, Int, DotNode]((i : Int, child : DotNode) =>
                for (_ <- edge(parent, child, eLabelTemplate(i.toString))) yield i + 1,
             0,
             children)

    def deepEnum(parent : DotNode, children : List[List[DotNode]], eLabelTemplate : (String, String) => String) : DotGen[Unit] =
      foldM_[DotGen, Int, List[DotNode]]((i, chldrn) =>
                for (_ <- enum(parent, chldrn, eLabelTemplate(i.toString, _))) yield i + 1,
            0,
            children)

}
