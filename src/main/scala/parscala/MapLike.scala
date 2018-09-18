package parscala

import controlflow.{CFGraph, Block, Node, C, O}

import scalaz.{State, IndexedStateT, IndexedState, Traverse}

object MapLike {
  def isMapLike(cfg : CFGraph) : Boolean = {
    traverseExecutionPaths(cfg)
    true

  }

  private def traverseBlock(b : Block[Node, C, C]) : State[Int, Unit] = {
    def noop(m : State[Int, Unit], n : Node[_, _]) : State[Int, Unit] = m
    val void : State[Int, Unit] = IndexedStateT.stateMonad.pure(())
    def countCalls(m : State[Int, Unit], n : Node[O, C]) : State[Int, Unit] = {
      val cVoid : Any => State[Int, Unit] = Function.const(void)
      val c2Void : (Any, Any) => State[Int, Unit] = Function.const2(void)
      val c3Void : (Any, Any, Any) => State[Int, Unit] = Function.const3(void)
      val nAction : State[Int, Unit] = 
	Node.OCCata( c3Void       // pattern
                 , (_, _, _) => // call
                     IndexedState.modify[Int](_ + 1)
                 , c3Void       // cond
                 , c2Void       // branch
                 , cVoid        // jump
                 , cVoid        // done
                 , n
                 )
      for (_ <- m; _ <- nAction) yield ()
    }

    b.fold(noop, noop, countCalls, void)
  }

  type T[A] = State[Int, A]

  private def traverseFrom(l : BLabel, cfg : CFGraph) : State[Int, Unit] = {
    cfg.get(l) match {
      case Some(b) =>
    val listTraverse : Traverse[List] = scalaz.std.list.listInstance
    for (_ <- traverseBlock(b);
         s <- IndexedState.get[Int];
         _ <- listTraverse.traverse[T, BLabel, Unit](b.successors.map(_._1))
                             ((succ : BLabel) => for (_ <- IndexedState.put[Int](s);
                                          _ <- traverseFrom(succ, cfg))
                                     yield ()
                             )(IndexedStateT.stateMonad))
    yield ()
    case None => IndexedStateT.stateMonad.pure(())
    }
  }

  private def traverseExecutionPaths(cfg : CFGraph) : Unit = {
    val start : Block[Node,C,C] = cfg.start
    println(traverseFrom(start.entryLabel, cfg).exec(0))
  }
}
