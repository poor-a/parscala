package parscala

import controlflow.{CFGraph, Block, Node, C}

import scalaz.{State, IndexedStateT, IndexedState, Traverse}
import scalaz.syntax.bind.ToBindOpsUnapply // >>= and >>

object MapLike {
  def isMapLike(m : DLabel, cfg : CFGraph) : Boolean =
    traverseExecutionPaths(m, cfg)

  private type St = (Int, BLabel, BLabel, Map[BLabel, Int])

  private def successors(b : Block[Node, C, C], end : BLabel) : List[BLabel] = {
    if (b.entryLabel == end)
      List()
    else {
      lazy val succ : List[BLabel] = b.successors.map(_._1)
      Node.OCCata( (_, _, _) => succ  // pattern
                 , (_, _, returnPoint) => List(returnPoint)  // call
                 , (_, _, _) => succ // cond
                 , (_, _) => succ // branch
                 , (_) => succ // jump
                 , (_) => succ // done
                 , Block.lastNode(b)
                 )
      }
  }

  private def countRecursiveCall(b : Block[Node, C, C]) : State[St, Unit] = {
    val void : State[St, Unit] = IndexedStateT.stateMonad.pure(())
    val cVoid : Any => State[St, Unit] = Function.const(void)
    val c2Void : (Any, Any) => State[St, Unit] = Function.const2(void)
    val c3Void : (Any, Any, Any) => State[St, Unit] = Function.const3(void)
    Node.OCCata( c3Void       // pattern
               , (_, targets, _) => // call
                   for (s <- IndexedState.get[St];
                        m = s._2;
                        _ <- IndexedState.modify[St](st => if (targets contains m) {println("found a call!"); st.copy(_1 = st._1 +1)} else {println("no recursive call here " + targets); st} ))
                   yield ()
               , c3Void       // cond
               , c2Void       // branch
               , cVoid        // jump
               , cVoid        // done
               , Block.lastNode(b)
               )
  }

  type T[A] = State[St, A]

  private def resetCounter(s : St, n : Int) : St = s.copy(_1 = n)

  private def resetVisited(s : St, visited : Map[BLabel, Int]) : St =
    s.copy(_4 = visited)

  private def markAsVisited(l : BLabel) : State[St, Unit] =
    IndexedState.modify[St](s => s.copy(_4 = s._4 + ((l, s._1))))

  private def isVisited(l : BLabel) : State[St, Boolean] =
    for (visited <- IndexedState.gets[St, Map[BLabel, Int]](_._4))
    yield visited contains l

  private def callsUntil(l : BLabel) : State[St, Option[Int]] =
    State.gets(_._4.get(l))

  private def callsSoFar : State[St, Int] = State.gets(_._1)

  private def traverseFrom(l : BLabel, cfg : CFGraph) : State[St, Boolean] = {
    println("traversing " + l)
    isVisited(l) >>= (visited =>
      if (!visited)
        markAsVisited(l) >> (
        cfg.get(l) match {
          case Some(b) =>
            val listTraverse : Traverse[List] = scalaz.std.list.listInstance
            for (_ <- countRecursiveCall(b);
                 recursiveCalls <- callsSoFar;
                 s <- IndexedState.get[St];
                 allPathsEligible <- listTraverse.traverse[T, BLabel, Boolean](successors(b, s._3))
                        ((succ : BLabel) => for (s2 <- IndexedState.get[St];
                                                 _ <- IndexedState.put[St](resetVisited(resetCounter(s2, s._1), s._4));
                                                 pathEligible <- traverseFrom(succ, cfg))
                                            yield pathEligible
                        )(IndexedStateT.stateMonad))
            yield { println("found " + s._1 + " calls in " + b.entryLabel); 
                    if (allPathsEligible.isEmpty) recursiveCalls <= 1 else !(allPathsEligible contains false); }
          case None => IndexedStateT.stateMonad.pure(false)
        })
      else 
        for (n <- callsSoFar;
             mCalls <- callsUntil(l))
        yield mCalls match {
                case Some(calls) => n - calls == 0 && n <= 1
                case _ => throw new IllegalArgumentException("non-existent node: " + l)
              }
      )
  }

  private def traverseExecutionPaths(m : DLabel, cfg : CFGraph) : Boolean = {
    cfg.methods.get(Left(m)) match {
      case Some((start, end)) => 
        traverseFrom(cfg.start.entryLabel, cfg).eval((0, start, end, Map()))
      case _ => 
        System.err.println("No such method.")
        false
    }

  }
}
