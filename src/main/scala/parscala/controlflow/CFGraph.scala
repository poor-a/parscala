package parscala
package controlflow

import scala.collection.immutable.Stream

import scalaz.{State, EitherT, Monoid, \/-}
import scalaz.syntax.bind._

import tree._
import parscala.Control.foldM
import parscala.{tree => tr}

abstract class O
abstract class C

object CFGraph {
  def apply(m : Method) : Option[CFGraph] =
    mkExtCFGraph(m).map(_.freeze)

  type St = (Stream[BLabel], Stream[SLabel], CFGraph)
  type CFGGen[A] = State[St, A]
  type CFGAnalyser[A] = EitherT[CFGGen, Unit, A]

  def nLabel() : CFGGen[Label] =
    for (b <- genBLabel)
    yield Label(b)

  def nCond(expr : SLabel, t : BLabel, f : BLabel) : Cond = 
    Cond(expr, t, f)

  def nJump(target : BLabel) : Jump = 
    Jump(target)

  def genBLabel : CFGGen[BLabel] =
    for (l <- State.gets[St, BLabel](_._1.head);
         _ <- State.modify[St]{ case (bGen, sGen, graph) => (bGen.tail, sGen, graph) })
    yield l

  def genSLabel : CFGGen[SLabel] =
    for (l <- State.gets[St, SLabel](_._2.head);
         _ <- State.modify[St]{ case (bGen, sGen, graph) => (bGen, sGen.tail, graph) })
    yield l

  def modifyGraph(f : CFGraph => CFGraph) : CFGGen[Unit] = 
    State.modify[St]{ case (bGen, sGen, graph) => (bGen, sGen, f(graph)) }

  def liftM[A](m : CFGGen[A]) : CFGAnalyser[A] = EitherT.eitherTHoist.liftM(m)

  def pure[A](x : A) : CFGAnalyser[A] = EitherT.eitherTMonad[CFGGen, Unit].pure(x)

  def mkExtCFGraph(m : Method) : Option[ExtensibleCFGraph] = {
    val bGen : BLabelGen = BLabel.stream
    val sGen : SLabelGen = SLabel.stream
    val (List(fst,s,d), bGen2) = ((bGen take 3).toList, bGen drop 3)

    val start = BCat(emptyBlockWithLabel(s), BLast(Jump(fst)))
    val done = BCat(emptyBlockWithLabel(d), BLast(Done()))
    val first = emptyBlockWithLabel(fst)

    m.nodes match {
      case Some(nodes) => 
        val graph = new CFGraph(start, done, nodes)
        cfgStmts(first, d, nodes.root).run.run((bGen2, sGen, graph)) match {
          case ((bGen3, sGen2, cfg), \/-(block)) => 
            val flushed = BCat(block, BLast(Jump(d)))
            Some(new ExtensibleCFGraph(cfg + flushed, bGen3, sGen2))
          case ((bGen3, sGen2, cfg), _) =>
            Some(new ExtensibleCFGraph(cfg, bGen3, sGen2))
        }
        
      case None =>
        None
    }
  }

  def emptyBlockWithLabel(b : BLabel) : Block[Node, C, O] = 
    BFirst(Label(b))

  def emptyBlock : State[St, Block[Node, C, O]] = 
    for (l <- genBLabel) yield emptyBlockWithLabel(l)

  implicit def unitMonoidInstance : Monoid[Unit] = Monoid.instance((_, _) => Unit, Unit)
    
  def cfgStmts(b : Block[Node,C,O], abruptNext : BLabel, node : tr.Node) : CFGAnalyser[Block[Node,C,O]] = {
    def step(acc : Block[Node,C,O], e : tr.Node) : CFGAnalyser[Block[Node,C,O]] =
      cfgStmts(acc, abruptNext, e)

    def deepStep(acc : Block[Node,C,O], xs : List[tr.Node]) : CFGAnalyser[Block[Node,C,O]] =
      foldM(step, acc, xs)

    Node.nodeCata(
        (l, _) => { // literal
          val literal = Expr(l)
          pure(BCat(b, BMiddle(literal)))
        }
      , (l, _, _) => { // identifier
          val identifier = Expr(l)
          pure(BCat(b, BMiddle(identifier)))
        }
      , (l, _, rhs, _) => // val or var def
          for (rhsEvaled <- cfgStmts(b, abruptNext, rhs))
          yield BCat(rhsEvaled, BMiddle(Expr(l)))
      , (l, lhs, rhs, _) => // assignment
          for (lhsEvaled <- cfgStmts(b, abruptNext, lhs);
               rhsEvaled <- cfgStmts(lhsEvaled, abruptNext, rhs))
          yield BCat(rhsEvaled, BMiddle(Expr(l)))
      , (l, method, args, _) => // application
          for (mEvaled <- cfgStmts(b, abruptNext, method);
               argsEvaled <- foldM(deepStep, mEvaled, args))
          yield BCat(argsEvaled, BMiddle(Expr(l)))
      , (l, constr, args, _) => // new
          for (argsEvaled <- foldM(deepStep, b, args))
          yield BCat(argsEvaled, BMiddle(Expr(l)))
      , (l, expr, tname, _) => // selection
          for (exprEvaled <- cfgStmts(b, abruptNext, expr))
          yield BCat(exprEvaled, BMiddle(Expr(l)))
      , (l, _, _) => // qualified this
          pure(BCat(b, BMiddle(Expr(l))))
      , (l, components, t) => // tuple
          for (compsEvaled <- foldM(step, b, components))
          yield BCat(compsEvaled, BMiddle(Expr(l)))
      , (l, p, t, _) => // if-then
          for (tBranch <- liftM(emptyBlock);
               succ <- liftM(emptyBlock);
               pEvaled <- cfgStmts(b, abruptNext, p);
               tEvaled <- cfgStmts(tBranch, abruptNext, t);
               cond = Cond(p.label, tBranch.entryLabel, succ.entryLabel);
               flushed = BCat(pEvaled, BLast(cond));
               tFlushed = BCat(tEvaled, BLast(Jump(succ.entryLabel)));
               _ <- liftM(modifyGraph{ _ + flushed + tFlushed}))
          yield succ
      , (l, p, t, f, _) => // if-then-else
          for (tBranch <- liftM(emptyBlock);
               fBranch <- liftM(emptyBlock);
               succ <- liftM(emptyBlock);
               pEvaled <- cfgStmts(b, abruptNext, p);
               tEvaled <- cfgStmts(tBranch, abruptNext, t);
               fEvaled <- cfgStmts(fBranch, abruptNext, f);
               cond = Cond(p.label, tBranch.entryLabel, fBranch.entryLabel);
               tFlushed = BCat(tEvaled, BLast(Jump(succ.entryLabel)));
               fFlushed = BCat(fEvaled, BLast(Jump(succ.entryLabel)));
               flushed = BCat(pEvaled, BLast(cond));
               _ <- liftM(modifyGraph{ _ + flushed + tFlushed + fFlushed }))
          yield succ
      , (l, p, body, _) => // while loop
          for (succ <- liftM(emptyBlock);
               bodyEmpty <- liftM(emptyBlock);
               testPBegin <- liftM(emptyBlock);
               pEvaled <- cfgStmts(testPBegin, abruptNext, p);
               bodyEvaled <- cfgStmts(bodyEmpty, abruptNext, body);
               cond = Cond(p.label, bodyEvaled.entryLabel, succ.entryLabel);
               testP = BCat(pEvaled, BLast(cond));
               flushed = BCat(b, BLast(Jump(testP.entryLabel)));
               bodyFlushed = BCat(bodyEvaled, BLast(Jump(testP.entryLabel)));
               _ <- liftM(modifyGraph{ _ + flushed + bodyFlushed + testP }))
          yield succ
      , (l, enums, body, _) => pure(b) // for loop
      , (l, enums, body, _) => pure(b) // for-yield loop
      , (l, expr, _) => // return
          cfgStmts(b, abruptNext, expr) >>= (exprEvaled => {
            val flushed = BCat(exprEvaled, BLast(Jump(abruptNext)))
            liftM (modifyGraph { _ + flushed }) >> 
            EitherT.eitherTMonadError[CFGGen, Unit].raiseError(())
          })
      , (l, expr, _) => // throw
          cfgStmts(b, abruptNext, expr) >>= (exprEvaled => {
            val flushed = BCat(exprEvaled, BLast(Jump(abruptNext)))
            liftM(modifyGraph { _ + flushed }) >> 
            EitherT.eitherTMonadError[CFGGen, Unit].raiseError(())
          })
      , (l, exprs, _) => // block
          foldM(step, b, exprs)
      , (l, _) => // other expression
          pure(BCat(b, BMiddle(Expr(l))))
      , node)
/*
      case (x @ q"do $loopBody while ($p)") :: xs => 
        for (succ <- liftM(emptyBlock);
             body <- liftM(emptyBlock);
             testPBegin <- liftM(emptyBlock);
             pAnalysed <- g(p, testPBegin);
             withTest = pAnalysed._1;
             test = pAnalysed._2;
             cond <- liftM(nCond(test, body.entryLabel, succ.entryLabel, x));
             jump <- liftM(nJump(body.entryLabel));
             testP = BCat(withTest, BLast(cond));
             flushed = BCat(b, BLast(jump));
             _ <- liftM(modifyGraph{ _ + flushed + testP });
             _ <- cfgStmts(body, testP.entryLabel, abruptNext, expressions(loopBody), g);
             _ <- cfgStmts(succ, abruptNext, xs, g))
        yield ()
      case q"try $expr" :: xs =>
        cfgStmts(b, abruptNext, expressions(expr) ++ xs, g)
      case q"try $expr catch { case $_ }" :: xs => 
        cfgStmts(b, abruptNext, expressions(expr) ++ xs, g)
      case q"try $expr finally $fin" :: xs => 
        cfgStmts(b, abruptNext, expressions(expr) ++ expressions(fin) ++ xs, g)
      case q"try $expr catch { case ..$_ } finally $fin" :: xs => 
        cfgStmts(b, abruptNext, expressions(expr) ++ expressions(fin) ++ xs, g)
      case q"$expr match { case ..$cases }" :: xs =>
        for (succ <- liftM(emptyBlock);
             exprAnalysed <- g(expr, b);
             exprEvaled = exprAnalysed._1;
             done <- liftM(State.gets[St, BLabel](_._3.done));
             _ <- cfgCases(cases, exprEvaled, succ.entryLabel, abruptNext, g);
             _ <- cfgStmts(succ, abruptNext, xs, g))
        yield ()
      case (ret @ q"return $e") :: _ =>
        for ((eEvaled, eAnalysed) <- g(e, b);
             l <- liftM(genSLabel);
             _ <- liftM(modifyGraph{ graph => 
                    val flushed = BCat(eEvaled, BLast(NReturn(l, eAnalysed, graph.done, ret)))
                    graph + flushed }))
        yield ()
      case (thr @ q"throw $e") :: _ => { 
        for ((eEvaled, eAnalysed) <- g(e, b);
             l <- liftM(genSLabel);
             flushed = BCat(eEvaled, BLast(NThrow(l, eAnalysed, abruptNext, thr)));
             _ <- liftM(modifyGraph{ _ + flushed }))
        yield ()
      }
      case x :: xs if x.isInstanceOf[compiler.Block] =>
        cfgStmts(b, abruptNext, expressions(x) ++ xs, g)
      case x :: xs => {
        for ((xEvaled, _) <- g(x, b);
             _ <- cfgStmts(xEvaled, abruptNext, xs, g))
        yield ()
      }
      case List() => {
        for (jump <- liftM(genSLabel);
             flushed = BCat(b, BLast(NJump(jump, nextLabel)));
             _ <- liftM(modifyGraph{ _ + flushed }))
        yield ()}*/
//      , node)
    }
  }
/*
  def cfgCases(cases : List[compiler.CaseDef], current : Block[Node,C,O], next : BLabel, abruptNext : BLabel, g : (Tree, Block[Node,C,O]) => CFGAnalyser[(Block[Node,C,O], Node[O,O])] ) : CFGAnalyser[Unit] = {
    cases match {
      case List(c) => 
        c match {
          case cq"$pat => $res" =>
            for (t <- liftM(emptyBlock);
                 f <- liftM(emptyBlock);
                 p <- liftM(genSLabel);
                 exceptionL <- liftM(genSLabel);
                 cond <- liftM(genSLabel);
                 error = BCat(f, BLast(new NException(exceptionL, classOf[MatchError], abruptNext)));
                 flushed = BCat(current, BLast(NPattern(p, pat, t.entryLabel, f.entryLabel)));
                 _ <- liftM(modifyGraph{ _ + flushed + error });
                 _ <- cfgStmts(t, next, abruptNext, expressions(res), g))
            yield ()
          case cq"$pat if $pred => $res" =>
            for (predEmpty <- liftM(emptyBlock);
                 t <- liftM(emptyBlock);
                 f <- liftM(emptyBlock);
                 exceptionL <- liftM(genSLabel);
                 patCond <- liftM(genSLabel);
                 error = BCat(f, BLast(new NException(exceptionL, classOf[MatchError], abruptNext)));
                 flushed = BCat(current, BLast(NPattern(patCond, pat, predEmpty.entryLabel, f.entryLabel)));
                 (predEvaled, predAnalysed) <- g(pred, predEmpty);
                 cond <- liftM(nCond(predAnalysed, t.entryLabel, f.entryLabel, pred));
                 predClosed = BCat(predEvaled, BLast(cond));
                 _ <- liftM(modifyGraph{ _ + flushed + error + predClosed });
                 _ <- cfgStmts(t, next, abruptNext, expressions(res), g))
            yield ()
        }
      case c :: cs => 
        c match {
          case cq"$pat => $res" =>
            for (t <- liftM(emptyBlock);
                 f <- liftM(emptyBlock);
                 patMatch <- liftM(genSLabel);
                 flushed = BCat(current, BLast(NPattern(patMatch, pat, t.entryLabel, f.entryLabel)));
                 _ <- liftM(modifyGraph{ _ + flushed });
                 _ <- cfgStmts(t, next, abruptNext, expressions(res), g);
                 _ <- cfgCases(cs, f, next, abruptNext, g))
            yield ()
          case cq"$pat if $pred => $res" =>
            for (predEmpty <- liftM(emptyBlock);
                 t <- liftM(emptyBlock);
                 f <- liftM(emptyBlock);
                 patMatch <- liftM(genSLabel);
                 flushed = BCat(current, BLast(NPattern(patMatch, pat, predEmpty.entryLabel, f.entryLabel)));
                 (predEvaled, predAnalysed) <- g(pred, predEmpty);
                 cond <- liftM(nCond(predAnalysed, t.entryLabel, f.entryLabel, pred));
                 predClosed = BCat(predEvaled, BLast(cond));
                 _ <- liftM(modifyGraph{ _ + flushed + predClosed });
                 _ <- cfgStmts(t, next, abruptNext, expressions(res), g);
                 _ <- cfgCases(cs, f, next, abruptNext, g))
            yield ()
        }
      case List() =>
        EitherT.right(State.state(()) : CFGGen[Unit])
    }
  }

  def detailedStmtCfg(stmt : Tree, current : Block[Node,C,O]) : CFGAnalyser[(Block[Node,C,O], Node[O,O])] = {
    println(stmt)
  */      

/*
    tree.Control.exprCata(
      components => {
        println("  a tuple")
        for ((b, comps) <- foldM(step, (current, List.empty), components);
             l <- liftM(genSLabel);
             tuple = NTuple(l, comps.reverse, stmt))
        yield (BCat(b, BMiddle(tuple)), tuple)
      },
      (earlydefns, parents, stats) => {
        val q"$p(...$argss)" :: _ = parents
        for ((b, nodes) <- foldM(deepStep, (current, List.empty), argss);
             `new` <- liftM(nNew(p, nodes.reverse, stmt)))
        yield (BCat(b, BMiddle(`new`)), `new`)
      },
      (qualifier) => {
        println("  a qualified this")
        for (`this` <- liftM(nThis(qualifier, stmt)))
        yield (BCat(current, BMiddle(`this`)), `this`)
      },
      (expr, tname) => {
        println("  a selection")
        for ((exprEvaled, exprAnalysed) <- detailedStmtCfg(expr, current);
             sel <- liftM(nSelect(exprAnalysed, tname, stmt)))
        yield (BCat(exprEvaled, BMiddle(sel)), sel)
      },
      (method, args) => {
        println("  an application")
        println("  " + method)
        println("  " + args)
        for ((mEvaled, nMethod) <- detailedStmtCfg(method, current); 
             (argsEvaled, argNodes) <- foldM(deepStep, (mEvaled, List.empty), args);
             app <- liftM(nApp(nMethod, argNodes.reverse, stmt)))
        yield (BCat(argsEvaled, BMiddle(app)), app)
      },
      (_, _) => {
        println(" an if-then -- impossible")
        for (l <- liftM(genSLabel);
             e = NExpr(l, stmt))
        yield (BCat(current, BMiddle(e)), e)
      },
      (_, _, _) => {
        println(" an if-the-else -- impossible")
        for (l <- liftM(genSLabel);
             e = NExpr(l, stmt))
        yield (BCat(current, BMiddle(e)), e)
      },
      (_, _) => {
        println(" a while loop -- impossible")
        for (l <- liftM(genSLabel);
             e = NExpr(l, stmt))
        yield (BCat(current, BMiddle(e)), e)
      },
      symbol => {
        println("  an identifier")
        for (n <- liftM(nVariable(stmt)))
        yield (BCat(current, BMiddle(n)), n)
      },
      (lexpr, rexpr) => {
        for ((lEvaled, lAnalysed) <- detailedStmtCfg(lexpr, current);
             (rEvaled, rAnalysed) <- detailedStmtCfg(rexpr, lEvaled);
             nAss <- liftM(nAssign(lAnalysed, rAnalysed, stmt)))
        yield (BCat(rEvaled, BMiddle(nAss)), nAss)
      },
      (_mods, name, expr) => {
        println("  an identifier definition")
        for ((eEvaled, eAnalysed) <- detailedStmtCfg(expr, current);
             valDef <- liftM(nValDef(eAnalysed, stmt)))
        yield (BCat(eEvaled, BMiddle(valDef)), valDef)
      },
      _ => {
        println("  a block -- impossible")
        for (l <- liftM(genSLabel);
             e = NExpr(l, stmt))
        yield (BCat(current, BMiddle(e)), e)
      },
      (other : Tree) => {
        println("  other")
        for (l <- liftM(genSLabel);
             e = NExpr(l, other))
        yield (BCat(current, BMiddle(e)), e)
      },
      stmt
    )

    for (l <- liftM(genSLabel);
         e = NExpr(l, stmt))
    yield (BCat(current, BMiddle(e)), e)
  }
}
*/
class CFGraph (val graph : Map[BLabel, Block[Node,C,C]], val start : BLabel, val done : BLabel, val nodeTree : tr.NodeTree) {
  type BEdge = (BLabel, BLabel, EdgeLabel.TagType)
  type SEdge = (SLabel, SLabel, EdgeLabel.TagType)

  def this(start : Block[Node,C,C], done : Block[Node,C,C], nodeTree : tr.NodeTree) =
    this(Map(start.entryLabel -> start, done.entryLabel -> done), start.entryLabel, done.entryLabel, nodeTree)

  def get(v : BLabel) : Option[Block[Node,C,C]] = 
    graph.get(v)

  def +(b : Block[Node,C,C]) : CFGraph =
    new CFGraph(graph + (b.entryLabel -> b), start, done, nodeTree)

  def +(bs : List[Block[Node,C,C]]) : CFGraph =
    bs.foldLeft(this)(_ + _)

  def successors(b : BLabel) : Option[List[(BLabel, EdgeLabel.TagType)]] = 
    get(b).map(_.successors)

  def traverse[A](f : (Block[Node,C,C], A) => A, x : A) : A = {
    def go(b : Block[Node,C,C], x : A, visited : Set[Block[Node,C,C]]) : (A, Set[Block[Node,C,C]]) = {
      if (visited(b))
        (x, visited)
      else {
        b.successors.foldLeft((f(b, x), visited + b)){(acc, succ) =>
          val (l, _) = succ
          get(l) match {
            case Some(block) => go(block, acc._1, acc._2)
            case None => acc
          }
        }
      }
    }

    get(start) map {go(_, x, Set.empty)._1} getOrElse x
  }

  def reverse() : ReverseCFGraph = {
    val reverseFlow : List[BEdge] = flow map { case (l1, l2, tag) => (l2, l1, tag) }
    new ReverseCFGraph(this, reverseFlow)
  }

  def flow : List[BEdge] = {
    def f (b : Block[Node,C,C], xs : List[BEdge]) : List[BEdge] = {
      val l : BLabel = b.entryLabel
      (b.successors map { case (succ, tag) => (l, succ, tag) }) ++ xs
    }
    traverse(f, List.empty)
  }

  def sLabels : List[SLabel] = {
    def collect(b : Block[Node,_,_], xs : List[SLabel]) : List[SLabel] = Block.sLabels(b) ++ xs    
    traverse(collect, List.empty)
  }

  def bLabels : List[BLabel] =
    flow flatMap { case (s, t, _) => List(s, t) }

  def apply(l : SLabel) : Option[tr.Node] =
    nodeTree.nodes.get(l)
}
