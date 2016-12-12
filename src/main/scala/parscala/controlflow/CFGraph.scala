package parscala
package controlflow

import scala.language.higherKinds

import scala.collection.immutable.Stream

import scalaz.{State, EitherT, Monoid, Monad}
import scalaz.EitherT.eitherTHoist

import tree._

abstract class O
abstract class C

object CFGraph {
  import compiler.Quasiquote

  def apply(m : Method) : CFGraph =
    mkExtCFGraph(m).freeze

  type St = (Stream[BLabel], Stream[SLabel], CFGraph)
  type CFGGen[A] = State[St, A]
  type CFGAnalyser[A] = EitherT[CFGGen, Unit, A]

  def nLabel : CFGGen[NLabel] =
    for (l <- genSLabel; b <- genBLabel)
    yield NLabel(l, b)

  def nLiteral(lit : Tree) : CFGGen[NLiteral] = 
    for (l <- genSLabel)
    yield NLiteral(l, lit)

  def nVariable(variable : Tree) : CFGGen[NVariable] =
    for (l <- genSLabel)
    yield NVariable(l, variable)

  def nValDef(rhs : Node[O,O], tr : Tree) : CFGGen[NValDef] =
    for (l <- genSLabel)
    yield NValDef(l, rhs, tr)

  def nAssign(lhs : Node[O,O], rhs : Node[O,O], tr : Tree) : CFGGen[NAssign] =
    for (l <- genSLabel)
    yield NAssign(l, lhs, rhs, tr)

  def nNew(constructor : Tree, args : List[List[Node[O,O]]], tr : Tree) : CFGGen[NNew] = 
    for (l <- genSLabel)
    yield NNew(l, constructor, args, tr)

  def nApp(method : Node[O,O], args : List[List[Node[O,O]]], tr : Tree) : CFGGen[NApp] = 
    for (l <- genSLabel)
    yield NApp(l, method, args, tr)

  def nSelect(expr : Node[O,O], sel : TermName, tr : Tree) : CFGGen[NSelect] = 
    for (l <- genSLabel)
    yield NSelect(l, expr, sel, tr)

  def nThis(obj : Node[O,O], expr : Node[O,O], tr : Tree) : CFGGen[NThis] = 
    for (l <- genSLabel)
    yield NThis(l, obj, expr, tr)

  def nCond(expr : Node[O,O], t : BLabel, f : BLabel, tr : Tree) : CFGGen[NCond] = 
    for (l <- genSLabel)
    yield NCond(l, expr, t, f, tr)

  def nJump(target : BLabel) : CFGGen[NJump] = 
    for (l <- genSLabel)
    yield NJump(l, target)

  def nReturn(expr : Node[O,O], next : BLabel, tr : Tree) : CFGGen[NReturn] =
    for (l <- genSLabel)
    yield NReturn(l, expr, next, tr)

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

  def liftM[A](m : CFGGen[A]) : CFGAnalyser[A] = 
    eitherTHoist.liftM(m)

  private def toList(t : Tree) : List[Tree] = 
    t match {
      case _ if t.isInstanceOf[compiler.Block] => {
        val q"{ ..$stmts }" = t
        stmts
      }
      case q"$stmt" => List(stmt)
    }

  def mkExtCFGraph(m : Method) : ExtensibleCFGraph = {
    val bGen : BLabelGen = BLabel.stream
    val sGen : SLabelGen = SLabel.stream
    val (List(fst,s,d), bGen2) = ((bGen take 3).toList, bGen drop 3)
    val (List(fst2,s2,d2,jump,d3), sGen2) = ((sGen take 5).toList, sGen drop 5)

    val start = BCat(emptyBlockWithLabel(s2, s), BLast(NJump(jump, fst)))
    val done = BCat(emptyBlockWithLabel(d2, d), BLast(NDone(d3)))
    val graph = new CFGraph(start, done)
    val first = emptyBlockWithLabel(fst2, fst)

    val stmts : List[Tree] = m.body match {
      case Some(ast) => toList(ast)
      case None => List.empty
    }

    val (bGen3, sGen3, cfg) = cfgStmts(first, d, d, stmts, detailedStmtCfg).run.exec((bGen2, sGen2, graph))
    new ExtensibleCFGraph(cfg, bGen3, sGen3)
  }

  def emptyBlockWithLabel(s : SLabel, b : BLabel) : Block[Node, C, O] = 
    BFirst(NLabel(s, b))

  def emptyBlock : State[St, Block[Node, C, O]] = 
    for (l <- genBLabel; s <- genSLabel) yield emptyBlockWithLabel(s, l)

  implicit def unitMonoidInstance : Monoid[Unit] = Monoid.instance((_,_) => Unit, Unit)
    
  def cfgStmts(b : Block[Node,C,O], nextLabel : BLabel, abruptNext : BLabel, stmts : List[Tree], g : (Tree, Block[Node,C,O]) => CFGAnalyser[(Block[Node,C,O], Node[O,O])]) : CFGAnalyser[Unit] = {
    stmts match {
      case (condE @ q"if ($p) $t else $f") :: xs =>
        for (tBranch <- liftM(emptyBlock);
             fBranch <- liftM(emptyBlock);
             succ <- liftM(emptyBlock);
             _ <- cfgStmts(tBranch, succ.entryLabel, abruptNext, toList(t), g);
             _ <- cfgStmts(fBranch, succ.entryLabel, abruptNext, toList(f), g);
             pAnalysed <- g(p, b);
             bWithTest = pAnalysed._1;
             test = pAnalysed._2;
             cond <- liftM(nCond(test, tBranch.entryLabel, fBranch.entryLabel, condE));
             flushed = BCat(bWithTest, BLast(cond));
             _ <- liftM(modifyGraph{ _ + flushed });
             _ <- cfgStmts(succ, nextLabel, abruptNext, xs, g))
        yield ()
      case (x @ q"while ($p) $loopBody") :: xs =>
        for (succ <- liftM(emptyBlock);
             body <- liftM(emptyBlock);
             testPBegin <- liftM(emptyBlock);
             pAnalysed <- g(p, testPBegin);
             withTest = pAnalysed._1;
             test = pAnalysed._2;
             cond <- liftM(nCond(test, body.entryLabel, succ.entryLabel, x));
             testP = BCat(withTest, BLast(cond));
             jump <- liftM(nJump(testP.entryLabel));
             flushed = BCat(b, BLast(jump));
             _ <- liftM(modifyGraph{ _ + flushed + testP });
             _ <- cfgStmts(body, testP.entryLabel, abruptNext, toList(loopBody), g);
             _ <- cfgStmts(succ, nextLabel, abruptNext, xs, g))
        yield ()
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
             _ <- cfgStmts(body, testP.entryLabel, abruptNext, toList(loopBody), g);
             _ <- cfgStmts(succ, nextLabel, abruptNext, xs, g))
        yield ()
      case q"try $expr" :: xs =>
        cfgStmts(b, nextLabel, abruptNext, toList(expr) ++ xs, g)
      case q"try $expr catch { case $_ }" :: xs => 
        cfgStmts(b, nextLabel, abruptNext, toList(expr) ++ xs, g)
      case q"try $expr finally $fin" :: xs => 
        cfgStmts(b, nextLabel, abruptNext, toList(expr) ++ toList(fin) ++ xs, g)
      case q"try $expr catch { case ..$_ } finally $fin" :: xs => 
        cfgStmts(b, nextLabel, abruptNext, toList(expr) ++ toList(fin) ++ xs, g)
      case q"$expr match { case ..$cases }" :: xs =>
        for (succ <- liftM(emptyBlock);
             exprAnalysed <- g(expr, b);
             exprEvaled = exprAnalysed._1;
             done <- liftM(State.gets[St, BLabel](_._3.done));
             _ <- cfgCases(cases, exprEvaled, succ.entryLabel, abruptNext, g);
             _ <- cfgStmts(succ, nextLabel, abruptNext, xs, g))
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
        cfgStmts(b, nextLabel, abruptNext, toList(x) ++ xs, g)
      case x :: xs => {
        for ((xEvaled, _) <- g(x, b);
             _ <- cfgStmts(xEvaled, nextLabel, abruptNext, xs, g))
        yield ()
      }
      case List() => {
        for (jump <- liftM(genSLabel);
             flushed = BCat(b, BLast(NJump(jump, nextLabel)));
             _ <- liftM(modifyGraph{ _ + flushed }))
        yield ()
      }
    }
  }

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
                 _ <- cfgStmts(t, next, abruptNext, toList(res), g))
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
                 _ <- cfgStmts(t, next, abruptNext, toList(res), g))
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
                 _ <- cfgStmts(t, next, abruptNext, toList(res), g);
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
                 _ <- cfgStmts(t, next, abruptNext, toList(res), g);
                 _ <- cfgCases(cs, f, next, abruptNext, g))
            yield ()
        }
      case List() =>
        EitherT.right(State.state(()) : CFGGen[Unit])
    }
  }

  def mapM[A, B, S](f : A => State[S, B], l : List[A]) : State[S, List[B]] =
    l match {
      case List() =>
        State.state(List())
      case x :: xs => 
        for (y <- f(x);
             ys <- mapM(f, xs))
        yield ys
    }

  def foldM[M[_], A, B](f : (A, B) => M[A], e : A, l : List[B])(implicit evidence: Monad[M]) : M[A] = {
    import scalaz.syntax.bind._
    l match {
      case List() =>
        evidence.pure(e)
      case x :: xs => 
        f(e, x) >>= (e2 => foldM(f, e2, xs))
/*        for (e2 <- f(e, x);
             e3 <- foldM(f, e2, xs))
        yield e3*/
    }
  }

  def detailedStmtCfg(stmt : Tree, current : Block[Node,C,O]) : CFGAnalyser[(Block[Node,C,O], Node[O,O])] = {
    println(stmt)
        
    def step(acc : (Block[Node,C,O], List[Node[O,O]]), e : Tree) : CFGAnalyser[(Block[Node,C,O], List[Node[O,O]])] = {
      val (b, ns) = acc
      for ((b2, n) <- detailedStmtCfg(e, b))
      yield (b2, n :: ns)
    }

    def deepStep(acc : (Block[Node,C,O], List[List[Node[O,O]]]), args : List[Tree]) : CFGAnalyser[(Block[Node,C,O], List[List[Node[O,O]]])] = {
      val (b, nns) = acc
      for ((b2, ns) <- foldM(step, (b, List.empty), args))
      yield (b2, ns.reverse :: nns)
    }

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
      (expr, tname) => {
        println("  a selection")
        for ((exprEvaled, exprAnalysed) <- detailedStmtCfg(expr, current);
             l <- liftM(genSLabel);
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
      (other : Tree) => {
        println("  other")
        for (l <- liftM(genSLabel);
             e = NExpr(l, other))
        yield (BCat(current, BMiddle(e)), e)
      },
      stmt
    )
  }
}

class CFGraph (val graph : Map[BLabel, Block[Node,C,C]], val start : BLabel, val done : BLabel) {
  type BEdge = (BLabel, BLabel, EdgeLabel.TagType)
  type SEdge = (SLabel, SLabel, EdgeLabel.TagType)

  def this(start : Block[Node,C,C], done : Block[Node,C,C]) =
    this(Map(start.entryLabel -> start, done.entryLabel -> done), start.entryLabel, done.entryLabel)

  def get(v : BLabel) : Option[Block[Node,C,C]] = 
    graph.get(v)

  def +(b : Block[Node,C,C]) : CFGraph =
    new CFGraph(graph + (b.entryLabel -> b), start, done)

  def +(bs : List[Block[Node,C,C]]) : CFGraph =
    bs.foldLeft(this)(_ + _)

  def traverse[A](f : (Block[Node,C,C], A) => A, x : A) : A = {
    def go(b : Block[Node,C,C], x : A, visited : Set[Block[Node,C,C]]) : (A, Set[Block[Node,C,C]]) = {
      if (visited(b))
        (x, visited)
      else {
        b.successors.foldLeft((f(b, x), visited + b)){(acc,succ) => 
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
    def collect(b : Block[Node,_,_], xs : List[SLabel]) : List[SLabel] = b.sLabels ++ xs    
    traverse(collect, List.empty)
  }
}
