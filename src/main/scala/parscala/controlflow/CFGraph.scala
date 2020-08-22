package parscala
package controlflow

import scala.language.higherKinds

import scalaz.{State, EitherT, Hoist, Monoid, Monad, MonadState, \/-, Left3, Middle3, Right3}
import scalaz.syntax.bind.ToBindOpsUnapply // >>= and >>

import parscala.Control.{foldM, forM}

import parscala.{tree => tr}

abstract class O // Open
abstract class C // Closed

object CFGraph {
  def fromMethod(m : tr.Defn.Method, pgraph : ProgramGraph) : CFGraph = {
    val (b, st) : (Block[Node, C, O], St) = initSt(pgraph)
    stToCFGraph(
      execCFGAnalyser( for (end <- emptyBlock(cfgASt);
                            done = Done(List(st.done.entryLabel));
                            _ <- close(end, done);
                            _ <- putMethod(Left(m.label), b.entryLabel, end.entryLabel)(cfgASt);
                            last <- cfgStmts(m.body, b)(cfgASt);
                            _ <- close(last, Jump(end.entryLabel)))
                       yield ()
                     , st
                     )
    )
  }

  def fromExpression(n : tr.Expr, pgraph : ProgramGraph) : CFGraph = {
    val (b, st) : (Block[Node, C, O], St) = initSt(pgraph)
    stToCFGraph(execCFGAnalyser(cfgStmts(n, b)(cfgASt), st))
  }

  private case class St(
      val bGen : BLabelGen
    , val sGen : SLabelGen
    , val blocks : Map[BLabel, Block[Node, C, C]]
    , val methods : Methods
    , val pgraph : ProgramGraph
    , val abruptNext : BLabel
    , val start : Block[Node, C, C]
    , val done : Block[Node, C, C]
  ) {
    def addBlock(b : Block[Node, C, C]) : St = 
      copy(blocks = this.blocks + (b.entryLabel -> b))
  }

  private type Methods = Map[MLabel, (BLabel, BLabel)]
  private type CFGGen[A] = State[St, A]
  private type CFGAnalyser[A] = EitherT[CFGGen, Unit, A]

//  private val cfgAMonad : Monad[CFGAnalyser] = implicitly[Monad[CFGAnalyser]]
//  private val cfgAErr : MonadError[CFGAnalyser, Unit] = implicitly[MonadError[CFGAnalyser, Unit]]
  private val cfgASt : MonadState[CFGAnalyser, St] = parscala.EitherT.eitherTStateTInstance
  private val cfgATrans : Hoist[({type λ[M[_], A] = EitherT[M, Unit, A]})#λ] = EitherT.eitherTHoist
  private val cfgAnalyser : Monad[CFGAnalyser] = EitherT.eitherTMonad

  private def genBLabel[M[_]](implicit monadSt : MonadState[M, St]) : M[BLabel] =
    modifySt{ st => (st.bGen.head, st.copy(bGen = st.bGen.tail)) }

  private def modifySt[A, M[_]](f : St => (A, St))(implicit monadSt : MonadState[M, St]) : M[A] =
    monadSt.get >>= (s => {
    val (x, sNew) : (A, St) = f(s)
    monadSt.put(sNew) >>
    monadSt.pure(x)
    })

  private def gets[A](f : St => A) : CFGGen[A] =
    for (s <- State.get[St]) yield f(s)

  private def putMethod[M[_]](method : MLabel, start : BLabel, done : BLabel)(implicit monadSt : MonadState[M, St]) : M[Unit] =
    modifySt{ st => ((), st.copy(methods = st.methods + (method -> ((start, done))))) }

  private def getAbruptNext : CFGGen[BLabel] =
    gets(_.abruptNext)

  private def setAbruptNext(l : BLabel) : CFGGen[Unit] =
    modifySt[Unit, CFGGen]{ st => ((), st.copy(abruptNext = l)) }

  private def singleton(n : Node[C, O]) : Block[Node, C, O] =
    BFirst(n)

  private def append(b : Block[Node, C, O], n : Node[O, O]) : Block[Node, C, O] =
    BCat(b, BMiddle(n))

  private def close(b : Block[Node, C, O], n : Node[O, C])(implicit monadSt : MonadState[CFGAnalyser, St] = cfgASt) : CFGAnalyser[Block[Node, C, C]] = {
    val closed : Block[Node, C, C] = BCat(b, BLast(n))
    modifySt{ st => (closed, st.addBlock(closed)) }
  }

  private def liftM[A](m : CFGGen[A]) : CFGAnalyser[A] = 
    EitherT.eitherTHoist.liftM(m)

  private def handleError[A](m : CFGAnalyser[A])(handler : Unit => CFGAnalyser[A]) : CFGAnalyser[A] =
    EitherT.eitherTMonadError[CFGGen, Unit].handleError(m)(handler)

  private def raiseError[A]() : CFGAnalyser[A] =
    EitherT.eitherTMonadError[CFGGen, Unit].raiseError(())

  private def execCFGAnalyser[A](m : CFGAnalyser[A], startSt : St) : St = m.run.run(startSt)._1

  private def stToCFGraph(st : St) : CFGraph =
    new CFGraph(st.blocks, st.start, st.done, st.methods, st.pgraph)

  private def stToExtensibleCFGraph(st : St) : ExtensibleCFGraph =
    new ExtensibleCFGraph(stToCFGraph(st), st.bGen, st.sGen)

  /**
   * Default state with an empty control flow graph and an empty set of
   * declarations.
   */
  private def initSt(pgraph : ProgramGraph) : (Block[Node,C,O], St) = {
    val bGen : BLabelGen = BLabel.stream
    val sGen : SLabelGen = SLabel.stream
    val List(first, start, done) : List[BLabel] = (bGen take 3).toList
    val fst : Block[Node, C, O] = emptyBlockWithLabel(first)
    val s : Block[Node, C, C] = BCat(BFirst(Label(start)), BLast(Jump(first)))
    val d : Block[Node, C, C] = BCat(BFirst(Label(done)), BLast(Done(List())))
    val blocks : Map[BLabel, Block[Node, C, C]] = Map(start -> s, done -> d)
    (fst, St(bGen drop 3, sGen, blocks, Map(), pgraph, done, s, d))
  }

  private def emptyBlockWithLabel(b : BLabel) : Block[Node, C, O] = 
    BFirst(Label(b))

  private def emptyBlock[M[_]](implicit monadSt : MonadState[M, St]) : M[Block[Node, C, O]] = 
    genBLabel >>= (l => monadSt.pure(emptyBlockWithLabel(l)))

  private def methodStartEnd(method : MLabel) : CFGAnalyser[Option[(BLabel, (BLabel, Done))]] =
    for (methods <- liftM(State.gets[St, Methods](_.methods));
         res <- (methods.get(method) match {
          case Some((start, end)) =>
            for (b <- getBlock(end);
                 done <- getDone(b))
            yield Some((start, (end, done)))
          case _ => cfgAnalyser.pure(None)
         }))
    yield res

  private def liftOption[A](o : Option[A]) : CFGAnalyser[A] =
    scalaz.std.option.cata(o)(cfgAnalyser.pure(_), raiseError())

  private def getDone(b : Block[Node, C, C]) : CFGAnalyser[Done] =
    Block.lastNode(b) match { 
      case n : Done => cfgAnalyser.pure(n)
      case _ => raiseError()
    }

  private def getBlock(l : BLabel) : CFGAnalyser[Block[Node, C, C]] =
    for (blocks <- liftM(State.gets[St, Map[BLabel, Block[Node, C, C]]](_.blocks));
         b <- liftOption(blocks.get(l)))
    yield b

  // private def addReturnPoint(method : Either[SLabel, DLabel], returnPoint : BLabel) : CFGAnalyser[Unit] =
  //   liftM(State.gets[St, Map[BLabel, Block[Node, C, C]]](_.blocks)) >>= (blocks =>
  //   methodStartEnd(method) >>= (mStartEnd =>
  //   scalaz.std.option.cata(mStartEnd)(
  //       { case (start, end) =>
  //           scalaz.std.option.cata(blocks.get(end))
  //               methodEnd => {
  //                 def addReturn()

  private def analyseMethod(method : MLabel)(implicit mSt : MonadState[CFGAnalyser, St], monadTrans : Hoist[({type λ[M[_], A] = EitherT[M, Unit, A]})#λ]) : CFGAnalyser[Option[(BLabel, (BLabel, Done))]] = {
    val const2 : (Any, Any) => CFGAnalyser[Option[(BLabel, (BLabel, Done))]] = Function.const2(mSt.pure(None))
    val const3 : (Any, Any, Any) => CFGAnalyser[Option[(BLabel, (BLabel, Done))]] = Function.const3(mSt.pure(None))
    val const4 : (Any, Any, Any, Any) => CFGAnalyser[Option[(BLabel, (BLabel, Done))]] = Function.const4(mSt.pure(None))
    val const5 : (Any, Any, Any, Any, Any) => CFGAnalyser[Option[(BLabel, (BLabel, Done))]] = Function.const5(mSt.pure(None))
    val const6 : (Any, Any, Any, Any, Any, Any) => CFGAnalyser[Option[(BLabel, (BLabel, Done))]] = Function.const6(mSt.pure(None))
    mSt.gets(_.pgraph) >>= (programgraph =>
    method match {
      case Left(dLabel) =>
        programgraph.lookupDeclDefn(dLabel) match {
          case Some(Left3(decl)) => 
            tr.Decl.cata( const5 // var
                        , const5 // val
                        , (_, _, _, _, _, _, _) => // method
                            for (start <- emptyBlock;
                                 end <- emptyBlock;
                                 done = Done(List());
                                 _ <- close(start, Jump(end.entryLabel));
                                 _ <- close(end, done);
                                 _ <- putMethod(method, start.entryLabel, end.entryLabel))

                            yield Some((start.entryLabel, (end.entryLabel, done)))
                        , const6 // type
                        , const2 // import
                        , decl
                        )
          case Some(Middle3(defn)) =>
            tr.Defn.cata( const6 // value
                        , const6 // variable
                        , (_, _, _, _, _, _, _, body) => { // method
                            emptyBlock >>= (start =>
                            emptyBlock >>= (end => {
                            val done = Done(List())
                            close(end, done) >> {
                            emptyBlock >>= (first => {
                            close(start, Jump(first.entryLabel)) >> (
                            putMethod(method, start.entryLabel, end.entryLabel) >> (
                            monadTrans.liftM(setAbruptNext(end.entryLabel)) >> (
                            monadTrans.liftM(cfgStmts(body, first).run) >>= (res => {
                            getBlock(end.entryLabel) >>= (b => (getDone(b) >>= (done_ =>
                            res match {
                                case \/-(b) =>
                                  close(b, Jump(end.entryLabel)) >>
                                  mSt.pure(Some((start.entryLabel, (end.entryLabel, done_))))
                                case _      =>
                                  mSt.pure(Some((start.entryLabel, (end.entryLabel, done_))))
                              }
                            )))}))))})}}))}
                        , const6 // type
                        , const6 // macro
                        , const6 // secondary constructor
                        , const5 // class
                        , const5 // trait
                        , const5 // object
                        , const5 // package object
                        , const4 // package
                        , defn
                        )
          case Some(Right3(foreignMethod @ _)) =>
            for (res @ (start, (end, done)) <- unknownMethod;
                 _ <- putMethod(method, start, end))
            yield Some(res)
          case None =>
            { println("no such method: " + method); mSt.pure(None) }
        }
      case Right(expr @ _) =>
        cfgASt.map(unknownMethod)(Some(_))
    }
    )
  }

  private def unknownMethod : CFGAnalyser[(BLabel, (BLabel, Done))] = {
    implicit val st : MonadState[CFGAnalyser, St] = cfgASt
    for (start <- emptyBlock;
         end <- emptyBlock;
         _ <- close(start, Jump(end.entryLabel));
         done = Done(List());
         _ <- close(end, done))
    yield (start.entryLabel, (end.entryLabel, done))
  }

  private implicit val unitMonoidInstance : Monoid[Unit] = Monoid.instance((_, _) => (), ())

  private def cfgMethodCall(callExpr : SLabel, b : Block[Node, C, O]) : CFGAnalyser[Block[Node, C, O]] = {
    implicit val cfgAnalyser : MonadState[CFGAnalyser, St] = cfgASt
    val callMethods : List[(BLabel, (BLabel, Done))] => CFGAnalyser[Block[Node, C, O]] = startEnds =>
              genBLabel >>= (returnPoint => {
              forM[CFGAnalyser, (BLabel, (BLabel, Done)), (BLabel, BLabel)](startEnds){ case (start, (end, done)) => {
                    val doneWithNewReturn : Block[Node, C, C] = BCat(BFirst(Label(end)), BLast(done.addSucc(returnPoint)))
                    modifySt{ st => ((), st.copy(blocks = st.blocks.updated(end, doneWithNewReturn)) )} >> {
                    cfgAnalyser.pure((start, end))
                    }}
              } >>= {startEnds => {
              val (startPoints, endPoints) : (List[BLabel], List[BLabel]) = startEnds.unzip
              val call : Call = Call(callExpr, startPoints, returnPoint)
              close(b, call) >>
              cfgAnalyser.pure(singleton(Return(returnPoint, endPoints, call)))
              }}})

          cfgAnalyser.gets(_.pgraph) >>= (pgraph => {
          val funRefs : CFGAnalyser[List[(BLabel, (BLabel, Done))]] = 
            pgraph.callTargets.get(callExpr) match {
            case None | Some(List()) =>
              cfgAnalyser.map(unknownMethod)(List(_))
            case Some(callTargets @ _ :: _) =>
              val getStartEnd : MLabel => CFGAnalyser[Option[(BLabel, (BLabel, Done))]] =
                callTarget =>
                  methodStartEnd(callTarget) >>= (mStartEnd =>
                  mStartEnd match {
                    case Some(_) => cfgAnalyser.pure(mStartEnd)
                    case None => analyseMethod(callTarget)(cfgAnalyser, cfgATrans)
                  })
              val listTraverse : scalaz.Traverse[List] = scalaz.std.list.listInstance
              val optionApplicative : scalaz.Applicative[Option] = scalaz.std.option.optionInstance
              cfgAnalyser.map(forM(callTargets)(getStartEnd))(listTraverse.sequence(_)(optionApplicative)) >>= (mStartEnds =>
                mStartEnds match {
                  case None => cfgAnalyser.map(unknownMethod)(List(_))
                  case Some(startEnds) => cfgAnalyser.pure(startEnds)
                }
              )
            }
          funRefs >>= callMethods
          })
  }

  private def cfgStmts
    ( node : tr.Expr, b : Block[Node, C, O] )
    ( implicit m : MonadState[CFGAnalyser, St] )
    : CFGAnalyser[Block[Node, C, O]] = {

    def foldArgs(init : Block[Node, C, O], args : List[tr.Expr]) : CFGAnalyser[Block[Node, C, O]] =
      foldM((acc : Block[Node, C, O], x : tr.Expr) => cfgStmts(x, acc), init, args)

    tr.Expr.cata(
        (l, _, _) => { // literal
          val literal = Expr(l)
          m.pure(append(b, literal))
        }
      , (l, _, _, _) => { // identifier
          val identifier = Expr(l)
          m.pure(append(b, identifier))
        }
      , (l, _, rhs, _) => // assignment
          m.map(cfgStmts(rhs, b))(append(_, Expr(l)))
      , (l, fun, args, _) => // application
          cfgStmts(fun, b) >>= (afterFun =>
          foldArgs(afterFun, args) >>= (afterArgs =>
          cfgMethodCall(l, afterArgs)))
      , (l, lhs, _op, args, _) => // infix application
          cfgStmts(lhs, b) >>= (afterLhs =>
          foldArgs(afterLhs, args) >>= (afterArgs =>
          cfgMethodCall(l, afterArgs)))
      , (l, _op, arg, _) => // unary application
          cfgStmts(arg, b) >>= (afterArg =>
          cfgMethodCall(l, afterArg))
      , (l, class_, argss, _) => // new
          m.map(foldM((acc : Block[Node, C, O], x : tr.Expr) => cfgStmts(x, acc), b, argss.flatten)(m))(append(_, Expr(l)))
      , (l, expr, tname, _, _) => // selection
          m.map(cfgStmts(expr, b))(append(_, Expr(l)))
      , (l, argss) => // this(...) application TODO: edge to called constructor
          foldM((acc, args) => foldArgs(acc, args), b, argss) >>= (afterArgss =>
          m.pure(append(afterArgss, Expr(l))))
      , (l, _, _) => // qualified this
          m.pure(append(b, Expr(l)))
      , (l, _, _, _) => // qualified super
          m.pure(append(b, Expr(l)))
      , (l, components, t) => // tuple
          m.map(foldM((acc : Block[Node, C, O], x : tr.Expr) => cfgStmts(x, acc), b, components)(m))(append(_, Expr(l)))
      , (l, p, t, _) => // if-then
          for (afterP <- cfgStmts(p, b);
               tBranch <- emptyBlock;
               succ <- emptyBlock;
               cond = Cond(p.label, tBranch.entryLabel, succ.entryLabel);
               _ <- close(afterP, cond);
               afterTBranch <- cfgStmts(t, tBranch);
               _ <- close(afterTBranch, Jump(succ.entryLabel))
              )
          yield succ
      , (l, p, t, f, _) => // if-then-else
          for (afterP <- cfgStmts(p, b);
               tBranch <- emptyBlock;
               fBranch <- emptyBlock;
               cond = Cond(p.label, tBranch.entryLabel, fBranch.entryLabel);
               _ <- close(afterP, cond);
               succ <- emptyBlock;
               normalFlow <- handleError {
                   for (afterTBranch <- cfgStmts(t, tBranch);
                        _ <- close(afterTBranch, Jump(succ.entryLabel)))
                   yield true
                 } {
                    _ => m.pure(false)
                 };
               _ <- handleError {
                      for (afterFBranch <- cfgStmts(f, fBranch);
                           _ <- close(afterFBranch, Jump(succ.entryLabel)))
                      yield ()
                    } {
                      _ => if (normalFlow)
                             m.pure(())
                           else
                             raiseError()
                    })
          yield succ
      , (l, p, loopBody, _) => // while loop
          for (testP <- emptyBlock;
               _ <- close(b, Jump(testP.entryLabel));
               afterP <- cfgStmts(p, testP);
               body <- emptyBlock;
               succ <- emptyBlock;
               cond = Cond(p.label, body.entryLabel, succ.entryLabel);
               _ <- close(body, cond);
               _ <- handleError {
                   for (afterBody <- cfgStmts(loopBody, body);
                        _ <- close(afterBody, Jump(testP.entryLabel)))
                   yield ()
                 } {
                   _ => m.pure(())
                 })
          yield succ
      , (l, enums, body, _) => // for loop
          m.pure(append(b, Expr(l))) 
      , (l, enums, body, _) => // for-yield loop
          m.pure(append(b, Expr(l))) 
      , (l, _) => // return 
          liftM(getAbruptNext) >>= (abruptNext =>
          close(append(b, Expr(l)), Jump(abruptNext)) >>
          raiseError())
      , (l, expr, _) => // return with expr
          cfgStmts(expr, b) >>= (afterExpr =>
          liftM(getAbruptNext) >>= (abruptNext =>
          close(append(afterExpr, Expr(l)), Jump(abruptNext)) >>
          raiseError()
          ))
      , (l, expr, _) => // throw
          cfgStmts(expr, b) >>= (afterExpr =>
          liftM(getAbruptNext) >>= (abruptNext =>
          close(append(afterExpr, Expr(l)), Jump(abruptNext)) >>
          raiseError()))
      , (l, statements, _) => // block
          foldM(
              (acc : Block[Node, C, O], stmt : tr.Statement) =>
                stmt.fold(
                    _ => m.pure(acc)
                  , _ => m.pure(acc)
                  , expr => cfgStmts(expr, acc)
                  )
            , b
            , statements
            )
      , (l, _, _) => // other expression
          m.pure(append(b, Expr(l)))
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
//      , node
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

class CFGraph ( val graph : Map[BLabel, Block[Node,C,C]]
              , val start : Block[Node,C,C]
              , val done : Block[Node,C,C]
              , val methods : Map[MLabel, (BLabel, BLabel)]
              , val pgraph : ProgramGraph
              ) {
  type BEdge = (BLabel, BLabel, EdgeLabel.TagType)
  type SEdge = (SLabel, SLabel, EdgeLabel.TagType)

  def get(v : BLabel) : Option[Block[Node,C,C]] = 
    graph.get(v)

  def +(b : Block[Node,C,C]) : CFGraph =
    new CFGraph(graph + (b.entryLabel -> b), start, done, methods, pgraph)

  def +(bs : List[Block[Node,C,C]]) : CFGraph =
    bs.foldLeft(this)(_ + _)

  def successors(b : BLabel) : List[(BLabel, EdgeLabel.TagType)] = 
    get(b).map(_.successors).getOrElse(List())

  def precedessors(b : BLabel) : List[(BLabel, EdgeLabel.TagType)] = 
    flow.filter{ case (_, trg, _) => b == trg }.map{ case (prec, _, tag) => (prec, tag) }

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

    go(start, x, Set.empty)._1
  }

  def reverseBEdge : BEdge => BEdge = { case (l1, l2, tag) => (l2, l1, tag) }

  def reverse() : ReverseCFGraph = {
    val reverseFlow : List[BEdge] = flow map reverseBEdge
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

  def apply(l : SLabel) : Option[tr.Expr] =
    pgraph.expressions.get(l)
}
