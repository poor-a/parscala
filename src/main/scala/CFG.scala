import java.io.File
import scala.tools.nsc._

object CFG {
  val settings : Settings = new Settings()
  settings.stopAfter.value = List("typer")
  settings.embeddedDefaults[CFG.type]
  val global : Global = new Global(settings)
  import global._


  abstract class CFGraph
  case class Block(stmts : List[Tree], cont : CFGraph) extends CFGraph
  case class If(thenBranch : CFGraph, elseBranch : CFGraph) extends CFGraph
  case class While(var body : CFGraph, cont : CFGraph) extends CFGraph {
    override def toString() : String = s"While(<body>, $cont)"
  }
  case class Entry(cont : CFGraph) extends CFGraph
  case class Exit() extends CFGraph

  def classesOf(packageDef : Tree) : List[Tree] = {
    val q"package $_ { ..$topstats }" = packageDef
    topstats.foldLeft(List[Tree]()){(classes, ast) => ast match {
      case q"package $_ { ..$_ }" => classesOf(ast) ++ classes
      case q"$_ class $_ extends $_" => ast :: classes
      case q"$_ class $_ extends $_ { ..$_ }" => ast :: classes
      case q"$_ class $_ extends $_ with $_ { ..$_ }" => ast :: classes
      case q"$_ object $_ extends $_" => ast :: classes
      case q"$_ object $_ extends $_ { ..$_ }" => ast :: classes
      case q"$_ object $_ extends $_ with $_ { ..$_ }" => ast :: classes
      case q"package object $_ extends $_" => ast :: classes
      case q"package object $_ extends $_ { ..$_ }" => ast :: classes
      case q"package object $_ extends $_ with $_ { ..$_ }" => ast :: classes
      case _                    => classes
    }}
  }

  def methodsOf(topstat : Tree) : List[Tree] = {
    def isMethod (definition : Tree) : Boolean = definition match {
      case q"$_ def $_ (...$_) : $_ = $_" => true
      case _ => false
    }

    val defs : List[Tree] = topstat match {
      case q"$_ class $_ extends $_ { ..$defs }" => defs
      case q"$_ class $_ extends $_ with $_ { ..$defs }" => defs
      case q"$_ object $_ extends $_ { ..$defs }" => defs
      case q"$_ object $_ extends $_ with $_ { ..$defs }" => defs
      case q"package object $_ extends $_ { ..$defs }" => defs
      case q"package object $_ extends $_ with $_ { ..$defs }" => defs
      case _ => List()
    }
    defs filter isMethod
  }

  def statName(topstat : Tree) : Option[String] = topstat match {
      case q"$_ class $name extends $_ { ..$defs }" => Some(name.toString)
      case q"$_ class $name extends $_ with $_ { ..$defs }" => Some(name.toString)
      case q"$_ object $name extends $_ { ..$defs }" => Some(name.toString)
      case q"$_ object $name extends $_ with $_ { ..$defs }" => Some(name.toString)
      case q"package object $name extends $_ { ..$defs }" => Some(name.toString)
      case q"package object $name extends $_ with $_ { ..$defs }" => Some(name.toString)
      case _ => None
  }

  def methodName(method : Tree) : Option[String] = method match {
    case q"$_ def $name (...$_) : $_ = $_" => Some(name.toString)
    case _ => None
  }

  def cfgMethod(method : Tree) : CFGraph = { 
    val q"$_ def $_ (...$_) : $_ = $body" = method
    def toList(t : Tree) : List[Tree] = t match {
      case q"{ ..$stmts }" => stmts
      case q"$stmt" => List(stmt)
    }

    def isBlockEnd (stmt : Tree) = stmt match {
      case q"if ($_) $_ else $_" => true
      case q"while ($_) $_" => true
      case q"do $_ while ($_)" => true    
      case q"return $_" => true
      case q"throw $_" => true
      case _ => false //etc.
    }

    def splitStmts(stmts : List[Tree], cont : CFGraph) : CFGraph = stmts match {
      case Nil => cont
      case q"if ($_) $thenBranch else $elseBranch" :: xs =>
        val contXs = splitStmts(xs, cont)
        If(splitStmts(toList(thenBranch), contXs), splitStmts(toList(elseBranch), contXs))
      case q"while ($_) ($body)" :: xs =>
        val w = While(null, splitStmts(xs, cont))
        val bodyCFG = splitStmts(toList(body), w)
        w.body = bodyCFG
        w
      case q"return $_" :: _ => cont
      case q"throw $_" :: _ => cont
      case x :: _ => 
        val (basicBlock,rest) = stmts span (!isBlockEnd(_))
        Block(basicBlock, splitStmts(rest, cont))
    }

    Entry(splitStmts(toList(body), Exit()))
  }

  def main(args : Array[String]) : Unit = {
    val run = new global.Run()
    println("args exists?")
    val xs : Array[(String, Boolean)] = args zip args.map{s => new File(s).exists()}
    xs foreach {x => println(s"${x._1} - ${x._2}")}
    run.compile(args.toList)
    println(s"units: ${run.units.length}")
    val units : Iterator[Tree] = run.units.map(_.body)
    val classes : Iterator[Tree] = units.flatMap(classesOf)
    val (cl1, cl2) = classes.duplicate
    val (cl3, cl4) = cl2.duplicate
    val (cl5, cl6) = cl4.duplicate
    print(s"classes (${cl1.length}): ")
    println(cl3.map(statName).map(_ getOrElse "").mkString(", "))
    print("methods: ")
    println(cl5.flatMap(methodsOf).map(methodName).map(_ getOrElse "").mkString(", "))
    println(cl6.flatMap(methodsOf).find(methodName(_) == Some("main")).map(cfgMethod))
  }
}
