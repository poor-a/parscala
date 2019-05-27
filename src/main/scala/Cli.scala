import org.apache.commons.cli
import java.nio.file.Paths

class Cli {
  private[this] val method : cli.Option = 
    cli.Option.builder("m")
     .argName("method")
     .longOpt("method")
     .desc("select a method, e.g. com.foo.bar")
     .hasArg()
     .required(false)
     .build()

  private[this] val showCfg : cli.Option = 
    cli.Option.builder("cfg")
     .longOpt("show-cfg")
     .desc("show the control flow graph in a window")
     .required(false)
     .build()

  private[this] val showAst : cli.Option = 
    cli.Option.builder("ast")
     .longOpt("show-ast")
     .desc("show the abstract syntax tree in a window")
     .required(false)
     .build()

  private[this] val showCalls : cli.Option = 
    cli.Option.builder("c")
     .longOpt("show-calls")
     .desc("show the call graph in a window")
     .required(false)
     .build()

  private[this] val showDataflow : cli.Option = 
    cli.Option.builder("df")
     .longOpt("show-dataflow")
     .desc("show the data flow graph in a window")
     .required(false)
     .build()

  private[this] val dotOutput : cli.Option = 
    cli.Option.builder("o")
     .desc("dump the control flow graph into a dot file")
     .argName("filename")
     .required(false)
     .hasArg()
     .build()

  private[this] val mapLike : cli.Option =
    cli.Option.builder("ml")
     .longOpt("map-like")
     .desc("check whether the selected method exhibits map-like behavior")
     .required(false)
     .build()

  private[this] val transformRemoveUnusedVars : cli.Option =
    cli.Option.builder("ru")
     .longOpt("remove-unused")
     .desc("Removes unused variables in the selected method")
     .required(false)
     .build()

  private[this] val files : cli.Option = 
    cli.Option.builder("f")
     .longOpt("files")
     .desc("source files to analyse")
     .argName("file1 file2 ...")
     .hasArgs()
     .numberOfArgs(cli.Option.UNLIMITED_VALUES)
     .build()

  private[this] val dirs : cli.Option = 
    cli.Option.builder("d")
     .longOpt("dir")
     .desc("directories of Scala files to analyse")
     .argName("dir1 dir2 ...")
     .hasArgs()
     .numberOfArgs(cli.Option.UNLIMITED_VALUES)
     .build()

  private[this] val classpath : cli.Option = 
    cli.Option.builder("cp")
     .longOpt("classpath")
     .desc("classpath for the Scala compiler")
     .argName("classpath")
     .hasArg()
     .build()

  val help : cli.Option =
    cli.Option.builder("h")
     .longOpt("help")
     .desc("show this help message")
     .build()  

  private val options : cli.Options = {
    val options = List(method, showAst, showCfg, showCalls, showDataflow, dotOutput, files, mapLike, transformRemoveUnusedVars, dirs, classpath, help)
    options.foldLeft(new cli.Options){(acc, o) => acc.addOption(o)}
  }

  private[this] lazy val helpFormatter = new cli.HelpFormatter()

  def printHelp() : Unit = {
    val name = "ParScala"
    val header = "Analyses Scala source code\n\n"
    val footer = ""
    helpFormatter.printHelp(name, header, options, footer, true)
  }

  def parse(args : Array[String]) : Either[String, Config] = {
    val parser : cli.CommandLineParser = new cli.DefaultParser
    try {
      val result = parser.parse(options, args)
      Right(new Config(Option(result.getOptionValue(method.getOpt)),
                       result.hasOption(showAst.getOpt),
                       result.hasOption(showCfg.getOpt),
                       result.hasOption(showCalls.getOpt),
                       result.hasOption(showDataflow.getOpt),
                       Option(result.getOptionValue(dotOutput.getOpt)).map(Paths.get(_)),
                       result.hasOption(mapLike.getOpt),
                       result.hasOption(transformRemoveUnusedVars.getOpt),
                       result.getOptionValues(files.getOpt) match {
                         case null => List()
                         case xs => xs.toList.map(Paths.get(_))
                       },
                       result.getOptionValues(dirs.getOpt) match {
                         case null => List()
                         case ds => ds.toList.map(Paths.get(_))
                       },
                       Option(result.getOptionValue(classpath.getOpt)),
                       result.hasOption(help.getOpt)))
    } catch {
      case ex : cli.MissingArgumentException =>
        Left("missing argument for option '%s'".format(ex.getOption.getLongOpt))
      case ex : cli.MissingOptionException => {
        import scala.collection.JavaConverters._

        Left("missing required option(s): %s".format(ex.getMissingOptions.asScala.mkString(",")))
      }
      case _ : cli.ParseException => {
        Left("error parsing options")
      }
    }
  }
}
