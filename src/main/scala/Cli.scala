import org.apache.commons.cli

class Cli {
  private[this] val method : cli.Option = 
    cli.Option.builder("m")
     .argName("method")
     .longOpt("method")
     .desc("select a method, e.g. com.foo.bar")
     .hasArg()
     .required()
     .build()

  private[this] val showCfg : cli.Option = 
    cli.Option.builder("g")
     .longOpt("show-cfg")
     .desc("show the control flow graph in a window")
     .required(false)
     .build()

  private[this] val dotOutput : cli.Option = 
    cli.Option.builder("o")
     .desc("dump the control flow graph into a dot file")
     .argName("filename")
     .required(false)
     .hasArg()
     .build()

  private[this] val files : cli.Option = 
    cli.Option.builder("f")
     .longOpt("files")
     .desc("source files to analyse")
     .argName("files")
     .hasArgs()
     .numberOfArgs(cli.Option.UNLIMITED_VALUES)
     .required()
     .build()

  val help : cli.Option =
    cli.Option.builder("h")
     .longOpt("help")
     .desc("show this help message")
     .build()  

  private val options : cli.Options = {
    val options = List(method, showCfg, dotOutput, files, help)
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
      Right(new Config(result.getOptionValue(method.getOpt, ""),
                       result.hasOption(showCfg.getOpt),
                       result.getOptionValue(dotOutput.getOpt, ""),
                       result.getOptionValues(files.getOpt) match {
                         case null => List()
                         case xs => xs.toList
                       },
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
