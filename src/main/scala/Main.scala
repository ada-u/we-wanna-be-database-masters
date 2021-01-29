object Main extends App {

  sealed trait MetaCommandResult
  case object MetaCommandSuccess extends MetaCommandResult
  case object MetaCommandUnrecognizedCommand extends MetaCommandResult

  sealed trait StatementType
  case object StatementInsert extends StatementType
  case object StatementSelect extends StatementType

  case class Statement(sType: StatementType)

  sealed trait PrepareResult
  case class PrepareSuccess(statement: Statement) extends PrepareResult
  case object PrepareUnrecognizedStatement extends PrepareResult
  case object PrepareSyntaxError extends PrepareResult

  def printPrompt(): Unit =
    print("db > ")

  def doMetaCommand: String => MetaCommandResult = {
    case "exit" =>
      System.exit(0)
      MetaCommandSuccess
    case _ =>
      MetaCommandUnrecognizedCommand
  }

  val insertRegexp = """insert(.+)""".r
  val insertParameterRegexp = """ (\d+) (\\S+) (.+)""".r

  def prepareStatement: String => PrepareResult = {
    case insertRegexp(param) => param match {
      case insertParameterRegexp(id, username, email) =>
        PrepareSuccess(Statement(StatementInsert))
      case _ =>
        PrepareSyntaxError
    }
    case "select" => PrepareSuccess(Statement(StatementSelect))
    case _ => PrepareUnrecognizedStatement
  }

  def executeStatement(statement: Statement): Unit = statement.sType match {
    case StatementInsert => println("This is where we would do an insert.")
    case StatementSelect => println("This is where we would do an select.")
  }

  val metaCommandRegexp = """\.(.+)""".r

  Iterator.continually {
    printPrompt()
    io.StdIn.readLine()
  }.foreach {
    case metaCommandRegexp(metaCommand) => 
      doMetaCommand(metaCommand) match {
        case MetaCommandSuccess => ()
        case MetaCommandUnrecognizedCommand =>
          println(s"Unrecognized command '.$metaCommand'")
      }
    case rawStatement => 
      prepareStatement(rawStatement) match {
        case PrepareSuccess(statement) =>
          executeStatement(statement)
          println("Executed.")
        case PrepareUnrecognizedStatement =>
          println(s"Unrecognized keyword at start of '$rawStatement'")
      }
  }

}
