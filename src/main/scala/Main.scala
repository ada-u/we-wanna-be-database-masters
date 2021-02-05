object Main extends App {

  sealed trait MetaCommandResult

  case object MetaCommandSuccess extends MetaCommandResult

  case object MetaCommandUnrecognizedCommand extends MetaCommandResult

  sealed trait StatementType

  case class StatementInsert(rowToInsert: Row) extends StatementType

  case object StatementSelect extends StatementType

  sealed trait ExecuteResult

  case object ExecuteTableFull extends ExecuteResult

  case object ExecuteSuccess extends ExecuteResult

  case class Row(
                  id: Long,
                  username: String,
                  email: String
                ) {
    require(0 <= id && id <= 4294967295L, "invalid id size")
    require(username.lengthIs <= 32, "invalid username size")
    require(email.lengthIs <= 255, "invalid email size")
  }

  type Page = Vector[Row]

  class Table(
               numRows: Long,
               pages: Vector[Page]
             ) {
    //def +(row: Row): Either[RuntimeException, Table] = {
    //  if (numRows >= 100) {
    //    Left(new RuntimeException("exceed page size"))
    //  } else {
    //  Right(new Table(numRows + 1, pages :+ row))
    //  }
    //}
  }

  object Table {
    def apply(): Table = new Table(0L, Vector.empty)
  }

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
  val insertParameterRegexp = """ (\d+) (\S+) (.+)""".r

  def prepareStatement: String => PrepareResult = {
    case insertRegexp(param) => param match {
      case insertParameterRegexp(id, username, email) =>
        val row = Row(id.toLong, username, email)
        PrepareSuccess(Statement(StatementInsert(row)))
      case _ =>
        PrepareSyntaxError
    }
    case "select" => PrepareSuccess(Statement(StatementSelect))
    case _ => PrepareUnrecognizedStatement
  }

  def executeStatement(statement: Statement): ExecuteResult = statement.sType match {
    case StatementInsert(_) =>
      println("This is where we would do an insert.")
      executeInsert(statement, ???)
    case StatementSelect =>
      println("This is where we would do an select.")
      executeSelect(statement, ???)
  }

  def executeInsert(statement: Statement, table: Table): ExecuteResult = {
    ???
  }

  def executeSelect(statement: Statement, table: Table): ExecuteResult = {
    ???
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
        case PrepareSyntaxError =>
          println(s"Syntax error. Could not parse statement.")
      }
  }

}
