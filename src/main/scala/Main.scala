import Parser.prepareStatement

object Main extends App {
  implicit val table: Table = new Table(1, Vector(Vector(Table.Row(
    0,
    "Tajiri",
    "tajiri@xxx.com"
  ))))

  def printPrompt(): Unit =
    print("db > ")

  Iterator
    .continually {
      printPrompt()
      io.StdIn.readLine()
    }
    .foreach {
      case MetaCommand.regexp(metaCommand) =>
        MetaCommand.execute(metaCommand) match {
          case MetaCommand.Success => ()
          case MetaCommand.UnrecognizedCommand =>
            println(s"Unrecognized command '.$metaCommand'")
        }
      case rawStatement =>
        prepareStatement(rawStatement) match {
          case Parser.Success(statement) =>
            Executor.execute(statement)
            println("Executed.")
          case Parser.UnrecognizedStatement =>
            println(s"Unrecognized keyword at start of '$rawStatement'")
          case Parser.SyntaxError =>
            println(s"Syntax error. Could not parse statement.")
        }
    }

}
