import Parser.prepareStatement

object Main extends App {

  val fileName: String = args.head
  var table: Table     = Table(fileName)

  def printPrompt(): Unit =
    print("db > ")

  Iterator
    .continually {
      printPrompt()
      scala.io.StdIn.readLine()
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
            Executor.execute(statement, table) match {
              case Executor.TableFull =>
                println("Error: Table full.")
              case Executor.Success(newTable) =>
                this.table = newTable
                println("Executed.")
            }
          case Parser.UnrecognizedStatement => println(s"Unrecognized keyword at start of '$rawStatement'")
          case Parser.SyntaxError           => println("Syntax error. Could not parse statement.")
          case Parser.NegativeIdError       => println("Negative id error.")
          case Parser.StringTooLongError    => println("String too long error.")
        }
    }

}
