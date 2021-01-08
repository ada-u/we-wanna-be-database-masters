object Main extends App {

  def printPrompt(): Unit =
    print("db > ")

  Iterator.continually {
    printPrompt()
    io.StdIn.readLine()
  }.foreach {
    case ".exit" => System.exit(0)
    case input => println(s"Unrecognized command $input.")
  }

}
