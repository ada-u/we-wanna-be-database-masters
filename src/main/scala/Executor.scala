object Executor {

  sealed trait Result

  case object TableFull extends Result

  case object Success extends Result

  def execute(statement: Statement)(implicit table: Table): Result = statement.sType match {
    case Statement.Insert(_) =>
      println("This is where we would do an insert.")
      executeInsert(statement, table)
    case Statement.Select =>
      executeSelect(statement, table)
  }

  private def executeInsert(statement: Statement, table: Table): Result = {
    ???
  }

  private def executeSelect(statement: Statement, table: Table): Result = {
    table.pages.flatten.foreach(println)

    Success
  }
}
