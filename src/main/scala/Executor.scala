object Executor {

  sealed trait Result

  case object TableFull extends Result

  case object Success extends Result

  def execute(statement: Statement): Result = statement.sType match {
    case Statement.Insert(_) =>
      println("This is where we would do an insert.")
      executeInsert(statement, ???)
    case Statement.Select =>
      println("This is where we would do an select.")
      executeSelect(statement, ???)
  }

  private def executeInsert(statement: Statement, table: Table): Result = {
    ???
  }

  private def executeSelect(statement: Statement, table: Table): Result = {
    ???
  }
}

