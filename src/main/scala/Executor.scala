import Table.Row

object Executor {

  sealed trait Result

  case object TableFull extends Result

  case object Success extends Result

  def execute(statement: Statement)(implicit table: Table): Result = statement.sType match {
    case Statement.Insert(rowToInsert) =>
      executeInsert(rowToInsert, table)
    case Statement.Select =>
      executeSelect(statement, table)
  }

  private def executeInsert(row: Row, table: Table): Result =
    table.insert(row) match {
      case Left(_)  => TableFull
      case Right(_) => Success // TODO Table引き回す
    }

  private def executeSelect(statement: Statement, table: Table): Result = {
    table.pages.flatten.foreach(println)

    Success
  }
}
