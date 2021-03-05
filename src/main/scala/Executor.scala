import Table.Row

object Executor {

  sealed trait Result

  case object TableFull extends Result

  case class Success(table: Table) extends Result

  def execute(statement: Statement, table: Table): Result = statement.sType match {
    case Statement.Insert(rowToInsert) =>
      executeInsert(rowToInsert, table)
    case Statement.Select =>
      executeSelect(statement, table)
  }

  private def executeInsert(row: Row, table: Table): Result =
    table.insert(row) match {
      case Left(_)      => TableFull
      case Right(table) => Success(table)
    }

  private def executeSelect(statement: Statement, table: Table): Result = {
    table.pages.flatten.foreach(println)

    Success(table)
  }
}
