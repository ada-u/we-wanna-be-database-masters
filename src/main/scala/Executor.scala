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
    if (table.numRows >= Table.TABLE_MAX_ROWS) {
      TableFull
    } else {
      // MEMO:
      // 現状 val で table を引き回しているため、変更が不可能
      // Immutable に table を取り回したい場合は、戻り値などの変更も必要
      // とはいえ、データベース全体を Immutable に取りまわすってどうよ 😇

      Success
    }
  }

  private def executeSelect(statement: Statement, table: Table): Result = {
    table.pages.flatten.foreach(println)

    Success
  }
}
