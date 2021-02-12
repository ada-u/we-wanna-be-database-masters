import Table.Row

object Parser {

  private val insertRegexp = """insert(.+)""".r
  private val insertParameterRegexp = """ (\d+) (\S+) (.+)""".r

  sealed trait Result

  case class Success(statement: Statement) extends Result

  case object UnrecognizedStatement extends Result

  case object SyntaxError extends Result

  def prepareStatement: String => Result = {
    case insertRegexp(param) => param match {
      case insertParameterRegexp(id, username, email) =>
        val row = Row(id.toLong, username, email)
        Success(Statement(Statement.Insert(row)))
      case _ =>
        SyntaxError
    }
    case "select" => Success(Statement(Statement.Select))
    case _ => UnrecognizedStatement
  }

}
