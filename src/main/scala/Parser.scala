import Table.Row

object Parser {

  private val insertRegexp          = """insert(.+)""".r
  private val insertParameterRegexp = """ (\d+) (\S+) (.+)""".r

  sealed trait Result

  case class Success(statement: Statement) extends Result

  case object UnrecognizedStatement extends Result

  case object SyntaxError extends Result

  case object NegativeIdError extends Result

  case object StringTooLongError extends Result

  def prepareStatement: String => Result = {
    case insertRegexp(param) =>
      param match {
        case insertParameterRegexp(id, _, _) if id.toLong < 0 =>
          NegativeIdError
        case insertParameterRegexp(idString, username, email) =>
          val id = idString.toLong
          if (id < 0)
            NegativeIdError
          else if (username.lengthIs > Row.USERNAME_LENGTH)
            StringTooLongError
          else if (email.lengthIs > Row.EMAIL_LENGTH)
            StringTooLongError
          else
            Success(Statement(Statement.Insert(Row(id, username, email))))
        case _ =>
          SyntaxError
      }
    case "select" => Success(Statement(Statement.Select))
    case _        => UnrecognizedStatement
  }

}
