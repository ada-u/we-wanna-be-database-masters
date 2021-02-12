import Table.Row

case class Statement(sType: Statement.Type)

object Statement {
  sealed trait Type

  case class Insert(rowToInsert: Row) extends Type

  case object Select extends Type
}


