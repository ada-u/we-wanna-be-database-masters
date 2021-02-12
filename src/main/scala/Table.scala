import Table.Page

class Table(
             numRows: Long,
             pages: Vector[Page]
           ) {
  //def +(row: Row): Either[RuntimeException, Table] = {
  //  if (numRows >= 100) {
  //    Left(new RuntimeException("exceed page size"))
  //  } else {
  //  Right(new Table(numRows + 1, pages :+ row))
  //  }
  //}
}

object Table {

  type Page = Vector[Row]

  case class Row(
                  id: Long,
                  username: String,
                  email: String
                ) {
    require(0 <= id && id <= 4294967295L, "invalid id size")
    require(username.lengthIs <= 32, "invalid username size")
    require(email.lengthIs <= 255, "invalid email size")
  }

  def apply(): Table = new Table(0L, Vector.empty)
}
