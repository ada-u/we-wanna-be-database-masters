import Table.Page

case class Table(
    numRows: Long,
    pages: Vector[Page]
) {
  // MEMO:
  // numRows ≠ numPages
  // 一旦データの追加は度外視して、空の Table を表示させてみる

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

  val PAGE_SIZE = 4096
  val TABLE_MAX_PAGES = 100
  val ROWS_PER_PAGE = PAGE_SIZE / Row.ROW_SIZE
  val TABLE_MAX_ROWS = ROWS_PER_PAGE * TABLE_MAX_PAGES

  object Row {
    val ROW_SIZE = 512
  }

  case class Row(
      id: Long,
      username: String,
      email: String
  ) {
    require(0 <= id && id <= 4294967295L, "invalid id size")
    require(username.lengthIs <= 32, "invalid username size")
    require(email.lengthIs <= 255, "invalid email size")

    override def toString: String =
      s"($id, $username, $email)"
  }

  def apply(): Table = new Table(0L, Vector.empty)
}
