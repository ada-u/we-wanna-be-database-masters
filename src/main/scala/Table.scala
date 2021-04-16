import Table.Page

case class Table(
    numRows: Long,
    //pages: Vector[Page]
    pager: Pager
) {

  import Table._

  private val pages = pager.pages

  def insert(row: Row): Either[RuntimeException, Table] =
    if (numRows < TABLE_MAX_ROWS) {
      val newPages: Vector[Page] =
        if (pages.lastOption.exists(_.lengthIs < ROWS_PER_PAGE)) {
          val newLastPage = pages.last :+ row
          pages.init :+ newLastPage

        } else pages :+ Vector(row)

      Right(Table(numRows + 1, Pager(newPages)))

    } else Left(new RuntimeException("exceed page size"))

}

object Table {

  type Page = Vector[Row]

  val PAGE_SIZE       = 4096
  val TABLE_MAX_PAGES = 100
  val ROWS_PER_PAGE   = PAGE_SIZE / Row.ROW_SIZE
  val TABLE_MAX_ROWS  = ROWS_PER_PAGE * TABLE_MAX_PAGES

  def apply(): Table = new Table(0L, Pager(Vector.empty))

  def apply(fileName: String): Table = {
    val pager   = Pager(fileName)
    val numRows = pager.numRows(Row.ROW_SIZE)

    new Table(numRows, pager)
  }

  object Row {
    val ROW_SIZE = 512

    val USERNAME_LENGTH = 32
    val EMAIL_LENGTH    = 255
  }

  case class Row(
      id: Long,
      username: String,
      email: String
  ) {
    import Row._

    require(0 <= id && id <= 4294967295L, "invalid id size")
    require(username.lengthIs <= USERNAME_LENGTH, "invalid username size")
    require(email.lengthIs <= EMAIL_LENGTH, "invalid email size")

    override def toString: String =
      s"($id, $username, $email)"
  }
}

case class Pager(pages: Vector[Page], fileLength: Int) {

  def numRows(rowSize: Int): Int =
    fileLength / rowSize
}

object Pager {
  def apply(fileName: String): Pager = {

    // e.g. https://docs.oracle.com/javase/jp/8/docs/api/java/nio/file/Files.html#newByteChannel-java.nio.file.Path-java.util.Set-java.nio.file.attribute.FileAttribute...-
    // // create file with initial permissions, opening it for both reading and writing
    // FileAttribute<Set<PosixFilePermission>> perms = ...
    // SeekableByteChannel sbc = Files.newByteChannel(path, EnumSet.of(CREATE_NEW,READ,WRITE), perms);

//    Pager(Vector.empty, 0)
  }
}
