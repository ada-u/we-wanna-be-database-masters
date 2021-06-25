import Table.Page

import scala.util.{ Failure, Success, Try }
import java.io.{ FileInputStream, ObjectInputStream, RandomAccessFile }

case class Table(
    numRows: Long,
    pager: Pager
)(implicit private val outputFile: RandomAccessFile) {

  import Table._

  val pages: Vector[Page] = pager.pages

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

  def init(implicit outputFile: RandomAccessFile): Table = {
    Table(0, Pager.empty)
  }

  def apply(fileName: String)(implicit outputFile: RandomAccessFile): Table = {
    val pagerTry = for {
      fileInputStream <- Try(new FileInputStream(fileName))
      objectInputStream = new ObjectInputStream(fileInputStream)
      pager: Pager      = objectInputStream.readObject.asInstanceOf[Pager]
      _                 = objectInputStream.close()
    } yield pager

    pagerTry match {
      case Failure(_) => Table.init // ここは読み込み処理なので、ファイル作成までは担わない
      case Success(pager) =>
        val numRows = pager.pages.map(_.size).sum
        Table(numRows, pager)
    }
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

@SerialVersionUID(0L)
case class Pager(pages: Vector[Page])

object Pager {
  def empty: Pager = Pager(Vector.empty)
}
