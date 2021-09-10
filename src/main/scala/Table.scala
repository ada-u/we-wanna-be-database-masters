import Table.Page

import java.io.{ File, FileNotFoundException, FileWriter }
import scala.util.{ Failure, Success, Try }

import io.circe.generic.auto._
import io.circe.syntax._
import io.circe.parser._

case class Table(
    numRows: Long,
    pager: Pager
) {
  import Table._

  val pages: Vector[Page] = pager.pages

  def insert(row: Row): Either[Throwable, Table] =
    if (numRows >= TABLE_MAX_ROWS) Left(new RuntimeException("exceed page size"))
    else {
      val newPages: Vector[Page] =
        if (pages.lastOption.exists(_.lengthIs < ROWS_PER_PAGE)) {
          val newLastPage = pages.last :+ row
          pages.init :+ newLastPage

        } else pages :+ Vector(row)

      // Pagerに包んでから保存するひつようあり
      val newPager = Pager(newPages)

      Try {
        val f = new FileWriter(new File("unko.json"))

        f.write(newPager.asJson.noSpaces)
        f.close()
        Table(numRows + 1, newPager)
      }.toEither

    }

}

object Table {

  type Page = Vector[Row]

  val PAGE_SIZE           = 4096
  val TABLE_MAX_PAGES     = 100
  val ROWS_PER_PAGE: Int  = PAGE_SIZE / Row.ROW_SIZE
  val TABLE_MAX_ROWS: Int = ROWS_PER_PAGE * TABLE_MAX_PAGES

  def init: Table = Table(0, Pager.empty)

  def apply(fileName: String): Table = {
    val pagerTry = for {
      source <- Try(scala.io.Source.fromFile(fileName))
      rawString = source.getLines().mkString
      _         = source.close()
      pager <- decode[Pager](rawString).toTry
    } yield pager

    pagerTry match {
      case Success(pager) =>
        val numRows = pager.pages.map(_.size).sum
        Table(numRows, pager)
      case Failure(_: FileNotFoundException) =>
        Table.init // ここは読み込み処理なので、ファイル作成までは担わない
      case Failure(e) =>
        throw e
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

case class Pager(pages: Vector[Page])

object Pager {
  def empty: Pager = Pager(Vector.empty)
}
