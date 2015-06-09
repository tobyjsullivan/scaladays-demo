package models

import play.api.db.DB
import play.api.libs.json.Json
import anorm._
import play.api.Play.current
import play.api.libs.ws.WS

import scala.annotation.tailrec
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

case class ShortUrl(hash: String, fullUrl: String)

object ShortUrl {
  implicit val fmt = Json.format[ShortUrl]

  def shorten(fullUrl: String): Future[ShortUrl] = {
    getNextId flatMap { id =>
      val hash = hashId(id)

      ShortUrlDAO.saveMapping(hash, fullUrl) map { _ =>
        ShortUrl(hash, fullUrl)
      }
    }
  }

  def lookup(hash: String): Future[Option[ShortUrl]] = {
    ShortUrlDAO.findByHash(hash) collect {
      case Some(fullUrl) => Some(ShortUrl(hash, fullUrl))
    }
  }

  private val countUrl = "http://localhost:9005/ids/urlshortener"
  private val countRequestHolder = WS.url(countUrl)

  private def getNextId: Future[Int] = {
    countRequestHolder.post("") map { response =>
      (response.json \ "result").as[Int]
    }
  }

  private val hashIndex = "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
  @tailrec
  private def hashId(id: Int, acc: String = ""): String =
    if(id == 0)
      acc
    else
      hashId(id / hashIndex.length, acc + hashIndex.charAt(id % hashIndex.length))
}

object ShortUrlDAO {
  def findByHash(hash: String): Future[Option[String]] = Future {
    DB.withConnection { implicit c =>
      val sql = SQL("SELECT fullurl FROM shorturls WHERE short = {short} LIMIT 1")
        .on("short" -> hash)

      sql().headOption map { row =>
        row[String]("fullurl")
      }
    }
  }

  def saveMapping(hash: String, fullUrl: String): Future[Unit] = Future {
    DB.withConnection { implicit c =>
      val sql = SQL("INSERT INTO shorturls (short, fullurl) VALUES ({hash}, {fullurl})")
        .on("hash" -> hash, "fullurl" -> fullUrl)

      sql.executeInsert()
    }
  }
}
