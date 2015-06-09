package controllers

import models.ShortUrl
import play.api._
import play.api.cache.Cached
import play.api.libs.json.Json
import play.api.mvc._
import play.api.Play.current

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Try

class Application extends Controller {

  def shorten = Action.async { request =>
    val oFullUrl: Option[String] = request.body.asJson flatMap { js =>
      Try((js \ "fullurl").as[String]).toOption
    }

    oFullUrl match {
      case None => Future.successful(BadRequest("No full URL received in request"))
      case Some(fullUrl) =>
        ShortUrl.shorten(fullUrl) map { shortUrl =>
          Ok(Json.obj("result" -> shortUrl))
        }
    }
  }

  def redirect(hash: String) = Cached("redirect."+hash) {
    Action.async {
      ShortUrl.lookup(hash) map {
        case None => NotFound("That URL doesn't exist")
        case Some(shortUrl) =>
          Redirect(shortUrl.fullUrl, 301)
      }
    }
  }

}
