package controllers

import javax.inject._
import play.api._
import play.api.mvc._
import com.thoughtbot._

case class PunResult(word: Option[String], puns: List[Pun])

/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class HomeController @Inject()(cc: ControllerComponents) extends AbstractController(cc) {

  /**
   * Create an Action to render an HTML page.
   *
   * The configuration in the `routes` file means that this method
   * will be called when the application receives a `GET` request with
   * a path of `/`.
   */
  def index() = Action { implicit request: Request[AnyContent] =>
    val oword = request.getQueryString("word")
    val result: PunResult = oword match {
      case Some(word) => PunResult(Some(word), Puns.getPuns(word))
      case _ => PunResult(None, List())
    }

    Ok(views.html.index(result))
  }
}
