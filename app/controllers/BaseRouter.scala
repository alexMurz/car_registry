package controllers

import javax.inject._
import play.api.routing.Router.Routes
import play.api.routing.SimpleRouter
import play.api.routing.sird._
import repo.DataId


/**
 * This controller creates an `Action` to handle HTTP requests to the
 * application's home page.
 */
@Singleton
class BaseRouter @Inject()(val controller: MainController) extends SimpleRouter {

  /// Construct link for data ID
  def link(id: DataId) = s"/$id"

  override def routes: Routes = {
    case GET(p"/") => controller.index
    case GET(p"/stats") => controller.stats
    case GET(p"/$id") => controller.get(id)
    case POST(p"/insert") => controller.insert
    case DELETE(p"/remove/$id") => controller.remove(id)
  }
}

