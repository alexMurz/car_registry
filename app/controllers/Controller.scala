package controllers

import controllers.resources.{DataResourceHandler, ResourceHandler, StatTracker, TrackedResourceHandler}
import javax.inject.Inject
import play.api.http.{FileMimeTypes, HttpVerbs}
import play.api.i18n.{Langs, MessagesApi}
import play.api.mvc._
import play.api.{Logger, MarkerContext}
import repo.QResult

import scala.concurrent.{ExecutionContext, Future}

trait MainRequestHeader extends MessagesRequestHeader with PreferredMessagesProvider

class MainRequest[A](request: Request[A], val messagesApi: MessagesApi)
  extends WrappedRequest(request) with MainRequestHeader

/**
 * This is the place to put logging, metrics, to augment
 * the request with contextual data, and manipulate the
 * result.
 */
class MainActionBuilder @Inject()(messagesApi: MessagesApi, playBodyParsers: PlayBodyParsers)
                                 (implicit val executionContext: ExecutionContext)
  extends ActionBuilder[MainRequest, AnyContent]
    with RequestMarkerContext
    with HttpVerbs {

  override val parser: BodyParser[AnyContent] = playBodyParsers.anyContent

  type RequestBlock[A] = MainRequest[A] => Future[Result]

  private val logger = Logger(this.getClass)

  override def invokeBlock[A](request: Request[A], block: RequestBlock[A]): Future[Result] = {
    // Convert to marker context and use request in block
    implicit val markerContext: MarkerContext = requestHeaderToMarkerContext(request)
    logger.trace(s"invokeBlock: ")

    val future = block(new MainRequest(request, messagesApi))

    future.map { result =>
      request.method match {
        case GET | HEAD =>
          result.withHeaders("Cache-Control" -> s"max-age: 100")
        case other =>
          result
      }
    }
  }
}

/**
 * Packaged resources for MainController
 */
case class MainControllerComponents @Inject()
(mainActionBuilder: MainActionBuilder,
 dataResourceHandler: DataResourceHandler,
 actionBuilder: DefaultActionBuilder,
 parsers: PlayBodyParsers,
 messagesApi: MessagesApi,
 langs: Langs,
 fileMimeTypes: FileMimeTypes,
 executionContext: ExecutionContext
) extends ControllerComponents


class BaseMainController @Inject()(cc: MainControllerComponents)
  extends BaseController with RequestMarkerContext {
  override protected def controllerComponents: ControllerComponents = cc

  def MainAction: MainActionBuilder = cc.mainActionBuilder

  private val res = new TrackedResourceHandler(cc.dataResourceHandler)(cc.executionContext)
  def resources: ResourceHandler = res
}

class MainController @Inject()(cc: MainControllerComponents)(implicit ec: ExecutionContext)
  extends BaseMainController(cc) {

  private val logger = Logger(getClass)
  implicit val results: Results = this

  def index: Action[AnyContent] = MainAction.async { implicit request =>
    logger.trace(s"index ${request.queryString.mkString(";")}")
    resources.getAll(request.queryString).map { QResult.toResult(_) }
  }

  def get(id: String): Action[AnyContent] = MainAction.async { implicit request =>
    logger.trace(s"get($id): ")
    resources.lookup(id).map { QResult.toResult(_) }
  }

  def insert: Action[AnyContent] = MainAction.async { implicit request =>
    logger.trace(s"insert")
    resources.insert(request.body.asJson).map { QResult.toResult(_) }
  }

  def remove(id: String): Action[AnyContent] = MainAction.async { implicit request =>
    logger.trace(s"remove($id): ")
    resources.remove(id).map { QResult.toResult(_) }
  }

  def stats: Action[AnyContent] = MainAction.async { implicit request =>
    logger.trace(s"stats")
    resources match {
      case tracker: StatTracker =>
        tracker.stats.map { QResult.toResult(_) }
      case _ =>
        Future { NoContent }
    }
  }

}

