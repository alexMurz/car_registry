package repo

import play.api.http.Status
import play.api.libs.json._
import play.api.mvc._

/// Quick Request results

/***
 *
 * Success.toJson => {
 *    "success": true
 *    if this.json is Some then
 *      "result": self.json
 *    else nothing
 * }
 *
 */

sealed trait QResult
object QResult {
  def toJson(v: QResult): JsObject = {v match {
    case success: QSuccess => success.json.map { x => JsObject(Seq(
      "success" -> JsTrue,
      "result" -> x
    ))}.getOrElse { JsObject(Seq(
      "success" -> JsTrue
    ))}
    case error: QFailure => error.json.map { x => JsObject(Seq(
      "success" -> JsFalse,
      "result" -> x
    ))}.getOrElse { JsObject(Seq(
      "success" -> JsFalse
    ))}
  } }
  def toResult(v: QResult)(implicit results: Results): Result = v match {
    case success: QSuccess => results.Status(success.status)(QResult.toJson(v))
    case failure: QFailure => results.Status(failure.status)(QResult.toJson(v))
  }
}

trait QSuccess extends QResult {
  def json: Option[JsValue] = None
  def status: Int = Status.OK
}
object QSuccess {
  def apply(): QSuccess = new QSuccess {}
  def apply[T](data: T)(implicit w: Writes[T]): QSuccess = new QSuccess {
    override def json: Option[JsValue] = Some(Json.toJson(data))
  }
}

trait QFailure extends QResult {
  def json: Option[JsValue] = None
  def status: Int = Status.BAD_REQUEST
}

object QFailure {
  def apply(): QFailure = new QFailure {}
  def apply[T](data: T)(implicit w: Writes[T]): QFailure = new QFailure {
    override def json: Option[JsValue] = Some(Json.toJson(data))
  }
}
