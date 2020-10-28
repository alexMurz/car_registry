package controllers.resources

import java.time.LocalDate
import java.util.NoSuchElementException

import controllers.BaseRouter
import javax.inject.{Inject, Provider}
import play.api.Logger
import play.api.http.Status
import play.api.libs.json._
import repo._

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContext, Future}

case class ResourceDataPoint
(
/// Extras
link: String,
// From DataPoint
id: String, num: String, mark: String, color: String, date: String
)
object ResourceDataPoint {
  implicit val format: Format[ResourceDataPoint] = Json.format

  /// Create from DataPoint and link generator
  def apply(v: DataPoint, linkGen: DataId => String): ResourceDataPoint = {
    ResourceDataPoint(
      linkGen(v.id),
      v.id.id.toString,
      v.num,
      v.mark,
      v.color,
      v.date.toString
    )
  }
}

////////////////////////////////////////////////////
// Json DataPoint deserializing errors, shortened to `De`

sealed trait DeFail
object DeFail {
  def toJson(ty: DeFail): JsValue = { ty match {
    case InsertFailMissingField(name) => JsObject(Seq(
      "ty" -> JsString("Missing Field"),
      "desc" -> JsString(s"Missing required field: `$name`")
    ))
    case InsertFailMalformedField(field, got, ty, ex) => JsObject(Seq(
      "ty" -> JsString("Malformed Input"),
      "desc" -> JsString(s"Malformed input for field `$field`, expected type `$ty` (ex: `$ex`), got `$got`")
    ))
  } }
}
/// Expected field, but its missing
case class InsertFailMissingField(name: String) extends DeFail
/// Unable to deserialize field `field`, `got` as type `ty`, example `ex`
case class InsertFailMalformedField(field: String, got: String, ty: String, ex: String) extends DeFail


////////////////////////////////////////////////////
// Success responses

/// Lookup successful
case class LookupSuccess(data: ResourceDataPoint) extends QSuccess {
  override def json: Option[JsValue] = Some(Json.toJson(data))
}

/// Successfully removed item
case class RemoveOK() extends QSuccess

/// Data inserted with `id`
case class InsertSuccess(id: DataId) extends QSuccess {
  override def json: Option[JsValue] = Some(JsNumber(id.id))
}

////////////////////////////////////////////////////
// Fail responses

// No Data for ID
case class EmptyID(id: DataId) extends QFailure {
  override def status: Int = Status.NO_CONTENT
}

/// Error parsing ID
case class BadID(id: String) extends QFailure {
  override def json: Option[JsValue] = Some(JsString(s"Malformed ID, expected number, got `$id`"))
}

/// Request body is not json
case class BodyNotJson() extends QFailure {
  override def json: Option[JsValue] = Some(JsString("Request body is not Json object"))
}

/// Failed to deserialize from json
case class InsertParseFail(errors: Seq[DeFail]) extends QFailure {
  override def json: Option[JsValue] = Some(JsArray(errors.map(DeFail.toJson)))
}

/// Util for RemoveOK and EmptyID
object RemoveResult {
  // If false, remove empty ID error
  def apply(id: DataId, flag: Boolean): QResult = {
    if (flag) {
      RemoveOK()
    } else {
      EmptyID(id)
    }
  }
}


/// ResourceHandler trait, to wrap resource handler with performance tracker and then return same trait for ease of use
trait ResourceHandler {
  // Return all items
  def getAll: Future[QResult]
  // Find item by ID
  def lookup(id: String): Future[QResult]
  // Insert item, using JsValue
  def insert(body: Option[JsValue]): Future[QResult]
  // Remove item by ID
  def remove(id: String): Future[QResult]
}
trait StatTracker {
  def stats: Future[QResult]
}


class DataResourceHandler @Inject()(routerProvider: Provider[BaseRouter], repo: DataRepository)
                                   (implicit ec: ExecutionContext)
  extends ResourceHandler
{
  private val logger = Logger(this.getClass)

  override def getAll: Future[QResult] = {
    logger.trace("getAll")
    repo.getAll.map { x => QSuccess(x.map(toResource)) }
  }

  /// Find entry with ID
  override def lookup(id: String): Future[QResult] = {
    logger.trace(s"lookup($id)")
    // Parse ID or return BadID
    DataId(id).map { id =>
      // get object, map future
      repo.get(id).map { inner: Option[DataPoint] =>
        // Some to LookupSuccess, None to EmptyID
        toResource(inner)
          .map(LookupSuccess)
          .getOrElse { EmptyID(id) }
      }
    }.getOrElse(Future { BadID(id) })
  }

  /// LocalDate formatter for `def insert`
  /// Using Option[JsObject] allows to use GET request to POST new entry
  override def insert(body: Option[JsValue]): Future[QResult] = {
    logger.trace(s"Insert new")

    // Manual json deserialization to collect all detected errors in one objects, instead of providing them one by one
    // on successive `insert` calls
    body.map { json =>
      // List of accumulated error, if empty => success, or return error
      val errors = new ArrayBuffer[DeFail]()

      // Return field (or None if error)
      // If error, add missingField to `errors`
      def getField[T](name: String)(implicit rd: Reads[T]): Option[T] = {
        try json(name).asOpt
        catch {
          case _: NoSuchElementException =>
            logger.trace(s"Missing field: $name")
            errors.addOne(InsertFailMissingField(name))
            None
        }
      }

      // Get opts
      val num = getField[String]("num")
      val mark = getField[String]("mark")
      val color = getField[String]("color")
      val raw_date = getField[String]("date")

      // Try parse date, but only if it exists
      val date = raw_date.flatMap { raw_date =>
        try {
          Some(LocalDate.parse(raw_date))
        } catch {
          case _: Throwable =>
            logger.trace(s"Malformed date: $raw_date")
            errors.addOne(InsertFailMalformedField("date", raw_date, "LocalDate", "2020-12-10 (ISO Format YYYY-MM-DD)"))
            None
        }
      }

      if (errors.isEmpty) {
        val point = new DataPoint(DataId(0), num.get, mark.get, color.get, date.get)
        repo.insert(point).map(InsertSuccess)
      } else {
        Future { InsertParseFail(errors.toSeq) }
      }
    }.getOrElse(Future { BodyNotJson() })
  }

  /// Remove element with ID
  override def remove(id: String): Future[QResult] = {
    DataId(id).map { id =>
      repo.remove(id).map(x => RemoveResult(id, x))
    }.getOrElse(Future { BadID(id) })
  }

  // Link generator from DataId => Path to see entry with this DataId
  private val linkGen: DataId => String = id => routerProvider.get().link(id)
  private def toResource(v: DataPoint) = ResourceDataPoint(v, linkGen)
  private def toResource(v: Option[DataPoint]): Option[ResourceDataPoint] = v.map(toResource)
}
