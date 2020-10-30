package repo

import java.time.LocalDate

import akka.actor.ActorSystem
import javax.inject.Inject
import play.api.libs.concurrent.CustomExecutionContext

import scala.concurrent.Future

/**
 * @param id - Underlying data ID
 */
case class DataId private (id: Int) {
  override def toString: String = id.toString
}
object DataId {
  def apply(i: Int) = new DataId(i)
  def apply(s: String): Option[DataId] = s.toIntOption.map(DataId(_))
}

/**
  * Repository data point (row)
  */
class DataPoint(var id: DataId, var num: String, var mark: String, var color: String, var date: LocalDate) {
}

/**
 * DataPoint column names, for filtering purposes
 */
sealed trait DataColumn
case object DataColumnId extends DataColumn
case object DataColumnNum extends DataColumn
case object DataColumnMark extends DataColumn
case object DataColumnColor extends DataColumn
case object DataColumnDate extends DataColumn

/**
 * Filter Params
 */
case class DataFilter(col: DataColumn, bounds: Bounds[String])


class RepositoryContext @Inject()(actorSystem: ActorSystem)
  extends CustomExecutionContext(actorSystem, "repository.dispatcher")


/**
  * Base repository interface, can be anything beyond the trait (local list or data base connection)
  */
trait DataRepository {

  /// Return all DataPoint in this repository
  def getAll(filters: Seq[DataFilter]): Future[Iterable[DataPoint]]

  /// Find for ID
  def get(id: DataId): Future[Option[DataPoint]]

  /// Add new entry, return new entries ID
  def insert(point: DataPoint): Future[DataId]

  /// Remove entry with id, return success flag
  def remove(id: DataId): Future[Boolean]

}
