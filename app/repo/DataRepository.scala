package repo

import java.time.LocalDate

import akka.actor.ActorSystem
import javax.inject.Inject
import play.api.libs.concurrent.CustomExecutionContext

import scala.collection.mutable.ListBuffer
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

class RepositoryContext @Inject()(actorSystem: ActorSystem)
  extends CustomExecutionContext(actorSystem, "repository.dispatcher")


/**
  * Base repository interface, can be anything beyond the trait (local list or data base connection)
  */
trait DataRepository {

  /// Return all DataPoint in this repository
  def getAll: Future[Iterable[DataPoint]]

  /// Find for ID
  def get(id: DataId): Future[Option[DataPoint]]

  /// Add new entry, return new entries ID
  def insert(point: DataPoint): Future[DataId]

  /// Remove entry with id, return success flag
  def remove(id: DataId): Future[Boolean]

}

/// List Repository
//@Singleton
class ListRepository(list: ListBuffer[DataPoint])(implicit val ec: RepositoryContext)
  extends DataRepository
{

  override def getAll: Future[Iterable[DataPoint]] = Future { list }

  override def get(id: DataId): Future[Option[DataPoint]] = Future { list.find(_.id == id) }

  override def insert(point: DataPoint): Future[DataId] = Future { ListRepository.this.synchronized {
    val id = nextID
    point.id = id
    list += point
    id
  } }

  override def remove(id: DataId): Future[Boolean] = Future { ListRepository.this.synchronized {
    // ListBuffer does not have retain, or findIndexed?
    list
      .find(_.id == id)
      .map(item => list.indexOf(item))
      .exists(index =>
        if (index >= 0) {
          list.remove(index)
          true
        } else false
      )
  } }

  /**
   * @return next empty ID
   */
  private var ai = 0 // Auto Increment index
  private def nextID: DataId = {
    val i = ai
    ai += 1
    DataId(i)
  }

}

class EmptyListRepository @Inject()(implicit ec: RepositoryContext) extends ListRepository(ListBuffer())(ec)
