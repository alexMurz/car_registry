package repo

import javax.inject.Inject
import play.api.Logger

import scala.collection.mutable.ListBuffer
import scala.concurrent.Future

/// List Repository
//@Singleton
class ListRepository(list: ListBuffer[DataPoint])(implicit val ec: RepositoryContext)
  extends DataRepository
{

  implicit class LocalFilter(filter: DataFilter) {
    def test(point: DataPoint): Boolean = {
      filter.col match {
        case DataColumnId =>
          // Transmute Bounds[String] to Bounds[Int]
          filter.bounds.transmute(_.toIntOption).exists { bounds =>
            bounds.isInside(point.id.id)
          }
        case DataColumnNum => filter.bounds.isInside(point.num)
        case DataColumnMark => filter.bounds.isInside(point.mark)
        case DataColumnColor => filter.bounds.isInside(point.color)
        case DataColumnDate => true // TODO
      }
    }
  }

  override def getAll(filters: Seq[DataFilter]): Future[Iterable[DataPoint]] = Future {
    list.filter { dataPoint =>
      filters.forall { _.test(dataPoint) }
    }
  }

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

