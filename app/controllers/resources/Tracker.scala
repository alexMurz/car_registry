package controllers.resources

import play.api.libs.json._
import repo.{QResult, QSuccess}

import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}

// Single function timing track
private class TimingTrack
(
// Track name
  val title: String,
// Amount of data point we will preserve
  val count: Int = 100,
) {

  // List of last N timing
  private val times = ListBuffer.fill(count)(0L)
  // Precalculated total
  private var total = 0L
  // Current cursor position
  private var cursor = 0
  // Checks as true if buffer was filled, and cursor wraps around at this point, hence we can consider all elements in buffer
  // And no only ones up to cursor
  private var wrapping = false

  // Return iterator over actualized numbers in buffer
  def iter: List[Long] = {
    if (wrapping) times.toList
    else times.take(cursor).toList
  }

  // Add item
  def add(v: Long): Unit = {
    val curr = cursor
    cursor = (cursor + 1) % count
    if (cursor == 0) wrapping = true
    total += v - times(curr)
    times(curr) = v
  }

  def measure[T](v: => Future[T])(implicit ec: ExecutionContext): Future[T] = {
    val t0 = System.nanoTime()
    v.map { res =>
      add(System.nanoTime() - t0)
      res
    }
  }

  def apply[T](v: => Future[T])(implicit ec: ExecutionContext): Future[T] = measure(v)

  def json: JsValue = {
    val itemCount = if (wrapping) count else cursor
    val average = if (itemCount == 0) 0.0
    else (total / itemCount).doubleValue / 1e6

    val list = iter
    val min = if (list.isEmpty) 0.0 else iter.min / 1e6
    val max = if (list.isEmpty) 0.0 else iter.max / 1e6

    JsObject(Seq(
      "name" -> JsString(title),
      "ty" -> JsString("timer"),
      "average_ms" -> JsNumber(average),
      "min_ms" -> JsNumber(min),
      "max_ms" -> JsNumber(max)
    ))
  }
}

// Index insertion, removal track
private class IndexTrack[T]
(
// Track name
  val title: String
) {

}

class TrackedResourceHandler(base: DataResourceHandler)(implicit ec: ExecutionContext) extends ResourceHandler with StatTracker {

  private val getAllTimer = new TimingTrack("getAll")
  override def getAll: Future[QResult] = getAllTimer(base.getAll)

  private val lookupTimer = new TimingTrack("getAll")
  override def lookup(id: String): Future[QResult] = lookupTimer(base.lookup(id))

  private val insertTimer = new TimingTrack("insert")
  override def insert(body: Option[JsValue]): Future[QResult] = insertTimer(base.insert(body))

  private val removeTimer = new TimingTrack("remove")
  override def remove(id: String): Future[QResult] = removeTimer(base.remove(id))

  override def stats: Future[QResult] = Future {
    QSuccess(Seq(
      getAllTimer.json, lookupTimer.json, insertTimer.json, removeTimer.json
    ))
  }
}
