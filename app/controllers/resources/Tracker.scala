package controllers.resources

import play.api.libs.json._
import repo.{QResult, QSuccess}

import scala.collection.mutable.ListBuffer
import scala.concurrent.{ExecutionContext, Future}

trait StatTracker {
  def stats: Future[QResult]
}

// Single function timing track
private class TimingTrack
(
// Track name
  val title: String,
// Amount of data points we will preserve
  val count: Int = 10,
) {

  // List of last `count` timings
  private val times = ListBuffer.fill(count)(0L)
  // Precalculated total
  private var total = 0L
  // Current cursor position
  private var cursor = 0
  // Usage counter. It is the same before buffer starts wrapping
  private var usageCounter = 0
  // Checks as true if buffer was filled, and cursor wraps around at this point, hence we can consider all elements in buffer
  // And no only ones up to cursor
  private var wrapping = false

  // Return iterator over actualized numbers in buffer
  def iter: List[Long] = {
    if (wrapping) times.toList
    else times.take(cursor).toList
  }

  // Add timing entry
  def add(v: Long): Unit = this.synchronized {
    val curr = cursor
    cursor = (cursor + 1) % count
    if (cursor == 0) wrapping = true
    total += v - times(curr)
    times(curr) = v
  }

  def measure[T](v: => Future[T])(implicit ec: ExecutionContext): Future[T] = {
    val t0 = System.nanoTime()
    usageCounter += 1
    v.map { res =>
      add(System.nanoTime() - t0)
      res
    }
  }

  def apply[T](v: => Future[T])(implicit ec: ExecutionContext): Future[T] = measure(v)

  def json: JsValue = {
    val list = iter
    val average = if (list.isEmpty) 0.0
    else (total / list.size).doubleValue / 1e6

    val min = if (list.isEmpty) 0.0 else list.min / 1e6
    val max = if (list.isEmpty) 0.0 else list.max / 1e6

    JsObject(Seq(
      "name" -> JsString(title),
      "ty" -> JsString("timer"),
      "average_ms" -> JsNumber(average),
      "min_ms" -> JsNumber(min),
      "max_ms" -> JsNumber(max),
      "usage" -> JsNumber(usageCounter)
    ))
  }
}

class TrackedResourceHandler(base: DataResourceHandler)(implicit ec: ExecutionContext) extends ResourceHandler with StatTracker {

  private val getAllTimer = new TimingTrack("getAll")
  override def getAll: Future[QResult] = getAllTimer(base.getAll)

  private val lookupTimer = new TimingTrack("lookup")
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
