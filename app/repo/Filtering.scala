package repo

/**
 * Search bounds edges
 * Open Edge - No limit
 * Inclusive - Including value
 * Exclusive - Excluding value
 * example:
 *    [0 .. 10) - From 0 inclusive to 10 exclusive
 *    Bounds(InclusiveEdge(0), ExclusiveEdge(10))
 *    [0 .. inf) - From 0 and beyond
 *    Bounds(InclusiveEdge(0), OpenEdge())
 */
sealed trait Edge[T] {
  // Test as if edge is lower
  def asLo(other: T): Boolean
  // Test as if edge is higher
  def asHi(other: T): Boolean
}
case class OpenEdge[T]() extends Edge[T] {
  override def asLo(other: T): Boolean = true
  override def asHi(other: T): Boolean = true
}
case class InclusiveEdge[T](v: T)(implicit ord: Ordering[T]) extends Edge[T] {
  override def asLo(other: T): Boolean = ord.lteq(v, other)
  override def asHi(other: T): Boolean = ord.gteq(v, other)
}
case class ExclusiveEdge[T](v: T)(implicit ord: Ordering[T]) extends Edge[T] {
  override def asLo(other: T): Boolean = ord.lt(v, other)
  override def asHi(other: T): Boolean = ord.gt(v, other)
}


/**
 * Bounds (pair of bound edges)
 */
case class Bounds[T](lo: Edge[T], hi: Edge[T]) {
  // Transmute Bounds[T] to Bounds[R] Using map: T => Option[R] return None if any of map's return None
  def transmute[R](map: T => Option[R])(implicit ord: Ordering[R]): Option[Bounds[R]] =  {
    val new_lo: Edge[R] = lo match {
      case OpenEdge() => OpenEdge()
      case InclusiveEdge(v) => map(v) match {
        case Some(v) => InclusiveEdge(v)
        case None => return None
      }
      case ExclusiveEdge(v) => map(v) match {
        case Some(v) => ExclusiveEdge(v)
        case None => return None
      }
    }
    val new_hi: Edge[R] = hi match {
      case OpenEdge() => OpenEdge()
      case InclusiveEdge(v) => map(v) match {
        case Some(v) => InclusiveEdge(v)
        case None => return None
      }
      case ExclusiveEdge(v) => map(v) match {
        case Some(v) => ExclusiveEdge(v)
        case None => return None
      }
    }
    Some(Bounds(new_lo, new_hi))
  }

  def isInside(other: T): Boolean = lo.asLo(other) && hi.asHi(other)
}


object Bounds {
  /**
   *
   * Try make bounds from Seq
   * Seq() => Open bounds
   * Seq(a) => Inclusive(a) to Inclusive(a)
   * Seq(a, "..") => From a Inclusive
   * Seq("..", b) => To b exclusive
   * Seq(a, "..", b) => Inclusive(a) to Exclusive(b)
   * Seq(a, "..=", b) => Inclusive(a) to Inclusive(b)
   */
  def fromSequence(data: Seq[String]): Option[Bounds[String]] = {
    data match {
      case Seq("..") || Seq() => Some(Bounds(OpenEdge(), OpenEdge()))
      case Seq(a) => Some(Bounds(InclusiveEdge(a), InclusiveEdge(a)))

      case Seq(a, "..") => Some(Bounds(InclusiveEdge(a), OpenEdge()))
      case Seq("..", b) => Some(Bounds(OpenEdge(), ExclusiveEdge(b)))
      case Seq("..=", b, _@_*) => Some(Bounds(OpenEdge(), InclusiveEdge(b)))

      case Seq(a, "..", b, _@_*) => Some(Bounds(InclusiveEdge(a), ExclusiveEdge(b)))
      case Seq(a, "..=", b, _@_*) => Some(Bounds(InclusiveEdge(a), InclusiveEdge(b)))

      case _ => None
    }
  }

}
