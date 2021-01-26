package machines

import scala.collection.mutable

class MultiMap[K[_], V[_]](underlying: mutable.Map[Any, mutable.Buffer[Any]]):
  def get[A](k: K[A]): Iterable[V[A]] =
    underlying.get(k).fold(Iterable.empty[V[A]])(_.asInstanceOf[Iterable[V[A]]])

class MapBuilder[K[_], V[_]]:
  val underlying = mutable.Map[Any, mutable.Buffer[Any]]()

  def add[A](k: K[A], v: V[A]): Unit =
    if underlying.contains(k) then underlying(k).append(v)
    else underlying(k) = mutable.Buffer(v)

  def result = MultiMap[K, V](underlying)
