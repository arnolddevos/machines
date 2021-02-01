package machines

import scala.collection.mutable

class MultiMap[K[_], V[_]](underlying: mutable.Map[Any, mutable.Buffer[Any]]):
  def get[A](k: K[A]): Iterable[V[A]] =
    underlying.get(k).fold(Iterable.empty[V[A]])(_.asInstanceOf[Iterable[V[A]]])

class MultiMapBuilder[K[_], V[_]]:
  val underlying = mutable.Map[Any, mutable.Buffer[Any]]()

  def add[A](k: K[A], v: V[A]): Unit =
    if underlying.contains(k) then underlying(k).append(v)
    else underlying(k) = mutable.Buffer(v)

  def result = MultiMap[K, V](underlying)

class HMap[K[_], V[_]]:
  val underlying = mutable.Map[Any, Any]()

  def get[A](k: K[A]): Option[V[A]] =
    underlying.get(k).map(_.asInstanceOf[V[A]])

  def getOrAdd[A](k: K[A], v: => V[A]): V[A] =
    if underlying.contains(k) then underlying(k).asInstanceOf[V[A]]
    else 
      val v0 = v
      underlying(k) = v0
      v0
