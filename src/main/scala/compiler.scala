package machines

import scala.collection.mutable
import Machine._
import Process._

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

case class Stage[-A, +B](left: Cell[A, Any], right: Cell[Nothing, B])
object Stage:
  def apply[A, B](c: Cell[A, B]): Stage[A, B] = apply(c, c)
  def apply[A, B](m: Machine[A, B, Any]): Stage[A, B] = apply(MachineCell(m, 0))
  val empty: Stage[Nothing, Nothing] = apply(Stop(()))

def compile(system: Process[Nothing, Nothing, Any]): Iterable[Synapse] =

  type UniStage[A] = Stage[A, A]

  val syns = mutable.Buffer[Synapse]()
  val topics = HMap[Tag, UniStage]

  def stage[A, B, C](p: Process[A, B, Any]): Stage[A, B] =
    p match 

      case Run(m) => Stage(m)

      case Pipe(p1, p2) => 
        val (s1, s2) = (stage(p1), stage(p2))
        syns += Synapse(s1.right, s2.left)
        Stage(s1.left, s2.right)

      case Balance(ps:_*) =>
        def recover[A](ps: Iterable[Consumer[A]]): Stage[A, Nothing] =
          val b = Stage(buffer[A])
          for p <- ps do  
            val c = stage(p)
            syns += Synapse(b.right, c.left)
          Stage(b.left, Stage.empty.right)
        recover(ps)
        
      case Concentrate(ps:_*) =>
        def recover[B](ps: Iterable[Producer[B]]): Stage[Nothing, B] =
          val b = Stage(buffer[B])
          for p <- ps do  
            val c = stage(p)
            syns += Synapse(c.right, b.left)
          Stage(Stage.empty.left, b.right)
        recover(ps)

      case Ref(t) => 
        def recover[A](t: Tag[A]): Stage[A, Nothing] =
          val b = topics.getOrAdd(t, Stage(buffer[A]))
          Stage(b.left, Stage.empty.right)
        recover(t)

      case Deref(t) => 
        def recover[A](t: Tag[A]): Stage[Nothing, A] =
          val b = topics.getOrAdd(t, Stage(buffer[A]))
          Stage(Stage.empty.left, b.right)
        recover(t)
          

  stage(system)
  syns.toArray