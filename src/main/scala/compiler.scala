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

abstract class Stage[-A, +B, +C] extends CellPair:
  type A1 >: A
  type B2 <: B
  type C2 <: C

object Stage:
  def apply[Al, Bl, Cl, Ar, Br, Cr](l: Cell[Al, Bl, Cl], r: Cell[Ar, Br, Cr]) = 
    new Stage[Al, Br, Cr]:
      type A1 = Al
      type B1 = Bl
      type C1 = Cl
      type A2 = Ar
      type B2 = Br
      type C2 = Cr
      val left = l
      val right = r

  def apply[A, B, C](c: Cell[A, B, C]): Stage[A, B, C] = apply(c, c)
  def apply[A, B, C](m: Machine[A, B, C]): Stage[A, B, C] = apply(Cell(m))
  val empty: Stage[Nothing, Nothing, Any] = apply(Stop(()))
  val top: Stage[Nothing, Any, Any] = apply(Stop(()))
  val bottom: Stage[Any, Nothing, Nothing] = apply(Error(new NotImplementedError))

def compile(system: Process[Nothing, Nothing, Any]): Iterable[Synapse] =

  val syns = mutable.Buffer[Synapse]()
  type Unistage[A] = Stage[A, A, Any]
  val topics = HMap[Tag, Unistage]

  def stage[A, B, C](p: Process[A, B, C]): Stage[A, B, C] =
    p match 

      case Run(m) => Stage(m)

      case Pipe(p1, p2) => 
        val (s1, s2) = (stage(p1), stage(p2))
        syns += CommonSynapse(s1.right, s2.left)
        Stage(s1.left, s2.right)

      case Balance(ps:_*) =>
        def recover[A](ps: Iterable[Consumer[A]]): Stage[A, Nothing, Any] =
          val b = Stage(buffer[A])
          for p <- ps do  
            val c = stage(p)
            syns += CommonSynapse(b.right, c.left)
          Stage(b.left, Stage.empty.right)
        recover(ps)
        
      case Concentrate(ps:_*) =>
        def recover[B](ps: Iterable[Producer[B]]): Stage[Nothing, B, Any] =
          val b = Stage(buffer[B])
          for p <- ps do  
            val c = stage(p)
            syns += CommonSynapse(c.right, b.left)
          Stage(Stage.empty.left, b.right)
        recover(ps)

      case Ref(t) => 
        def recover[A](t: Tag[A]): Stage[A, Nothing, Any] =
          val b = topics.getOrAdd(t, Stage(buffer[A]))
          Stage(b.left, Stage.empty.right)
        recover(t)

      case Deref(t) => 
        def recover[B](t: Tag[B]): Stage[Nothing, B, Any] =
          val b = topics.getOrAdd(t, Stage(buffer[B]))
          Stage(Stage.empty.left, b.right)
        recover(t)

      case Input(r) => Stage.bottom
      case Output(r) => Stage.bottom
      case Reduce(_) => Stage.bottom
      case Monitor(_, _) => Stage.bottom
      case Repeat(_) => Stage.bottom
      case Concat(_:_*) => Stage.bottom
      case Broadcast(_, _:_*) => Stage.bottom
      case System(_:_*) => Stage.bottom

  stage(system)
  syns.toIterable