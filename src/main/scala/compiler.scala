package machines

import scala.collection.mutable
import Machine._
import Process._

abstract class Stage[-A, +B, +D] extends CellPair:
  type A1 >: A
  type B2 <: B
  type D2 <: D

object Stage:
  def apply[Al, Bl, Dl, Ar, Br, Dr](l: Cell[Al, Bl, Dl], r: Cell[Ar, Br, Dr]) = 
    new Stage[Al, Br, Dr]:
      type A1 = Al
      type B1 = Bl
      type D1 = Dl
      type A2 = Ar
      type B2 = Br
      type D2 = Dr
      val left = l
      val right = r

  def apply[A, B, D](d: Cell[A, B, D]): Stage[A, B, D] = apply(d, d)
  def apply[A, B, D](m: Machine[A, B, Pure, D]): Stage[A, B, D] = apply(Cell(m, Pure))
  val empty: Stage[Nothing, Nothing, Any] = apply(Return(()))
  val top: Stage[Nothing, Any, Any] = apply(Return(()))
  val bottom: Stage[Any, Nothing, Nothing] = apply(Error(new NotImplementedError))

def compile[C <: Pure](system: Process[Nothing, Nothing, C, Any], c: C): Iterable[Synapse] =

  val syns = mutable.Buffer[Synapse]()
  type Buffer[A] = Cell[A, A, Any]
  val topics = HMap[Tag, Buffer]
  def bufferCell[A] = Cell(buffer[A], Pure)

  def stage[A, B, D](p: Process[A, B, C, D]): Stage[A, B, D] =
    p match 

      case Run(m) => Stage(Cell(m, c))

      case Pipe(p1, p2) => 
        val (s1, s2) = (stage(p1), stage(p2))
        syns += CommonSynapse(s1.right, s2.left)
        Stage(s1.left, s2.right)

      case Balance(ps:_*) =>
        val b = bufferCell[A]
        for p <- ps do  
          val d = stage(p)
          syns += CommonSynapse(b, d.left)
        Stage(b, Stage.empty.right)
        
      case Concentrate(ps:_*) =>
        val b = bufferCell[B]
        for p <- ps do  
          val d = stage(p)
          syns += CommonSynapse(d.right, b)
        Stage(Stage.empty.left, b)

      case Ref(t) => 
        val b = topics.getOrAdd(t, bufferCell[A])
        Stage(b, Stage.empty.right)

      case Deref(t) => 
        val b = topics.getOrAdd(t, bufferCell[B])
        Stage(Stage.empty.left, b)

      case Monitor(p1, p2) => 
        val (s1, s2) = (stage(p1), stage(p2))
        syns += OneShotSynapse(s1.right, s2.left)
        Stage(s1.left, s1.right)

      case Reduce(p) => 
        val b = bufferCell[B]
        val s = stage(p)
        syns += OneShotSynapse(s.right, b)
        Stage(s.left, b)

      case Broadcast(backlog, ps:_*) => 
        val b = bufferCell[A]
        val cs = 
          for p <- ps 
          yield 
            val s = stage(p)
            val d = if backlog > 1 then Cell(queue[A](backlog), Pure) else bufferCell[A]
            syns += CommonSynapse(d, s.left)
            d
        syns += BroadcastSynapse(b, cs)
        Stage(b, Stage.empty.right)

      case Repeat(_) => Stage.bottom
      case Concat(_:_*) => Stage.bottom

      case System(ps:_*) => 
        for p <- ps 
        do stage(p)
        Stage.empty

  stage(system)
  syns.toIterable

def compileAndRun[C<: Pure](system: Process[Nothing, Nothing, C, Any], c: C): Unit =
  val syns = compile(system, c)

  def cycle: Unit =
    var changes = false
    for s <- syns do changes ||= s.fire
    if changes then cycle

  cycle