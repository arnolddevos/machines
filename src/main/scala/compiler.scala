package machines

import scala.collection.mutable
import Machine._
import Process._

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
  type Buffer[A] = Cell[A, A, Any]
  val topics = HMap[Tag, Buffer]

  def stage[A, B, C](p: Process[A, B, C]): Stage[A, B, C] =
    p match 

      case Run(m) => Stage(m)

      case Pipe(p1, p2) => 
        val (s1, s2) = (stage(p1), stage(p2))
        syns += CommonSynapse(s1.right, s2.left)
        Stage(s1.left, s2.right)

      case Balance(ps:_*) =>
        val b = Cell(buffer[A])
        for p <- ps do  
          val c = stage(p)
          syns += CommonSynapse(b, c.left)
        Stage(b, Stage.empty.right)
        
      case Concentrate(ps:_*) =>
        val b = Cell(buffer[B])
        for p <- ps do  
          val c = stage(p)
          syns += CommonSynapse(c.right, b)
        Stage(Stage.empty.left, b)

      case Ref(t) => 
        val b = topics.getOrAdd(t, Cell(buffer[A]))
        Stage(b, Stage.empty.right)

      case Deref(t) => 
        val b = topics.getOrAdd(t, Cell(buffer[B]))
        Stage(Stage.empty.left, b)

      case Monitor(p1, p2) => 
        val (s1, s2) = (stage(p1), stage(p2))
        syns += OneShotSynapse(s1.right, s2.left)
        Stage(s1.left, s1.right)

      case Reduce(p) => 
        val b = Cell(buffer[B])
        val s = stage(p)
        syns += OneShotSynapse(s.right, b)
        Stage(s.left, b)

      case Input(e) => 
        val b = Cell(buffer[B])
        syns += Sensor(e, b)
        Stage(Stage.empty.left, b)

      case Output(e) => 
        val b = Cell(buffer[A])
        syns += Motor(e, b)
        Stage(b, Stage.empty.right)

      case Broadcast(backlog, ps:_*) => 
        val b = Cell(buffer[A])
        val cs = 
          for p <- ps 
          yield 
            val s = stage(p)
            val c = Cell(if backlog > 1 then queue[A](backlog) else buffer[A])
            syns += CommonSynapse(c, s.left)
            c
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

def compileAndRun(system: Process[Nothing, Nothing, Any]): Unit =
  val syns = compile(system)

  def cycle: Unit =
    var changes = false
    for s <- syns do changes ||= s.fire
    if changes then cycle

  cycle