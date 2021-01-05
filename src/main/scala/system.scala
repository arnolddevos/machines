package machines

case class System(cs: Node*)

enum Node:
  case Process[A, B, C, D](m: Machine[A, B, C, D], a: Topic[A], b: Topic[B], c: Topic[C], d: Topic[D])
  case Input[A](a: Topic[A])
  case Output[A](a: Topic[A])
  case Subsystem(s: System)

sealed trait Topic[A]
class Broadcast[A] extends Topic[A]
class Distribute[A] extends Topic[A]
class Latch[A] extends Topic[A]
class Discard[A] extends Topic[A]

val panic   = Latch[Throwable]
val unused  = Discard[Nothing]
val discard = Discard[Any]
