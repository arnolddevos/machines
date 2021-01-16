package machines

case class System(cs: Process*)

enum Process:
  case Stage[A, B, C](m: Machine[A, B, C], a: Topic[A], b: Topic[B], c: Topic[C])
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
