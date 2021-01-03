package machines

case class System(cs: List[Connection])

class Topic[A]
trait Input[+A]
trait Output[-A]

enum Connection:
  case MachineConnect[A, B, C, D](m: Machine[A, B, C, D], a: Topic[A], b: Topic[B], c: Topic[C], d: Topic[D])
  case InputConnect[A](i: Input[A], a: Topic[A])
  case OutputConnect[A](o: Output[A], a: Topic[A])
