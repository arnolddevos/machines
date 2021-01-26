package machines

enum Machine[-A, +B, +C]:
  case React[A, B, C, S]( state: S, accept: (S, A) => Machine[A, B, C], close: Machine[Nothing, B, C]) extends Machine[A, B, C]
  case Defer[A, B, C, S]( state: S, advance: S => Machine[A, B, C]) extends Machine[A, B, C]
  case Emit( b: B, next: Machine[A, B, C])
  case Branch( react: Machine[A, B, C], emit: Machine[A, B, C])
  case Stop(c: C)
  case Error(e: Throwable)
