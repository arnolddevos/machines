package machines

enum Machine[-A, +B, +C]:
  case React[A, B, C, S]( state: S, accept: (S, A) => Machine[A, B, C], close: Machine[A, B, C]) extends Machine[A, B, C]
  case Defer[A, B, C, S]( state: S, advance: S => Machine[A, B, C]) extends Machine[A, B, C]
  case Emit( b: B, continue: Machine[A, B, C])
  case Branch( react: Machine[A, B, C], emit: Machine[A, B, C])
  case Stop(c: C)
  case Error(e: Throwable)

  def mapEmit[D](f: B => D): Machine[A, D, C] =
    this match 
      case React(s, g, c) => React(s, (s, a) => g(s, a).mapEmit(f), c.mapEmit(f))
      case Defer(s, g) => Defer(s, s => g(s).mapEmit(f))
      case Emit(b, c) => Emit(f(b), c.mapEmit(f))
      case Branch(l, r) => Branch(l.mapEmit(f), r.mapEmit(f))
      case Stop(c) => Stop(c)
      case Error(t) => Error(t)

  def mapAccept[D](f: D => A): Machine[D, B, C] =
    this match 
      case React(s, g, c) => React(s, (s, d) => g(s, f(d)).mapAccept(f), c.mapAccept(f))
      case Defer(s, g) => Defer(s, s => g(s).mapAccept(f))
      case Emit(b, c) => Emit(b, c.mapAccept(f))
      case Branch(l, r) => Branch(l.mapAccept(f), r.mapAccept(f))
      case Stop(c) => Stop(c)
      case Error(t) => Error(t)

  def map[D](f: C => D): Machine[A, B, D] =
    this match 
      case React(s, g, c) => React(s, (s, a) => g(s, a).map(f), c.map(f))
      case Defer(s, g) => Defer(s, s => g(s).map(f))
      case Emit(b, c) => Emit(b, c.map(f))
      case Branch(l, r) => Branch(l.map(f), r.map(f))
      case Stop(c) => Stop(f(c))
      case Error(t) => Error(t)

  def flatMap[A1 <: A, B1 >: B, D](f: C => Machine[A1, B1, D]): Machine[A1, B1, D] = 
    this match 
      case React(s, g, c) => React(s, (s, a) => g(s, a).flatMap(f), c.flatMap(f))
      case Defer(s, g) => Defer(s, s => g(s).flatMap(f))
      case Emit(b, c) => Emit(b, c.flatMap(f))
      case Branch(l, r) => Branch(l.flatMap(f), r.flatMap(f))
      case Stop(c) => f(c)
      case Error(t) => Error(t)

  def fold[D](f: B => D, h: C => D): Machine[A, D, Any] =
    this match
      case React(s, g, c) => React(s, (s, a) => g(s, a).fold(f, h), c.fold(f, h))
      case Defer(s, g) => Defer(s, s => g(s).fold(f, h))
      case Emit(b, c) => Emit(f(b), c.fold(f, h))
      case Branch(l, r) => Branch(l.fold(f, h), r.fold(f, h))
      case Stop(c) => Emit(h(c), Stop(()))
      case Error(t) => Error(t)

  @annotation.tailrec
  final def seekBranch: Machine[A, B, C] =
    this match 
      case Defer(s, f) => f(s).seekBranch
      case m => m

  final def seekReact: Machine[A, B, C] =
    this match 
      case Defer(s, f)  => f(s).seekReact
      case Branch(r, e) => 
        r.seekReact match
          case m @ React(_, _, _) => m
          case _ => e.seekReact
      case m => m

  final def seekEmit: Machine[A, B, C] =
    this match 
      case Defer(s, f)  => f(s).seekEmit
      case Branch(r, e) => 
        e.seekEmit match
          case m @ Emit(_, _) => m
          case _ => e.seekEmit
      case m => m

  final def seekEmitStop: Machine[A, B, C] =
    this match 
      case Defer(s, f)  => f(s).seekEmitStop
      case Branch(r, e) => 
        e.seekEmitStop match
          case m @ (Emit(_, _) | Stop(_)) => m
          case _ => e.seekEmitStop
      case m => m
